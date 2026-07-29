const { spawn } = require("node:child_process");
const crypto = require("node:crypto");
const fsp = require("node:fs/promises");

const {
  FINISHED_RUN_RETENTION_MS,
  INTERACTIVE_RUN_TIMEOUT_MS,
  MAX_INTERACTIVE_RUN_SESSIONS,
  MAX_STDIN_CHARS,
} = require("./config");
const { buildProgram, compilationFailureResult, linkAssembly } = require("./compiler");
const { appendOutputBuffer } = require("./process-runner");

class InteractiveRunSession {
  constructor({ id, child, tempDir, compiler, assembly, architecture, optimise, onFinished }) {
    this.id = id;
    this.child = child;
    this.tempDir = tempDir;
    this.compiler = compiler;
    this.assembly = assembly;
    this.architecture = architecture;
    this.optimise = optimise;
    this.onFinished = onFinished;
    this.startedAt = Date.now();
    this.closedAt = null;
    this.exitCode = null;
    this.stdout = Buffer.alloc(0);
    this.stderr = Buffer.alloc(0);
    this.outputExceeded = false;
    this.timedOut = false;
    this.stopped = false;
    this.spawnError = null;
    this.inputChars = 0;
    this.cleanupTimer = null;
    this.killTimer = null;

    this.timeout = setTimeout(() => {
      this.timedOut = true;
      this.child.kill("SIGKILL");
    }, INTERACTIVE_RUN_TIMEOUT_MS);
    this.timeout.unref?.();

    child.stdout.on("data", (chunk) => this.appendOutput("stdout", chunk));
    child.stderr.on("data", (chunk) => this.appendOutput("stderr", chunk));
    child.on("error", (error) => {
      this.spawnError = error.message;
      this.finish(null);
    });
    child.on("close", (code) => this.finish(code));
  }

  appendOutput(stream, chunk) {
    const appended = appendOutputBuffer(this[stream], chunk);
    this[stream] = appended.buffer;
    this.outputExceeded ||= appended.exceeded;
  }

  finish(exitCode) {
    if (this.closedAt) return;
    this.closedAt = Date.now();
    this.exitCode = exitCode;
    clearTimeout(this.timeout);
    clearTimeout(this.killTimer);
    try {
      if (!this.child.stdin.destroyed) this.child.stdin.destroy();
    } catch {
      // The child process may already have closed stdin.
    }
    this.onFinished?.(this);
  }

  write(input, appendNewline) {
    if (this.closedAt) {
      throw Object.assign(new Error("Program has already finished"), { statusCode: 409 });
    }

    const text = appendNewline && !input.endsWith("\n") ? `${input}\n` : input;
    if (this.inputChars + text.length > MAX_STDIN_CHARS) {
      throw Object.assign(
        new Error(`Program input cannot exceed ${MAX_STDIN_CHARS} characters per run`),
        { statusCode: 400 },
      );
    }
    this.inputChars += text.length;

    return new Promise((resolve, reject) => {
      this.child.stdin.write(text, "utf8", (error) => {
        if (!error) {
          resolve();
          return;
        }
        reject(Object.assign(new Error("Could not send input to the running program"), {
          statusCode: this.closedAt ? 409 : 500,
        }));
      });
    });
  }

  stop() {
    this.stopped = true;
    if (this.closedAt) return;
    this.child.kill("SIGTERM");
    this.killTimer = setTimeout(() => {
      if (!this.closedAt) this.child.kill("SIGKILL");
    }, 1_000);
    this.killTimer.unref?.();
  }

  snapshot() {
    const finished = Boolean(this.closedAt);
    return {
      available: true,
      stage: "run",
      running: !finished,
      stopped: this.stopped,
      exitCode: finished ? this.exitCode : null,
      stdout: this.stdout.toString("utf8"),
      stderr: this.stderr.toString("utf8"),
      durationMs: (this.closedAt || Date.now()) - this.startedAt,
      timedOut: this.timedOut,
      outputExceeded: this.outputExceeded,
      spawnError: this.spawnError,
    };
  }

  async cleanup() {
    clearTimeout(this.timeout);
    clearTimeout(this.killTimer);
    clearTimeout(this.cleanupTimer);
    if (!this.closedAt) this.stop();
    await fsp.rm(this.tempDir, { recursive: true, force: true });
  }
}

class InteractiveRunStore {
  constructor(options = {}) {
    this.maxSessions = options.maxSessions || MAX_INTERACTIVE_RUN_SESSIONS;
    this.finishedRetentionMs = options.finishedRetentionMs || FINISHED_RUN_RETENTION_MS;
    this.sessions = new Map();
  }

  get status() {
    let running = 0;
    for (const session of this.sessions.values()) {
      if (!session.closedAt) running += 1;
    }
    return {
      running,
      retained: this.sessions.size - running,
      maxSessions: this.maxSessions,
    };
  }

  responseFor(session) {
    const execution = session.snapshot();
    const finishedOk = !execution.running
      && execution.exitCode === 0
      && !execution.timedOut
      && !execution.spawnError
      && !execution.stopped;
    return {
      ok: execution.running || finishedOk,
      phase: execution.running ? "running" : "complete",
      message: execution.running
        ? "Program is running"
        : execution.stopped
          ? "Program stopped"
          : finishedOk
            ? "Program finished"
            : execution.timedOut
              ? "Program timed out"
              : "Program failed",
      sessionId: session.id,
      architecture: session.architecture,
      optimise: session.optimise,
      compiler: session.compiler,
      assembly: session.assembly,
      execution,
    };
  }

  get(id) {
    const session = this.sessions.get(id);
    if (!session) {
      throw Object.assign(new Error("Run session not found"), { statusCode: 404 });
    }
    return session;
  }

  scheduleCleanup(session) {
    clearTimeout(session.cleanupTimer);
    session.cleanupTimer = setTimeout(() => {
      this.delete(session.id).catch((error) => console.error(error));
    }, this.finishedRetentionMs);
    session.cleanupTimer.unref?.();
  }

  async delete(id) {
    const session = this.sessions.get(id);
    if (!session) return;
    this.sessions.delete(id);
    await session.cleanup();
  }

  async closeAll() {
    await Promise.all(Array.from(this.sessions.keys(), (id) => this.delete(id)));
  }

  async start(request, options = {}) {
    for (const session of this.sessions.values()) {
      if (session.closedAt && Date.now() - session.closedAt >= this.finishedRetentionMs) {
        await this.delete(session.id);
      }
    }
    if (this.sessions.size >= this.maxSessions) {
      throw Object.assign(new Error("Too many interactive programs are running. Stop one and try again."), {
        statusCode: 503,
      });
    }

    const build = await buildProgram(request, options);
    let session = null;
    try {
      const failure = compilationFailureResult(build);
      if (failure) {
        await fsp.rm(build.tempDir, { recursive: true, force: true });
        return failure;
      }

      const linked = await linkAssembly(build.tempDir, request.architecture);
      if (!linked.ok) {
        await fsp.rm(build.tempDir, { recursive: true, force: true });
        return {
          ok: false,
          phase: "run-environment",
          message: linked.execution.stderr,
          architecture: request.architecture,
          optimise: request.optimise,
          compiler: build.compiler,
          assembly: build.assembly,
          execution: linked.execution,
        };
      }

      const command = linked.config.emulator || linked.binaryPath;
      const args = [];
      if (linked.config.emulator) {
        if (linked.tools.sysroot) args.push("-L", linked.tools.sysroot);
        args.push(linked.binaryPath);
      }

      const id = crypto.randomUUID();
      const child = spawn(command, args, {
        cwd: build.tempDir,
        env: process.env,
        stdio: ["pipe", "pipe", "pipe"],
      });
      session = new InteractiveRunSession({
        id,
        child,
        tempDir: build.tempDir,
        compiler: build.compiler,
        assembly: build.assembly,
        architecture: request.architecture,
        optimise: request.optimise,
        onFinished: (finishedSession) => this.scheduleCleanup(finishedSession),
      });
      this.sessions.set(id, session);

      if (request.stdin) {
        try {
          await session.write(request.stdin, true);
        } catch (error) {
          if (!session.closedAt) throw error;
        }
      }
      return this.responseFor(session);
    } catch (error) {
      if (session) await this.delete(session.id);
      else await fsp.rm(build.tempDir, { recursive: true, force: true });
      throw error;
    }
  }

  async sendInput(id, inputRequest) {
    const session = this.get(id);
    await session.write(inputRequest.input, inputRequest.appendNewline);
    return this.responseFor(session);
  }

  stop(id) {
    const session = this.get(id);
    session.stop();
    return this.responseFor(session);
  }
}

module.exports = {
  InteractiveRunSession,
  InteractiveRunStore,
};
