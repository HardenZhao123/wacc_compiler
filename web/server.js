const http = require("node:http");
const fs = require("node:fs");
const fsp = require("node:fs/promises");
const os = require("node:os");
const path = require("node:path");
const crypto = require("node:crypto");
const { spawn } = require("node:child_process");

const PROJECT_ROOT = path.resolve(__dirname, "..");
const PUBLIC_ROOT = path.join(__dirname, "public");
const MAX_BODY_BYTES = 512 * 1024;
const MAX_SOURCE_CHARS = 200_000;
const MAX_STDIN_CHARS = 64_000;
const MAX_INTERACTIVE_INPUT_CHARS = 8_000;
const MAX_PROCESS_OUTPUT_BYTES = 2 * 1024 * 1024;
const MAX_INTERACTIVE_RUN_SESSIONS = 8;
const INTERACTIVE_RUN_TIMEOUT_MS = 10 * 60_000;
const FINISHED_RUN_RETENTION_MS = 60_000;
const DEFAULT_MAX_CONCURRENT_JOBS = 2;
const DEFAULT_MAX_QUEUED_JOBS = 20;
const DEFAULT_RATE_LIMIT = 30;
const RATE_LIMIT_WINDOW_MS = 60_000;

const EXAMPLES = Object.freeze({
  hello: {
    name: "Hello world",
    file: "examples/valid/IO/print/println.wacc",
  },
  echo: {
    name: "Read and echo an integer",
    file: "examples/valid/IO/read/echoInt.wacc",
  },
  fibonacci: {
    name: "Recursive Fibonacci",
    file: "examples/valid/function/nested_functions/fibonacciRecursive.wacc",
  },
});

const MIME_TYPES = {
  ".css": "text/css; charset=utf-8",
  ".html": "text/html; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".json": "application/json; charset=utf-8",
  ".md": "text/markdown; charset=utf-8",
  ".svg": "image/svg+xml",
  ".webmanifest": "application/manifest+json; charset=utf-8",
};

function json(res, statusCode, value, omitBody = false) {
  const body = JSON.stringify(value);
  res.writeHead(statusCode, {
    "Content-Type": "application/json; charset=utf-8",
    "Content-Length": Buffer.byteLength(body),
    "Cache-Control": "no-store",
  });
  res.end(omitBody ? undefined : body);
}

function positiveInteger(value, fallback) {
  const parsed = Number.parseInt(value, 10);
  return Number.isInteger(parsed) && parsed > 0 ? parsed : fallback;
}

class JobQueue {
  constructor(maxConcurrent = DEFAULT_MAX_CONCURRENT_JOBS, maxQueued = DEFAULT_MAX_QUEUED_JOBS) {
    this.maxConcurrent = maxConcurrent;
    this.maxQueued = maxQueued;
    this.active = 0;
    this.queue = [];
  }

  get status() {
    return {
      active: this.active,
      queued: this.queue.length,
      maxConcurrent: this.maxConcurrent,
      maxQueued: this.maxQueued,
    };
  }

  run(task) {
    if (this.active < this.maxConcurrent) {
      return new Promise((resolve, reject) => this.start({ task, resolve, reject }));
    }
    if (this.queue.length >= this.maxQueued) {
      return Promise.reject(Object.assign(
        new Error("The compiler is busy. Please try again shortly."),
        { statusCode: 503 },
      ));
    }
    return new Promise((resolve, reject) => this.queue.push({ task, resolve, reject }));
  }

  start(job) {
    this.active += 1;
    Promise.resolve()
      .then(job.task)
      .then(job.resolve, job.reject)
      .finally(() => {
        this.active -= 1;
        const next = this.queue.shift();
        if (next) this.start(next);
      });
  }
}

class FixedWindowRateLimiter {
  constructor(limit = DEFAULT_RATE_LIMIT, windowMs = RATE_LIMIT_WINDOW_MS) {
    this.limit = limit;
    this.windowMs = windowMs;
    this.clients = new Map();
  }

  consume(key, now = Date.now()) {
    let entry = this.clients.get(key);
    if (!entry || now >= entry.resetAt) {
      entry = { count: 0, resetAt: now + this.windowMs };
      this.clients.set(key, entry);
    }
    entry.count += 1;

    if (this.clients.size > 1_000) {
      for (const [client, value] of this.clients) {
        if (now >= value.resetAt) this.clients.delete(client);
      }
    }

    return {
      allowed: entry.count <= this.limit,
      remaining: Math.max(0, this.limit - entry.count),
      retryAfterSeconds: Math.max(1, Math.ceil((entry.resetAt - now) / 1_000)),
    };
  }
}

function setSecurityHeaders(res) {
  res.setHeader("Content-Security-Policy", [
    "default-src 'self'",
    "base-uri 'none'",
    "connect-src 'self'",
    "font-src 'self'",
    "form-action 'self'",
    "frame-ancestors 'none'",
    "img-src 'self' data:",
    "object-src 'none'",
    "script-src 'self'",
    "style-src 'self'",
  ].join("; "));
  res.setHeader("Cross-Origin-Opener-Policy", "same-origin");
  res.setHeader("Permissions-Policy", "camera=(), microphone=(), geolocation=()");
  res.setHeader("Referrer-Policy", "no-referrer");
  res.setHeader("X-Content-Type-Options", "nosniff");
  res.setHeader("X-Frame-Options", "DENY");
}

function clientAddress(req, trustProxy) {
  if (trustProxy) {
    const forwarded = req.headers["x-forwarded-for"];
    if (typeof forwarded === "string" && forwarded.length > 0) {
      return forwarded.split(",", 1)[0].trim();
    }
  }
  return req.socket.remoteAddress || "unknown";
}

function readJsonBody(req) {
  return new Promise((resolve, reject) => {
    let size = 0;
    let failed = false;
    const chunks = [];

    req.on("data", (chunk) => {
      if (failed) return;
      size += chunk.length;
      if (size > MAX_BODY_BYTES) {
        failed = true;
        reject(Object.assign(new Error("Request body is too large"), { statusCode: 413 }));
        return;
      }
      chunks.push(chunk);
    });

    req.on("end", () => {
      if (failed) return;
      try {
        resolve(JSON.parse(Buffer.concat(chunks).toString("utf8")));
      } catch {
        reject(Object.assign(new Error("Request body must be valid JSON"), { statusCode: 400 }));
      }
    });
    req.on("error", reject);
  });
}

function validateCompileRequest(body) {
  if (!body || typeof body !== "object" || Array.isArray(body)) {
    throw Object.assign(new Error("Request body must be an object"), { statusCode: 400 });
  }

  const source = body.source;
  const architecture = body.architecture;
  const stdin = body.stdin ?? "";

  if (typeof source !== "string" || source.trim().length === 0) {
    throw Object.assign(new Error("WACC source cannot be empty"), { statusCode: 400 });
  }
  if (source.length > MAX_SOURCE_CHARS) {
    throw Object.assign(new Error(`WACC source cannot exceed ${MAX_SOURCE_CHARS} characters`), { statusCode: 400 });
  }
  if (architecture !== "aarch64" && architecture !== "arm32") {
    throw Object.assign(new Error("Architecture must be aarch64 or arm32"), { statusCode: 400 });
  }
  if (typeof stdin !== "string" || stdin.length > MAX_STDIN_CHARS) {
    throw Object.assign(new Error(`Program input cannot exceed ${MAX_STDIN_CHARS} characters`), { statusCode: 400 });
  }

  return {
    source,
    architecture,
    optimise: body.optimise !== false,
    run: body.run === true,
    stdin,
  };
}

function validateInteractiveInputRequest(body) {
  if (!body || typeof body !== "object" || Array.isArray(body)) {
    throw Object.assign(new Error("Request body must be an object"), { statusCode: 400 });
  }

  const input = body.input ?? "";
  if (typeof input !== "string" || input.length > MAX_INTERACTIVE_INPUT_CHARS) {
    throw Object.assign(
      new Error(`Program input cannot exceed ${MAX_INTERACTIVE_INPUT_CHARS} characters per send`),
      { statusCode: 400 },
    );
  }

  return {
    input,
    appendNewline: body.appendNewline !== false,
  };
}

function findExecutable(command, env = process.env) {
  if (!command) return null;
  if (command.includes(path.sep)) {
    try {
      fs.accessSync(command, fs.constants.X_OK);
      return command;
    } catch {
      return null;
    }
  }

  const pathEntries = (env.PATH || "").split(path.delimiter);
  for (const entry of pathEntries) {
    const candidate = path.join(entry, command);
    try {
      fs.accessSync(candidate, fs.constants.X_OK);
      return candidate;
    } catch {
      // Continue searching PATH.
    }
  }
  return null;
}

function compilerInvocation(options = {}) {
  if (options.compilerCommand) return options.compilerCommand;

  const configured = process.env.WACC_COMPILER;
  if (configured) return { command: configured, prefixArgs: [] };

  const nativeCompiler = path.join(PROJECT_ROOT, "wacc-compiler");
  if (findExecutable(nativeCompiler)) {
    return { command: nativeCompiler, prefixArgs: [] };
  }

  return {
    command: "scala",
    prefixArgs: ["run", PROJECT_ROOT, "--server=false", "--"],
  };
}

function appendOutputBuffer(current, chunk) {
  if (current.length >= MAX_PROCESS_OUTPUT_BYTES) {
    return { buffer: current, exceeded: true };
  }
  const remaining = MAX_PROCESS_OUTPUT_BYTES - current.length;
  return {
    buffer: Buffer.concat([current, chunk.subarray(0, remaining)]),
    exceeded: chunk.length > remaining,
  };
}

function runProcess(command, args, options = {}) {
  const startedAt = Date.now();
  return new Promise((resolve) => {
    let stdout = Buffer.alloc(0);
    let stderr = Buffer.alloc(0);
    let outputExceeded = false;
    let timedOut = false;
    let settled = false;

    const child = spawn(command, args, {
      cwd: options.cwd,
      env: options.env || process.env,
      stdio: ["pipe", "pipe", "pipe"],
    });

    child.stdout.on("data", (chunk) => {
      const appended = appendOutputBuffer(stdout, chunk);
      stdout = appended.buffer;
      outputExceeded ||= appended.exceeded;
    });
    child.stderr.on("data", (chunk) => {
      const appended = appendOutputBuffer(stderr, chunk);
      stderr = appended.buffer;
      outputExceeded ||= appended.exceeded;
    });

    const timeout = setTimeout(() => {
      timedOut = true;
      child.kill("SIGKILL");
    }, options.timeoutMs || 30_000);

    const finish = (exitCode, spawnError = null) => {
      if (settled) return;
      settled = true;
      clearTimeout(timeout);
      resolve({
        exitCode,
        stdout: stdout.toString("utf8"),
        stderr: stderr.toString("utf8"),
        durationMs: Date.now() - startedAt,
        timedOut,
        outputExceeded,
        spawnError: spawnError ? spawnError.message : null,
      });
    };

    child.on("error", (error) => finish(null, error));
    child.on("close", (code) => finish(code));

    if (options.input) child.stdin.end(options.input);
    else child.stdin.end();
  });
}

function cleanCompilerText(text, tempDir, sourcePath) {
  return text
    .split(sourcePath).join("program.wacc")
    .split(tempDir + path.sep).join("");
}

function runnerConfig(architecture) {
  if (architecture === "aarch64") {
    return {
      compiler: process.env.WACC_AARCH64_GCC || "aarch64-linux-gnu-gcc",
      compilerArgs: ["-z", "noexecstack", "-march=armv8-a"],
      emulator: process.env.WACC_AARCH64_QEMU || "qemu-aarch64",
      sysroot: process.env.WACC_AARCH64_SYSROOT || "/usr/aarch64-linux-gnu/",
    };
  }
  return {
    compiler: process.env.WACC_ARM32_GCC || "arm-linux-gnueabi-gcc",
    compilerArgs: ["-z", "noexecstack", "-march=armv6"],
    emulator: process.env.WACC_ARM32_QEMU || "qemu-arm",
    sysroot: process.env.WACC_ARM32_SYSROOT || "/usr/arm-linux-gnueabi/",
  };
}

function toolchainStatus(architecture) {
  const config = runnerConfig(architecture);
  const compiler = findExecutable(config.compiler);
  const emulator = findExecutable(config.emulator);
  const sysroot = config.sysroot && fs.existsSync(config.sysroot) ? config.sysroot : null;
  const missing = [];
  if (!compiler) missing.push(config.compiler);
  if (!emulator) missing.push(config.emulator);
  if (!sysroot) {
    missing.push(config.sysroot);
  } else {
    for (const objectFile of ["Scrt1.o", "crti.o"]) {
      const objectPath = path.join(sysroot, "lib", objectFile);
      if (!fs.existsSync(objectPath)) missing.push(objectPath);
    }
  }

  return {
    available: missing.length === 0,
    compiler,
    emulator,
    sysroot,
    missing,
  };
}

function serviceStatus(options = {}) {
  const invocation = compilerInvocation(options);
  const compilerAvailable = Boolean(findExecutable(invocation.command));
  const architectures = {
    aarch64: toolchainStatus("aarch64"),
    arm32: toolchainStatus("arm32"),
  };
  return {
    ready: compilerAvailable && architectures.aarch64.available && architectures.arm32.available,
    compiler: {
      available: compilerAvailable,
      command: path.basename(invocation.command),
    },
    architectures,
  };
}

function compilationFailureResult(build) {
  if (build.compiler.spawnError) {
    return {
      ok: false,
      phase: "compiler-start",
      message: `Could not start the compiler: ${build.compiler.spawnError}`,
      compiler: build.compiler,
      assembly: build.assembly,
      execution: null,
    };
  }
  if (build.compiler.exitCode !== 0) {
    return {
      ok: false,
      phase: "compile",
      message: build.compiler.timedOut ? "Compilation timed out" : "Compilation failed",
      compiler: build.compiler,
      assembly: build.assembly,
      execution: null,
    };
  }
  return null;
}

async function buildProgram(request, options = {}) {
  const tempDir = await fsp.mkdtemp(path.join(os.tmpdir(), "wacc-web-"));
  const sourcePath = path.join(tempDir, "program.wacc");
  const assemblyPath = path.join(tempDir, "program.s");

  try {
    await fsp.writeFile(sourcePath, request.source, "utf8");
    const invocation = compilerInvocation(options);
    const compilerArgs = [
      ...(invocation.prefixArgs || []),
      sourcePath,
      "--architecture",
      request.architecture,
      request.optimise ? "--peephole-optim" : "--no-peephole",
    ];
    const compiler = await runProcess(invocation.command, compilerArgs, {
      cwd: tempDir,
      timeoutMs: 60_000,
    });
    compiler.stdout = cleanCompilerText(compiler.stdout, tempDir, sourcePath);
    compiler.stderr = cleanCompilerText(compiler.stderr, tempDir, sourcePath);

    let assembly = "";
    try {
      assembly = await fsp.readFile(assemblyPath, "utf8");
    } catch {
      // A failed frontend compilation correctly produces no assembly file.
    }

    return {
      tempDir,
      sourcePath,
      assemblyPath,
      compiler,
      assembly,
    };
  } catch (error) {
    await fsp.rm(tempDir, { recursive: true, force: true });
    throw error;
  }
}

async function linkAssembly(tempDir, architecture) {
  const config = runnerConfig(architecture);
  const tools = toolchainStatus(architecture);
  if (!tools.available) {
    return {
      ok: false,
      execution: {
        available: false,
        stage: "run",
        exitCode: null,
        stdout: "",
        stderr: `Cannot run ${architecture}: missing ${tools.missing.join(", ")}. See README.md for setup instructions.`,
        durationMs: 0,
        timedOut: false,
      },
    };
  }

  const assemblyPath = path.join(tempDir, "program.s");
  const binaryPath = path.join(tempDir, "program.out");
  const assembleResult = await runProcess(
    config.compiler,
    ["-o", binaryPath, ...config.compilerArgs, assemblyPath],
    { cwd: tempDir, timeoutMs: 60_000 },
  );

  if (assembleResult.exitCode !== 0) {
    return {
      ok: false,
      execution: {
        available: true,
        stage: "assemble",
        ...assembleResult,
      },
    };
  }

  return {
    ok: true,
    config,
    tools,
    binaryPath,
  };
}

async function executeAssembly(tempDir, architecture, stdin) {
  const linked = await linkAssembly(tempDir, architecture);
  if (!linked.ok) return linked.execution;

  const emulatorArgs = [];
  if (linked.tools.sysroot) emulatorArgs.push("-L", linked.tools.sysroot);
  emulatorArgs.push(linked.binaryPath);
  const result = await runProcess(linked.config.emulator, emulatorArgs, {
    cwd: tempDir,
    input: stdin,
    timeoutMs: 10_000,
  });
  return { available: true, stage: "run", ...result };
}

async function compileProgram(request, options = {}) {
  const build = await buildProgram(request, options);
  try {
    const failure = compilationFailureResult(build);
    if (failure) return failure;

    const execution = request.run
      ? await executeAssembly(build.tempDir, request.architecture, request.stdin)
      : null;
    const executionSucceeded = !execution || (
      execution.available
      && execution.stage === "run"
      && execution.exitCode !== null
      && !execution.timedOut
      && !execution.spawnError
    );

    return {
      ok: executionSucceeded,
      phase: execution && !executionSucceeded ? "run-environment" : "complete",
      message: execution && !executionSucceeded ? execution.stderr : "Compilation succeeded",
      architecture: request.architecture,
      optimise: request.optimise,
      compiler: build.compiler,
      assembly: build.assembly,
      execution,
    };
  } finally {
    await fsp.rm(build.tempDir, { recursive: true, force: true });
  }
}

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

      const emulatorArgs = [];
      if (linked.tools.sysroot) emulatorArgs.push("-L", linked.tools.sysroot);
      emulatorArgs.push(linked.binaryPath);

      const id = crypto.randomUUID();
      const child = spawn(linked.config.emulator, emulatorArgs, {
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

async function serveStatic(req, res, pathname) {
  const requested = pathname === "/" ? "index.html" : pathname.slice(1);
  const filePath = path.resolve(PUBLIC_ROOT, requested);
  if (!filePath.startsWith(PUBLIC_ROOT + path.sep) && filePath !== path.join(PUBLIC_ROOT, "index.html")) {
    json(res, 403, { error: "Forbidden" });
    return;
  }

  try {
    const contents = await fsp.readFile(filePath);
    res.writeHead(200, {
      "Content-Type": MIME_TYPES[path.extname(filePath)] || "application/octet-stream",
      "Content-Length": contents.length,
      "Cache-Control": "no-cache",
      "X-Content-Type-Options": "nosniff",
    });
    res.end(req.method === "HEAD" ? undefined : contents);
  } catch (error) {
    if (error.code === "ENOENT") json(res, 404, { error: "Not found" });
    else throw error;
  }
}

function createServer(options = {}) {
  const compileQueue = options.compileQueue || new JobQueue(
    positiveInteger(process.env.MAX_CONCURRENT_JOBS, DEFAULT_MAX_CONCURRENT_JOBS),
    positiveInteger(process.env.MAX_QUEUED_JOBS, DEFAULT_MAX_QUEUED_JOBS),
  );
  const runSessions = options.runSessions || new InteractiveRunStore({
    maxSessions: options.maxInteractiveRunSessions,
    finishedRetentionMs: options.finishedRunRetentionMs,
  });
  const rateLimiter = options.rateLimiter || new FixedWindowRateLimiter(
    positiveInteger(process.env.RATE_LIMIT_REQUESTS, DEFAULT_RATE_LIMIT),
    positiveInteger(process.env.RATE_LIMIT_WINDOW_MS, RATE_LIMIT_WINDOW_MS),
  );
  const trustProxy = options.trustProxy ?? process.env.TRUST_PROXY === "1";

  const server = http.createServer(async (req, res) => {
    const url = new URL(req.url, "http://localhost");
    setSecurityHeaders(res);
    try {
      if ((req.method === "GET" || req.method === "HEAD") && url.pathname === "/api/ready") {
        const status = serviceStatus(options);
        json(res, status.ready ? 200 : 503, {
          ready: status.ready,
          compiler: status.compiler.available,
          architectures: {
            aarch64: status.architectures.aarch64.available,
            arm32: status.architectures.arm32.available,
          },
          queue: compileQueue.status,
          runs: runSessions.status,
        }, req.method === "HEAD");
        return;
      }

      if ((req.method === "GET" || req.method === "HEAD") && url.pathname === "/api/health") {
        const status = serviceStatus(options);
        json(res, 200, {
          ready: status.ready,
          compiler: status.compiler,
          architectures: status.architectures,
          queue: compileQueue.status,
          runs: runSessions.status,
        }, req.method === "HEAD");
        return;
      }

      if (req.method === "GET" && url.pathname === "/api/examples") {
        json(res, 200, Object.entries(EXAMPLES).map(([id, value]) => ({ id, name: value.name })));
        return;
      }

      if (req.method === "GET" && url.pathname.startsWith("/api/examples/")) {
        const id = decodeURIComponent(url.pathname.slice("/api/examples/".length));
        const example = EXAMPLES[id];
        if (!example) {
          json(res, 404, { error: "Example not found" });
          return;
        }
        const source = await fsp.readFile(path.join(PROJECT_ROOT, example.file), "utf8");
        json(res, 200, { id, name: example.name, source });
        return;
      }

      if (req.method === "POST" && url.pathname === "/api/runs") {
        const rateLimit = rateLimiter.consume(clientAddress(req, trustProxy));
        res.setHeader("RateLimit-Limit", String(rateLimiter.limit));
        res.setHeader("RateLimit-Remaining", String(rateLimit.remaining));
        if (!rateLimit.allowed) {
          res.setHeader("Retry-After", String(rateLimit.retryAfterSeconds));
          json(res, 429, { error: "Too many compile requests. Please wait before trying again." });
          return;
        }
        const contentType = (req.headers["content-type"] || "").split(";", 1)[0].trim();
        if (contentType !== "application/json") {
          json(res, 415, { error: "Content-Type must be application/json" });
          return;
        }
        const request = validateCompileRequest(await readJsonBody(req));
        const result = await compileQueue.run(() => runSessions.start({ ...request, run: true }, options));
        json(res, 200, result);
        return;
      }

      const runPath = /^\/api\/runs\/([^/]+)(?:\/([^/]+))?$/.exec(url.pathname);
      if (runPath) {
        const id = decodeURIComponent(runPath[1]);
        const action = runPath[2];

        if (req.method === "GET" && !action) {
          json(res, 200, runSessions.responseFor(runSessions.get(id)));
          return;
        }

        if (req.method === "POST" && action === "input") {
          const contentType = (req.headers["content-type"] || "").split(";", 1)[0].trim();
          if (contentType !== "application/json") {
            json(res, 415, { error: "Content-Type must be application/json" });
            return;
          }
          const input = validateInteractiveInputRequest(await readJsonBody(req));
          json(res, 200, await runSessions.sendInput(id, input));
          return;
        }

        if ((req.method === "DELETE" && !action) || (req.method === "POST" && action === "stop")) {
          json(res, 200, runSessions.stop(id));
          return;
        }

        json(res, 404, { error: "Not found" });
        return;
      }

      if (req.method === "POST" && url.pathname === "/api/compile") {
        const rateLimit = rateLimiter.consume(clientAddress(req, trustProxy));
        res.setHeader("RateLimit-Limit", String(rateLimiter.limit));
        res.setHeader("RateLimit-Remaining", String(rateLimit.remaining));
        if (!rateLimit.allowed) {
          res.setHeader("Retry-After", String(rateLimit.retryAfterSeconds));
          json(res, 429, { error: "Too many compile requests. Please wait before trying again." });
          return;
        }
        const contentType = (req.headers["content-type"] || "").split(";", 1)[0].trim();
        if (contentType !== "application/json") {
          json(res, 415, { error: "Content-Type must be application/json" });
          return;
        }
        const request = validateCompileRequest(await readJsonBody(req));
        const result = await compileQueue.run(() => compileProgram(request, options));
        json(res, 200, result);
        return;
      }

      if (req.method === "GET" || req.method === "HEAD") {
        await serveStatic(req, res, url.pathname);
        return;
      }

      json(res, 404, { error: "Not found" });
    } catch (error) {
      if (!error.statusCode) console.error(error);
      json(res, error.statusCode || 500, {
        error: error.statusCode ? error.message : "Internal server error",
      });
    }
  });

  server.on("close", () => {
    runSessions.closeAll().catch((error) => console.error(error));
  });
  return server;
}

if (require.main === module) {
  const host = process.env.HOST || "127.0.0.1";
  const port = Number(process.env.PORT || 3000);
  const server = createServer();
  server.listen(port, host, () => {
    console.log(`WACC Compiler Studio is running at http://${host}:${port}`);
  });

  const shutdown = () => {
    server.close(() => process.exit(0));
    setTimeout(() => process.exit(1), 15_000).unref();
  };
  process.on("SIGINT", shutdown);
  process.on("SIGTERM", shutdown);
}

module.exports = {
  FixedWindowRateLimiter,
  InteractiveRunStore,
  JobQueue,
  compileProgram,
  createServer,
  findExecutable,
  serviceStatus,
  toolchainStatus,
  validateCompileRequest,
  validateInteractiveInputRequest,
};
