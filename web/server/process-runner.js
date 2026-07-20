const { spawn } = require("node:child_process");

const { MAX_PROCESS_OUTPUT_BYTES } = require("./config");

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

module.exports = {
  appendOutputBuffer,
  runProcess,
};
