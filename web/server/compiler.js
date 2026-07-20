const fsp = require("node:fs/promises");
const os = require("node:os");
const path = require("node:path");

const { runProcess } = require("./process-runner");
const { compilerInvocation, runnerConfig, toolchainStatus } = require("./toolchain");

function cleanCompilerText(text, tempDir, sourcePath) {
  return text
    .split(sourcePath).join("program.wacc")
    .split(tempDir + path.sep).join("");
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

module.exports = {
  buildProgram,
  cleanCompilerText,
  compilationFailureResult,
  compileProgram,
  executeAssembly,
  linkAssembly,
};
