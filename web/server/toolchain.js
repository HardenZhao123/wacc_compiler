const fs = require("node:fs");
const path = require("node:path");

const { PROJECT_ROOT } = require("./config");

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

module.exports = {
  compilerInvocation,
  findExecutable,
  runnerConfig,
  serviceStatus,
  toolchainStatus,
};
