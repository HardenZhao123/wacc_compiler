const assert = require("node:assert/strict");
const fs = require("node:fs/promises");
const http = require("node:http");
const os = require("node:os");
const path = require("node:path");
const test = require("node:test");

const {
  FixedWindowRateLimiter,
  JobQueue,
  compileProgram,
  createServer,
  serviceStatus,
  validateCompileRequest,
} = require("../server");

test("validates architecture and applies optimisation defaults", () => {
  const request = validateCompileRequest({
    source: "begin\n  skip\nend",
    architecture: "aarch64",
  });
  assert.equal(request.optimise, true);
  assert.equal(request.run, false);
  assert.equal(request.stdin, "");
  assert.throws(
    () => validateCompileRequest({ source: "begin end", architecture: "x86" }),
    /Architecture must be/,
  );
});

test("bounds concurrent and queued compiler jobs", async () => {
  const queue = new JobQueue(1, 1);
  let releaseFirst;
  const first = queue.run(() => new Promise((resolve) => { releaseFirst = resolve; }));
  const second = queue.run(() => "second");

  await assert.rejects(
    queue.run(() => "overflow"),
    (error) => error.statusCode === 503 && /compiler is busy/i.test(error.message),
  );
  assert.deepEqual(queue.status, {
    active: 1,
    queued: 1,
    maxConcurrent: 1,
    maxQueued: 1,
  });

  releaseFirst("first");
  assert.equal(await first, "first");
  assert.equal(await second, "second");
});

test("rate limits each client within a fixed window", () => {
  const limiter = new FixedWindowRateLimiter(2, 1_000);
  assert.equal(limiter.consume("client", 100).allowed, true);
  assert.equal(limiter.consume("client", 200).allowed, true);
  assert.equal(limiter.consume("client", 300).allowed, false);
  assert.equal(limiter.consume("other", 300).allowed, true);
  assert.equal(limiter.consume("client", 1_101).allowed, true);
});

test("serves the editor and compiles source through an isolated file", async (t) => {
  const fakeDir = await fs.mkdtemp(path.join(os.tmpdir(), "wacc-web-test-"));
  const fakeCompiler = path.join(fakeDir, "fake-compiler.js");
  await fs.writeFile(fakeCompiler, `#!/usr/bin/env node
const fs = require("node:fs");
const path = require("node:path");
const sourcePath = process.argv[2];
fs.writeFileSync(path.join(process.cwd(), "program.s"), ".text\\n.global main\\n");
console.log("compiled " + path.basename(sourcePath));
`, { mode: 0o755 });

  const server = createServer({ compilerCommand: { command: fakeCompiler, prefixArgs: [] } });
  await new Promise((resolve) => server.listen(0, "127.0.0.1", resolve));
  t.after(async () => {
    await new Promise((resolve) => server.close(resolve));
    await fs.rm(fakeDir, { recursive: true, force: true });
  });

  const address = server.address();
  const page = await request(address.port, "GET", "/");
  assert.equal(page.statusCode, 200);
  assert.match(page.body, /WACC Compiler Studio/);
  assert.match(page.headers["content-security-policy"], /default-src 'self'/);
  assert.equal(page.headers["x-frame-options"], "DENY");

  const manifest = await request(address.port, "GET", "/manifest.webmanifest");
  assert.equal(manifest.statusCode, 200);
  assert.match(manifest.headers["content-type"], /application\/manifest\+json/);

  const readiness = await request(address.port, "GET", "/api/ready");
  assert.ok(readiness.statusCode === 200 || readiness.statusCode === 503);
  assert.equal(typeof JSON.parse(readiness.body).ready, "boolean");

  const headReadiness = await request(address.port, "HEAD", "/api/ready");
  assert.equal(headReadiness.statusCode, readiness.statusCode);
  assert.equal(headReadiness.body, "");
  assert.match(headReadiness.headers["content-type"], /application\/json/);

  const wrongContentType = await request(address.port, "POST", "/api/compile");
  assert.equal(wrongContentType.statusCode, 415);

  const compiled = await request(address.port, "POST", "/api/compile", {
    source: "begin\n  skip\nend",
    architecture: "arm32",
    optimise: false,
    run: false,
  });
  assert.equal(compiled.statusCode, 200);
  const result = JSON.parse(compiled.body);
  assert.equal(result.ok, true);
  assert.equal(result.architecture, "arm32");
  assert.match(result.assembly, /\.global main/);
  assert.match(result.compiler.stdout, /compiled program\.wacc/);
});

test("assembles and runs generated code with the configured target toolchain", async (t) => {
  const fakeDir = await fs.mkdtemp(path.join(os.tmpdir(), "wacc-run-test-"));
  const fakeCompiler = path.join(fakeDir, "fake-compiler.js");
  const fakeLinker = path.join(fakeDir, "fake-linker.js");
  const fakeQemu = path.join(fakeDir, "fake-qemu.js");

  await fs.writeFile(fakeCompiler, `#!/usr/bin/env node
const fs = require("node:fs");
const path = require("node:path");
fs.writeFileSync(path.join(process.cwd(), "program.s"), ".text\\n.global main\\n");
`, { mode: 0o755 });
  await fs.writeFile(fakeLinker, `#!/usr/bin/env node
const fs = require("node:fs");
const outputIndex = process.argv.indexOf("-o") + 1;
fs.writeFileSync(process.argv[outputIndex], "fake binary");
`, { mode: 0o755 });
  await fs.writeFile(fakeQemu, `#!/usr/bin/env node
process.stdin.resume();
process.stdin.on("end", () => console.log("Hello from emulated WACC"));
`, { mode: 0o755 });
  await fs.mkdir(path.join(fakeDir, "lib"));
  await fs.writeFile(path.join(fakeDir, "lib", "Scrt1.o"), "fake startup object");
  await fs.writeFile(path.join(fakeDir, "lib", "crti.o"), "fake startup object");

  const previous = {
    gcc: process.env.WACC_AARCH64_GCC,
    qemu: process.env.WACC_AARCH64_QEMU,
    sysroot: process.env.WACC_AARCH64_SYSROOT,
    arm32Gcc: process.env.WACC_ARM32_GCC,
    arm32Qemu: process.env.WACC_ARM32_QEMU,
    arm32Sysroot: process.env.WACC_ARM32_SYSROOT,
  };
  process.env.WACC_AARCH64_GCC = fakeLinker;
  process.env.WACC_AARCH64_QEMU = fakeQemu;
  process.env.WACC_AARCH64_SYSROOT = fakeDir;
  process.env.WACC_ARM32_GCC = fakeLinker;
  process.env.WACC_ARM32_QEMU = fakeQemu;
  process.env.WACC_ARM32_SYSROOT = fakeDir;

  t.after(async () => {
    restoreEnv("WACC_AARCH64_GCC", previous.gcc);
    restoreEnv("WACC_AARCH64_QEMU", previous.qemu);
    restoreEnv("WACC_AARCH64_SYSROOT", previous.sysroot);
    restoreEnv("WACC_ARM32_GCC", previous.arm32Gcc);
    restoreEnv("WACC_ARM32_QEMU", previous.arm32Qemu);
    restoreEnv("WACC_ARM32_SYSROOT", previous.arm32Sysroot);
    await fs.rm(fakeDir, { recursive: true, force: true });
  });

  const result = await compileProgram({
    source: "begin\n  println \"Hello\"\nend",
    architecture: "aarch64",
    optimise: true,
    run: true,
    stdin: "",
  }, { compilerCommand: { command: fakeCompiler, prefixArgs: [] } });

  assert.equal(result.ok, true);
  assert.equal(result.execution.stage, "run");
  assert.equal(result.execution.exitCode, 0);
  assert.match(result.execution.stdout, /Hello from emulated WACC/);
  assert.equal(serviceStatus({
    compilerCommand: { command: fakeCompiler, prefixArgs: [] },
  }).ready, true);
});

function request(port, method, pathname, body) {
  return new Promise((resolve, reject) => {
    const encoded = body === undefined ? null : JSON.stringify(body);
    const req = http.request({
      hostname: "127.0.0.1",
      port,
      method,
      path: pathname,
      headers: encoded ? {
        "Content-Type": "application/json",
        "Content-Length": Buffer.byteLength(encoded),
      } : {},
    }, (res) => {
      const chunks = [];
      res.on("data", (chunk) => chunks.push(chunk));
      res.on("end", () => resolve({
        statusCode: res.statusCode,
        headers: res.headers,
        body: Buffer.concat(chunks).toString("utf8"),
      }));
    });
    req.on("error", reject);
    req.end(encoded || undefined);
  });
}

function restoreEnv(name, value) {
  if (value === undefined) delete process.env[name];
  else process.env[name] = value;
}
