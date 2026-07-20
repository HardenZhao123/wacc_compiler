const { compileProgram } = require("./server/compiler");
const {
  createDefaultCompileQueue,
  createDefaultRateLimiter,
  createDefaultRunStore,
  createServer,
} = require("./server/create-server");
const {
  InteractiveRunSession,
  InteractiveRunStore,
} = require("./server/interactive-runs");
const { JobQueue } = require("./server/job-queue");
const { FixedWindowRateLimiter } = require("./server/rate-limiter");
const {
  findExecutable,
  serviceStatus,
  toolchainStatus,
} = require("./server/toolchain");
const {
  validateCompileRequest,
  validateInteractiveInputRequest,
} = require("./server/validation");

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
  InteractiveRunSession,
  InteractiveRunStore,
  JobQueue,
  compileProgram,
  createDefaultCompileQueue,
  createDefaultRateLimiter,
  createDefaultRunStore,
  createServer,
  findExecutable,
  serviceStatus,
  toolchainStatus,
  validateCompileRequest,
  validateInteractiveInputRequest,
};
