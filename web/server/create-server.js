const fsp = require("node:fs/promises");
const http = require("node:http");
const path = require("node:path");

const {
  DEFAULT_MAX_CONCURRENT_JOBS,
  DEFAULT_MAX_QUEUED_JOBS,
  DEFAULT_RATE_LIMIT,
  EXAMPLES,
  FINISHED_RUN_RETENTION_MS,
  PROJECT_ROOT,
  RATE_LIMIT_WINDOW_MS,
} = require("./config");
const { compileProgram } = require("./compiler");
const {
  clientAddress,
  json,
  positiveInteger,
  readJsonBody,
  setSecurityHeaders,
} = require("./http-utils");
const { InteractiveRunStore } = require("./interactive-runs");
const { JobQueue } = require("./job-queue");
const { FixedWindowRateLimiter } = require("./rate-limiter");
const { serviceStatus } = require("./toolchain");
const { serveStatic } = require("./static-files");
const {
  validateCompileRequest,
  validateInteractiveInputRequest,
} = require("./validation");

function createDefaultCompileQueue() {
  return new JobQueue(
    positiveInteger(process.env.MAX_CONCURRENT_JOBS, DEFAULT_MAX_CONCURRENT_JOBS),
    positiveInteger(process.env.MAX_QUEUED_JOBS, DEFAULT_MAX_QUEUED_JOBS),
  );
}

function createDefaultRateLimiter() {
  return new FixedWindowRateLimiter(
    positiveInteger(process.env.RATE_LIMIT_REQUESTS, DEFAULT_RATE_LIMIT),
    positiveInteger(process.env.RATE_LIMIT_WINDOW_MS, RATE_LIMIT_WINDOW_MS),
  );
}

function createDefaultRunStore(options = {}) {
  return new InteractiveRunStore({
    maxSessions: options.maxInteractiveRunSessions,
    finishedRetentionMs: options.finishedRunRetentionMs || FINISHED_RUN_RETENTION_MS,
  });
}

function jsonReadiness(status, compileQueue, runSessions) {
  return {
    ready: status.ready,
    compiler: status.compiler.available,
    architectures: {
      aarch64: status.architectures.aarch64.available,
      arm32: status.architectures.arm32.available,
    },
    queue: compileQueue.status,
    runs: runSessions.status,
  };
}

function jsonHealth(status, compileQueue, runSessions) {
  return {
    ready: status.ready,
    compiler: status.compiler,
    architectures: status.architectures,
    queue: compileQueue.status,
    runs: runSessions.status,
  };
}

function assertJsonContent(req, res) {
  const contentType = (req.headers["content-type"] || "").split(";", 1)[0].trim();
  if (contentType === "application/json") return true;
  json(res, 415, { error: "Content-Type must be application/json" });
  return false;
}

function consumeCompileRateLimit(req, res, rateLimiter, trustProxy) {
  const rateLimit = rateLimiter.consume(clientAddress(req, trustProxy));
  res.setHeader("RateLimit-Limit", String(rateLimiter.limit));
  res.setHeader("RateLimit-Remaining", String(rateLimit.remaining));
  if (rateLimit.allowed) return true;
  res.setHeader("Retry-After", String(rateLimit.retryAfterSeconds));
  json(res, 429, { error: "Too many compile requests. Please wait before trying again." });
  return false;
}

async function handleExampleRequest(req, res, pathname) {
  if (req.method === "GET" && pathname === "/api/examples") {
    json(res, 200, Object.entries(EXAMPLES).map(([id, value]) => ({ id, name: value.name })));
    return true;
  }

  if (req.method === "GET" && pathname.startsWith("/api/examples/")) {
    const id = decodeURIComponent(pathname.slice("/api/examples/".length));
    const example = EXAMPLES[id];
    if (!example) {
      json(res, 404, { error: "Example not found" });
      return true;
    }
    const source = await fsp.readFile(path.join(PROJECT_ROOT, example.file), "utf8");
    json(res, 200, { id, name: example.name, source });
    return true;
  }

  return false;
}

async function handleRunRequest(req, res, pathname, runSessions) {
  const runPath = /^\/api\/runs\/([^/]+)(?:\/([^/]+))?$/.exec(pathname);
  if (!runPath) return false;

  const id = decodeURIComponent(runPath[1]);
  const action = runPath[2];

  if (req.method === "GET" && !action) {
    json(res, 200, runSessions.responseFor(runSessions.get(id)));
    return true;
  }

  if (req.method === "POST" && action === "input") {
    if (!assertJsonContent(req, res)) return true;
    const input = validateInteractiveInputRequest(await readJsonBody(req));
    json(res, 200, await runSessions.sendInput(id, input));
    return true;
  }

  if ((req.method === "DELETE" && !action) || (req.method === "POST" && action === "stop")) {
    json(res, 200, runSessions.stop(id));
    return true;
  }

  json(res, 404, { error: "Not found" });
  return true;
}

async function handleCompileRequest(req, res, options, compileQueue) {
  if (!assertJsonContent(req, res)) return;
  const request = validateCompileRequest(await readJsonBody(req));
  const result = await compileQueue.run(() => compileProgram(request, options));
  json(res, 200, result);
}

function createServer(options = {}) {
  const compileQueue = options.compileQueue || createDefaultCompileQueue();
  const runSessions = options.runSessions || createDefaultRunStore(options);
  const rateLimiter = options.rateLimiter || createDefaultRateLimiter();
  const trustProxy = options.trustProxy ?? process.env.TRUST_PROXY === "1";

  const server = http.createServer(async (req, res) => {
    const url = new URL(req.url, "http://localhost");
    setSecurityHeaders(res);
    try {
      if ((req.method === "GET" || req.method === "HEAD") && url.pathname === "/api/ready") {
        const status = serviceStatus(options);
        json(res, status.ready ? 200 : 503, jsonReadiness(status, compileQueue, runSessions), req.method === "HEAD");
        return;
      }

      if ((req.method === "GET" || req.method === "HEAD") && url.pathname === "/api/health") {
        const status = serviceStatus(options);
        json(res, 200, jsonHealth(status, compileQueue, runSessions), req.method === "HEAD");
        return;
      }

      if (await handleExampleRequest(req, res, url.pathname)) return;

      if (req.method === "POST" && url.pathname === "/api/runs") {
        if (!consumeCompileRateLimit(req, res, rateLimiter, trustProxy)) return;
        if (!assertJsonContent(req, res)) return;
        const request = validateCompileRequest(await readJsonBody(req));
        const result = await compileQueue.run(() => runSessions.start({ ...request, run: true }, options));
        json(res, 200, result);
        return;
      }

      if (await handleRunRequest(req, res, url.pathname, runSessions)) return;

      if (req.method === "POST" && url.pathname === "/api/compile") {
        if (!consumeCompileRateLimit(req, res, rateLimiter, trustProxy)) return;
        await handleCompileRequest(req, res, options, compileQueue);
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

module.exports = {
  createServer,
  createDefaultCompileQueue,
  createDefaultRateLimiter,
  createDefaultRunStore,
};
