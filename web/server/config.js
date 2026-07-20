const path = require("node:path");

const PROJECT_ROOT = path.resolve(__dirname, "../..");
const PUBLIC_ROOT = path.resolve(__dirname, "..", "public");

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

module.exports = {
  DEFAULT_MAX_CONCURRENT_JOBS,
  DEFAULT_MAX_QUEUED_JOBS,
  DEFAULT_RATE_LIMIT,
  EXAMPLES,
  FINISHED_RUN_RETENTION_MS,
  INTERACTIVE_RUN_TIMEOUT_MS,
  MAX_BODY_BYTES,
  MAX_INTERACTIVE_INPUT_CHARS,
  MAX_INTERACTIVE_RUN_SESSIONS,
  MAX_PROCESS_OUTPUT_BYTES,
  MAX_SOURCE_CHARS,
  MAX_STDIN_CHARS,
  MIME_TYPES,
  PROJECT_ROOT,
  PUBLIC_ROOT,
  RATE_LIMIT_WINDOW_MS,
};
