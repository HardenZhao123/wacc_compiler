const {
  MAX_INTERACTIVE_INPUT_CHARS,
  MAX_SOURCE_CHARS,
  MAX_STDIN_CHARS,
} = require("./config");

function validateCompileRequest(body) {
  if (!body || typeof body !== "object" || Array.isArray(body)) {
    throw Object.assign(new Error("Request body must be an object"), { statusCode: 400 });
  }

  const source = body.source;
  let architecture = body.architecture;
  const stdin = body.stdin ?? "";

  if (typeof source !== "string" || source.trim().length === 0) {
    throw Object.assign(new Error("WACC source cannot be empty"), { statusCode: 400 });
  }
  if (source.length > MAX_SOURCE_CHARS) {
    throw Object.assign(new Error(`WACC source cannot exceed ${MAX_SOURCE_CHARS} characters`), { statusCode: 400 });
  }
  if (architecture === "x86" || architecture === "x86_64") {
    architecture = "x86-64";
  }
  if (!["aarch64", "arm32", "x86-64"].includes(architecture)) {
    throw Object.assign(new Error("Architecture must be aarch64, arm32, or x86-64"), { statusCode: 400 });
  }
  if (typeof stdin !== "string" || stdin.length > MAX_STDIN_CHARS) {
    throw Object.assign(new Error(`Program input cannot exceed ${MAX_STDIN_CHARS} characters`), { statusCode: 400 });
  }

  return {
    source,
    architecture,
    optimise: architecture === "x86-64" ? false : body.optimise !== false,
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

module.exports = {
  validateCompileRequest,
  validateInteractiveInputRequest,
};
