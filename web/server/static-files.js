const fsp = require("node:fs/promises");
const path = require("node:path");

const { MIME_TYPES, PUBLIC_ROOT } = require("./config");
const { json } = require("./http-utils");

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

module.exports = { serveStatic };
