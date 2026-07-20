const {
  DEFAULT_RATE_LIMIT,
  RATE_LIMIT_WINDOW_MS,
} = require("./config");

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

module.exports = { FixedWindowRateLimiter };
