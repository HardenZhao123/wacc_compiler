const {
  DEFAULT_MAX_CONCURRENT_JOBS,
  DEFAULT_MAX_QUEUED_JOBS,
} = require("./config");

class JobQueue {
  constructor(maxConcurrent = DEFAULT_MAX_CONCURRENT_JOBS, maxQueued = DEFAULT_MAX_QUEUED_JOBS) {
    this.maxConcurrent = maxConcurrent;
    this.maxQueued = maxQueued;
    this.active = 0;
    this.queue = [];
  }

  get status() {
    return {
      active: this.active,
      queued: this.queue.length,
      maxConcurrent: this.maxConcurrent,
      maxQueued: this.maxQueued,
    };
  }

  run(task) {
    if (this.active < this.maxConcurrent) {
      return new Promise((resolve, reject) => this.start({ task, resolve, reject }));
    }
    if (this.queue.length >= this.maxQueued) {
      return Promise.reject(Object.assign(
        new Error("The compiler is busy. Please try again shortly."),
        { statusCode: 503 },
      ));
    }
    return new Promise((resolve, reject) => this.queue.push({ task, resolve, reject }));
  }

  start(job) {
    this.active += 1;
    Promise.resolve()
      .then(job.task)
      .then(job.resolve, job.reject)
      .finally(() => {
        this.active -= 1;
        const next = this.queue.shift();
        if (next) this.start(next);
      });
  }
}

module.exports = { JobQueue };
