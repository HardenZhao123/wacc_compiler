import { fetchHealth } from "./api.js";
import { elements } from "./dom.js";

export async function checkEnvironment() {
  try {
    const health = await fetchHealth();
    const availableCount = Object.values(health.architectures).filter((entry) => entry.available).length;
    elements.environmentStatus.className = `header-status ${availableCount === 2 ? "available" : "unavailable"}`;
    elements.environmentStatus.lastElementChild.textContent = availableCount === 2
      ? "Both execution toolchains ready"
      : availableCount === 0
        ? "Compile ready · execution tools missing"
        : "One execution toolchain ready";
  } catch {
    elements.environmentStatus.className = "header-status unavailable";
    elements.environmentStatus.lastElementChild.textContent = "Server unavailable";
  }
}
