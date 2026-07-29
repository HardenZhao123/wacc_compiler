import { fetchHealth } from "./api.js";
import { elements } from "./dom.js";

export async function checkEnvironment() {
  try {
    const health = await fetchHealth();
    const architectureEntries = Object.values(health.architectures);
    const availableCount = architectureEntries.filter((entry) => entry.available).length;
    const totalCount = architectureEntries.length;
    elements.environmentStatus.className = `header-status ${availableCount === totalCount ? "available" : "unavailable"}`;
    elements.environmentStatus.lastElementChild.textContent = availableCount === totalCount
      ? "All execution toolchains ready"
      : availableCount === 0
        ? "Compile ready · execution tools missing"
        : `${availableCount} of ${totalCount} execution toolchains ready`;
  } catch {
    elements.environmentStatus.className = "header-status unavailable";
    elements.environmentStatus.lastElementChild.textContent = "Server unavailable";
  }
}
