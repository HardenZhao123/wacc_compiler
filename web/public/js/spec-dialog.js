import { loadSpecificationText } from "./api.js";
import { elements } from "./dom.js";
import { escapeHtml, renderMarkdown } from "./markdown.js";

export function createSpecDialog({ showToast }) {
  let specMarkdown = "";
  let specLoadPromise = null;
  let previousFocus = null;

  async function loadSpecification() {
    if (specMarkdown) return specMarkdown;
    if (!specLoadPromise) {
      specLoadPromise = loadSpecificationText()
        .then((markdown) => {
          specMarkdown = markdown;
          return specMarkdown;
        })
        .catch((error) => {
          specLoadPromise = null;
          throw error;
        });
    }
    return specLoadPromise;
  }

  async function openSpecification() {
    previousFocus = document.activeElement;
    elements.specDialog.hidden = false;
    document.body.classList.add("spec-open");
    elements.specContent.innerHTML = '<p class="spec-loading">Loading specification...</p>';
    elements.specCopyButton.disabled = true;
    elements.specContent.focus();

    try {
      const markdown = await loadSpecification();
      elements.specContent.innerHTML = renderMarkdown(markdown);
      elements.specCopyButton.disabled = false;
    } catch (error) {
      elements.specContent.innerHTML = `<p class="spec-error">${escapeHtml(error.message)}</p>`;
    }
  }

  function closeSpecification() {
    if (elements.specDialog.hidden) return;
    elements.specDialog.hidden = true;
    document.body.classList.remove("spec-open");
    if (previousFocus && typeof previousFocus.focus === "function") previousFocus.focus();
    previousFocus = null;
  }

  elements.specButton.addEventListener("click", openSpecification);
  elements.specCloseButton.addEventListener("click", closeSpecification);
  elements.specDialog.addEventListener("click", (event) => {
    if (event.target.matches("[data-spec-close]")) closeSpecification();
  });
  document.addEventListener("keydown", (event) => {
    if (event.key === "Escape" && !elements.specDialog.hidden) closeSpecification();
  });
  elements.specCopyButton.addEventListener("click", async () => {
    const markdown = await loadSpecification();
    await navigator.clipboard.writeText(markdown);
    showToast("Specification copied");
  });

  return {
    closeSpecification,
    loadSpecification,
    openSpecification,
  };
}
