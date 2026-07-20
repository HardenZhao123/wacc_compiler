import { elements } from "./dom.js";

export function updateLineNumbers() {
  const count = elements.sourceEditor.value.split("\n").length;
  elements.lineNumbers.textContent = Array.from({ length: count }, (_, index) => index + 1).join("\n");
}

export function setupEditor({ showToast }) {
  elements.sourceEditor.addEventListener("input", updateLineNumbers);
  elements.sourceEditor.addEventListener("scroll", () => {
    elements.lineNumbers.scrollTop = elements.sourceEditor.scrollTop;
  });
  elements.sourceEditor.addEventListener("keydown", (event) => {
    if (event.key !== "Tab") return;
    event.preventDefault();
    const start = elements.sourceEditor.selectionStart;
    const end = elements.sourceEditor.selectionEnd;
    elements.sourceEditor.setRangeText("  ", start, end, "end");
    updateLineNumbers();
  });

  elements.fileInput.addEventListener("change", async () => {
    const [file] = elements.fileInput.files;
    if (!file) return;
    if (file.size > 200_000) {
      showToast("File is too large");
      return;
    }
    elements.sourceEditor.value = await file.text();
    elements.sourceTitle.textContent = file.name;
    elements.exampleSelect.value = "";
    updateLineNumbers();
  });

  updateLineNumbers();
}
