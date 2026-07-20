import { loadExample, listExamples } from "./api.js";
import { elements } from "./dom.js";
import { updateLineNumbers } from "./editor.js";

export async function loadExamples() {
  try {
    const examples = await listExamples();
    for (const example of examples) {
      const option = document.createElement("option");
      option.value = example.id;
      option.textContent = example.name;
      elements.exampleSelect.append(option);
    }
  } catch {
    elements.exampleSelect.disabled = true;
  }
}

export function setupExamples() {
  elements.exampleSelect.addEventListener("change", async () => {
    if (!elements.exampleSelect.value) return;
    const example = await loadExample(elements.exampleSelect.value);
    elements.sourceEditor.value = example.source;
    elements.sourceTitle.textContent = `${example.id}.wacc`;
    updateLineNumbers();
  });

  loadExamples();
}
