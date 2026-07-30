import { createCompilerSession } from "./js/compiler-session.js";
import { elements } from "./js/dom.js";
import { setupEditor } from "./js/editor.js";
import { checkEnvironment } from "./js/environment.js";
import { setupExamples } from "./js/examples.js";
import { createSpecDialog } from "./js/spec-dialog.js";
import { showToast } from "./js/toast.js";

setupEditor({ showToast });
setupExamples();
createSpecDialog({ showToast });

const compilerSession = createCompilerSession({ showToast });

document.querySelectorAll('input[name="architecture"]').forEach((input) => {
  input.addEventListener("change", compilerSession.syncOptimisationAvailability);
});
elements.optimiseInput.addEventListener("change", compilerSession.updateOptimiseChoice);
compilerSession.syncOptimisationAvailability();

elements.compileButton.addEventListener("click", () => compilerSession.compile(false));
elements.runButton.addEventListener("click", () => compilerSession.compile(true));
elements.interactiveInputForm.addEventListener("submit", (event) => {
  event.preventDefault();
  compilerSession.sendInteractiveInput();
});
elements.stopRunButton.addEventListener("click", compilerSession.stopActiveRun);

document.querySelectorAll(".tab").forEach((button) => {
  button.addEventListener("click", () => compilerSession.switchTab(button.dataset.tab));
});

elements.copyButton.addEventListener("click", async () => {
  await navigator.clipboard.writeText(compilerSession.currentTabText());
  showToast("Copied to clipboard");
});

elements.downloadButton.addEventListener("click", compilerSession.downloadAssembly);

checkEnvironment();
