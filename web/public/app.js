const sourceEditor = document.querySelector("#sourceEditor");
const lineNumbers = document.querySelector("#lineNumbers");
const stdinInput = document.querySelector("#stdinInput");
const fileInput = document.querySelector("#fileInput");
const exampleSelect = document.querySelector("#exampleSelect");
const sourceTitle = document.querySelector("#sourceTitle");
const optimiseInput = document.querySelector("#optimiseInput");
const compileButton = document.querySelector("#compileButton");
const runButton = document.querySelector("#runButton");
const environmentStatus = document.querySelector("#environmentStatus");
const resultBadge = document.querySelector("#resultBadge");
const resultMeta = document.querySelector("#resultMeta");
const emptyState = document.querySelector("#emptyState");
const resultView = document.querySelector("#resultView");
const copyButton = document.querySelector("#copyButton");
const downloadButton = document.querySelector("#downloadButton");
const toast = document.querySelector("#toast");

let activeTab = "output";
let latestResult = null;
let toastTimer = null;

function updateLineNumbers() {
  const count = sourceEditor.value.split("\n").length;
  lineNumbers.textContent = Array.from({ length: count }, (_, index) => index + 1).join("\n");
}

function selectedArchitecture() {
  return document.querySelector('input[name="architecture"]:checked').value;
}

function showToast(message) {
  clearTimeout(toastTimer);
  toast.textContent = message;
  toast.classList.add("show");
  toastTimer = setTimeout(() => toast.classList.remove("show"), 1800);
}

function setBusy(busy, run) {
  compileButton.disabled = busy;
  runButton.disabled = busy;
  compileButton.textContent = busy && !run ? "Compiling..." : "Compile assembly";
  runButton.innerHTML = busy && run
    ? "Running..."
    : '<span class="play-icon" aria-hidden="true">▶</span> Compile &amp; run';
  if (busy) {
    resultBadge.className = "result-badge running";
    resultBadge.textContent = run ? "Running" : "Compiling";
  }
}

function compilerLog(result) {
  const sections = [];
  if (result.message) sections.push(result.message);
  if (result.compiler?.stdout) sections.push(result.compiler.stdout.trimEnd());
  if (result.compiler?.stderr) sections.push(result.compiler.stderr.trimEnd());
  if (result.execution?.stage === "assemble" && result.execution.stderr) {
    sections.push(`Assembler error:\n${result.execution.stderr.trimEnd()}`);
  }
  return sections.filter(Boolean).join("\n\n");
}

function currentTabText() {
  if (!latestResult) return "";
  if (activeTab === "assembly") return latestResult.assembly || "No assembly was generated.";
  if (activeTab === "log") return compilerLog(latestResult) || "No compiler messages.";

  if (!latestResult.execution) {
    return latestResult.ok
      ? "Compilation completed. Choose “Compile & run” to execute the program."
      : compilerLog(latestResult);
  }
  const execution = latestResult.execution;
  const parts = [];
  if (execution.stdout) parts.push(execution.stdout.trimEnd());
  if (execution.stderr) parts.push(execution.stderr.trimEnd());
  if (execution.exitCode !== null && execution.exitCode !== undefined) {
    parts.push(`Process exited with code ${execution.exitCode}`);
  }
  return parts.filter(Boolean).join("\n\n") || "Program finished without output.";
}

function renderResult() {
  const text = currentTabText();
  emptyState.hidden = Boolean(latestResult);
  resultView.hidden = !latestResult;
  resultView.textContent = text;
  copyButton.disabled = !text;
  downloadButton.disabled = !latestResult?.assembly;

  if (!latestResult) return;
  const duration = (latestResult.compiler?.durationMs || 0) + (latestResult.execution?.durationMs || 0);
  const arch = latestResult.architecture?.toUpperCase() || selectedArchitecture().toUpperCase();
  resultMeta.textContent = `${arch} · ${latestResult.optimise ? "optimised" : "unoptimised"} · ${duration} ms`;
}

function switchTab(tabName) {
  activeTab = tabName;
  document.querySelectorAll(".tab").forEach((button) => {
    const active = button.dataset.tab === tabName;
    button.classList.toggle("active", active);
    button.setAttribute("aria-selected", String(active));
  });
  renderResult();
}

async function compile(run) {
  setBusy(true, run);
  try {
    const response = await fetch("/api/compile", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        source: sourceEditor.value,
        architecture: selectedArchitecture(),
        optimise: optimiseInput.checked,
        run,
        stdin: stdinInput.value,
      }),
    });
    const result = await response.json();
    if (!response.ok) throw new Error(result.error || "Request failed");

    latestResult = result;
    resultBadge.className = `result-badge ${result.ok ? "success" : "failure"}`;
    resultBadge.textContent = result.ok ? (run ? "Finished" : "Compiled") : "Failed";

    if (!result.ok) switchTab(result.phase === "run-environment" ? "output" : "log");
    else switchTab(run ? "output" : "assembly");
  } catch (error) {
    latestResult = {
      ok: false,
      message: error.message,
      compiler: { stdout: "", stderr: "", durationMs: 0 },
      assembly: "",
      execution: null,
      architecture: selectedArchitecture(),
      optimise: optimiseInput.checked,
    };
    resultBadge.className = "result-badge failure";
    resultBadge.textContent = "Failed";
    switchTab("log");
  } finally {
    setBusy(false, run);
    renderResult();
  }
}

async function loadExamples() {
  try {
    const response = await fetch("/api/examples");
    const examples = await response.json();
    for (const example of examples) {
      const option = document.createElement("option");
      option.value = example.id;
      option.textContent = example.name;
      exampleSelect.append(option);
    }
  } catch {
    exampleSelect.disabled = true;
  }
}

async function checkEnvironment() {
  try {
    const response = await fetch("/api/health");
    const health = await response.json();
    const availableCount = Object.values(health.architectures).filter((entry) => entry.available).length;
    environmentStatus.className = `header-status ${availableCount === 2 ? "available" : "unavailable"}`;
    environmentStatus.lastElementChild.textContent = availableCount === 2
      ? "Both execution toolchains ready"
      : availableCount === 0
        ? "Compile ready · execution tools missing"
        : "One execution toolchain ready";
  } catch {
    environmentStatus.className = "header-status unavailable";
    environmentStatus.lastElementChild.textContent = "Server unavailable";
  }
}

sourceEditor.addEventListener("input", updateLineNumbers);
sourceEditor.addEventListener("scroll", () => { lineNumbers.scrollTop = sourceEditor.scrollTop; });
sourceEditor.addEventListener("keydown", (event) => {
  if (event.key !== "Tab") return;
  event.preventDefault();
  const start = sourceEditor.selectionStart;
  const end = sourceEditor.selectionEnd;
  sourceEditor.setRangeText("  ", start, end, "end");
  updateLineNumbers();
});

fileInput.addEventListener("change", async () => {
  const [file] = fileInput.files;
  if (!file) return;
  if (file.size > 200_000) {
    showToast("File is too large");
    return;
  }
  sourceEditor.value = await file.text();
  sourceTitle.textContent = file.name;
  exampleSelect.value = "";
  updateLineNumbers();
});

exampleSelect.addEventListener("change", async () => {
  if (!exampleSelect.value) return;
  const response = await fetch(`/api/examples/${encodeURIComponent(exampleSelect.value)}`);
  const example = await response.json();
  sourceEditor.value = example.source;
  sourceTitle.textContent = `${example.id}.wacc`;
  updateLineNumbers();
});

compileButton.addEventListener("click", () => compile(false));
runButton.addEventListener("click", () => compile(true));
document.querySelectorAll(".tab").forEach((button) => {
  button.addEventListener("click", () => switchTab(button.dataset.tab));
});

copyButton.addEventListener("click", async () => {
  await navigator.clipboard.writeText(currentTabText());
  showToast("Copied to clipboard");
});

downloadButton.addEventListener("click", () => {
  if (!latestResult?.assembly) return;
  const blob = new Blob([latestResult.assembly], { type: "text/plain" });
  const link = document.createElement("a");
  link.href = URL.createObjectURL(blob);
  link.download = `${sourceTitle.textContent.replace(/\.wacc$/i, "") || "program"}.s`;
  link.click();
  URL.revokeObjectURL(link.href);
});

updateLineNumbers();
loadExamples();
checkEnvironment();
