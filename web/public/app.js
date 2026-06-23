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
const specButton = document.querySelector("#specButton");
const specDialog = document.querySelector("#specDialog");
const specCloseButton = document.querySelector("#specCloseButton");
const specCopyButton = document.querySelector("#specCopyButton");
const specContent = document.querySelector("#specContent");

let activeTab = "output";
let latestResult = null;
let toastTimer = null;
let specMarkdown = "";
let specLoadPromise = null;
let previousFocus = null;

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

function escapeHtml(value) {
  return value.replace(/[&<>"']/g, (character) => ({
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    "\"": "&quot;",
    "'": "&#39;",
  }[character]));
}

function renderInlineMarkdown(text) {
  return escapeHtml(text)
    .replace(/`([^`]+)`/g, "<code>$1</code>")
    .replace(/\*\*([^*]+)\*\*/g, "<strong>$1</strong>");
}

function isTableSeparator(line) {
  return /^\|?\s*:?-{3,}:?\s*(\|\s*:?-{3,}:?\s*)+\|?$/.test(line.trim());
}

function splitTableRow(row) {
  let trimmed = row.trim();
  if (trimmed.startsWith("|")) trimmed = trimmed.slice(1);
  if (trimmed.endsWith("|")) trimmed = trimmed.slice(0, -1);
  return trimmed.split("|").map((cell) => cell.trim());
}

function renderMarkdownTable(rows) {
  const header = splitTableRow(rows[0]);
  const body = rows.slice(2).filter((row) => row.trim());
  const headHtml = header.map((cell) => `<th>${renderInlineMarkdown(cell)}</th>`).join("");
  const bodyHtml = body.map((row) => {
    const cells = splitTableRow(row).map((cell) => `<td>${renderInlineMarkdown(cell)}</td>`).join("");
    return `<tr>${cells}</tr>`;
  }).join("");

  return `<div class="spec-table-wrap"><table><thead><tr>${headHtml}</tr></thead><tbody>${bodyHtml}</tbody></table></div>`;
}

function renderMarkdown(markdown) {
  const lines = markdown.replace(/\r\n/g, "\n").split("\n");
  const html = [];
  let paragraph = [];
  let listType = null;
  let inCodeBlock = false;
  let codeLanguage = "";
  let codeLines = [];

  const closeParagraph = () => {
    if (!paragraph.length) return;
    html.push(`<p>${renderInlineMarkdown(paragraph.join(" "))}</p>`);
    paragraph = [];
  };

  const closeList = () => {
    if (!listType) return;
    html.push(`</${listType}>`);
    listType = null;
  };

  const closeCodeBlock = () => {
    const className = codeLanguage ? ` class="language-${escapeHtml(codeLanguage)}"` : "";
    html.push(`<pre><code${className}>${escapeHtml(codeLines.join("\n"))}</code></pre>`);
    inCodeBlock = false;
    codeLanguage = "";
    codeLines = [];
  };

  for (let index = 0; index < lines.length; index += 1) {
    const line = lines[index];
    const trimmed = line.trim();

    if (inCodeBlock) {
      if (trimmed.startsWith("```")) closeCodeBlock();
      else codeLines.push(line);
      continue;
    }

    if (trimmed.startsWith("```")) {
      closeParagraph();
      closeList();
      inCodeBlock = true;
      codeLanguage = trimmed.slice(3).trim().split(/\s+/, 1)[0];
      continue;
    }

    if (!trimmed) {
      closeParagraph();
      closeList();
      continue;
    }

    const heading = /^(#{1,4})\s+(.+)$/.exec(trimmed);
    if (heading) {
      closeParagraph();
      closeList();
      const level = Math.min(heading[1].length + 1, 5);
      html.push(`<h${level}>${renderInlineMarkdown(heading[2])}</h${level}>`);
      continue;
    }

    if (trimmed.startsWith("|") && isTableSeparator(lines[index + 1] || "")) {
      closeParagraph();
      closeList();
      const tableRows = [line, lines[index + 1]];
      index += 2;
      while (index < lines.length && lines[index].trim().startsWith("|")) {
        tableRows.push(lines[index]);
        index += 1;
      }
      index -= 1;
      html.push(renderMarkdownTable(tableRows));
      continue;
    }

    const ordered = /^\d+\.\s+(.+)$/.exec(trimmed);
    const unordered = /^-\s+(.+)$/.exec(trimmed);
    if (ordered || unordered) {
      closeParagraph();
      const nextListType = ordered ? "ol" : "ul";
      if (listType !== nextListType) {
        closeList();
        html.push(`<${nextListType}>`);
        listType = nextListType;
      }
      html.push(`<li>${renderInlineMarkdown((ordered || unordered)[1])}</li>`);
      continue;
    }

    closeList();
    paragraph.push(trimmed);
  }

  closeParagraph();
  closeList();
  if (inCodeBlock) closeCodeBlock();

  return html.join("\n");
}

async function loadSpecification() {
  if (specMarkdown) return specMarkdown;
  if (!specLoadPromise) {
    specLoadPromise = fetch("/wacc-language-spec.md")
      .then(async (response) => {
        if (!response.ok) throw new Error("Could not load the language specification.");
        return response.text();
      })
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
  specDialog.hidden = false;
  document.body.classList.add("spec-open");
  specContent.innerHTML = '<p class="spec-loading">Loading specification...</p>';
  specCopyButton.disabled = true;
  specContent.focus();

  try {
    const markdown = await loadSpecification();
    specContent.innerHTML = renderMarkdown(markdown);
    specCopyButton.disabled = false;
  } catch (error) {
    specContent.innerHTML = `<p class="spec-error">${escapeHtml(error.message)}</p>`;
  }
}

function closeSpecification() {
  if (specDialog.hidden) return;
  specDialog.hidden = true;
  document.body.classList.remove("spec-open");
  if (previousFocus && typeof previousFocus.focus === "function") previousFocus.focus();
  previousFocus = null;
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
specButton.addEventListener("click", openSpecification);
specCloseButton.addEventListener("click", closeSpecification);
specDialog.addEventListener("click", (event) => {
  if (event.target.matches("[data-spec-close]")) closeSpecification();
});
document.addEventListener("keydown", (event) => {
  if (event.key === "Escape" && !specDialog.hidden) closeSpecification();
});
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

specCopyButton.addEventListener("click", async () => {
  const markdown = await loadSpecification();
  await navigator.clipboard.writeText(markdown);
  showToast("Specification copied");
});

updateLineNumbers();
loadExamples();
checkEnvironment();
