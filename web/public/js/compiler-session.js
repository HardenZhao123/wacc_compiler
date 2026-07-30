import {
  compileProgram,
  fetchRun,
  sendRunInput,
  startRun,
  stopRun,
} from "./api.js";
import { elements, selectedArchitecture } from "./dom.js";

export function createCompilerSession({ showToast }) {
  let activeTab = "output";
  let latestResult = null;
  let activeRunId = null;
  let runPollTimer = null;
  let pollingRun = false;
  let lastSupportedOptimiseChoice = elements.optimiseInput.checked;

  function x86Selected() {
    return selectedArchitecture() === "x86-64";
  }

  function effectiveOptimise() {
    return !x86Selected() && elements.optimiseInput.checked;
  }

  function syncOptimisationAvailability() {
    const disabled = x86Selected();
    const switchRow = elements.optimiseInput.closest(".switch-row");

    if (disabled) {
      elements.optimiseInput.checked = false;
    } else {
      elements.optimiseInput.checked = lastSupportedOptimiseChoice;
    }
    elements.optimiseInput.disabled = disabled;
    switchRow?.classList.toggle("disabled", disabled);
    switchRow?.setAttribute("aria-disabled", String(disabled));
  }

  function compilerLog(result) {
    const sections = [];
    if (result.message) sections.push(result.message);
    if (result.compiler?.stdout) sections.push(result.compiler.stdout.trimEnd());
    if (result.compiler?.stderr) sections.push(result.compiler.stderr.trimEnd());
    if (result.execution?.stage === "assemble" && result.execution.stderr) {
      sections.push(`Assembler error:\n${result.execution.stderr.trimEnd()}`);
    }
    if (result.execution?.spawnError) {
      sections.push(`Runtime start error: ${result.execution.spawnError}`);
    }
    if (result.execution?.timedOut) {
      sections.push("Execution timed out.");
    }
    if (result.execution?.outputExceeded) {
      sections.push("Output was truncated because it exceeded the server limit.");
    }
    return sections.filter(Boolean).join("\n\n");
  }

  function updateResultBadge(result, action = "compile") {
    if (result.execution?.running) {
      elements.resultBadge.className = "result-badge running";
      elements.resultBadge.textContent = result.execution.stopped ? "Stopping" : "Running";
      return;
    }

    elements.resultBadge.className = `result-badge ${result.ok ? "success" : "failure"}`;
    elements.resultBadge.textContent = result.ok
      ? (result.sessionId || action === "run" ? "Finished" : "Compiled")
      : "Failed";
  }

  function stopRunPolling() {
    clearInterval(runPollTimer);
    runPollTimer = null;
    pollingRun = false;
  }

  function setBusy(busy, run) {
    const running = Boolean(activeRunId);
    elements.compileButton.disabled = busy || running;
    elements.runButton.disabled = busy || running;
    elements.stdinInput.disabled = busy || running;
    elements.compileButton.textContent = busy && !run ? "Compiling..." : "Compile assembly";
    elements.runButton.innerHTML = busy && run
      ? "Starting..."
      : '<span class="play-icon" aria-hidden="true">▶</span> Compile &amp; run';
    if (busy) {
      elements.resultBadge.className = "result-badge running";
      elements.resultBadge.textContent = run ? "Starting" : "Compiling";
    }
  }

  function updateInteractiveControls() {
    const acceptingInput = Boolean(activeRunId && latestResult?.execution?.running && !latestResult.execution.stopped);
    elements.interactiveInputForm.hidden = !acceptingInput;
    elements.interactiveInput.disabled = !acceptingInput;
    elements.sendInputButton.disabled = !acceptingInput;
    elements.stopRunButton.disabled = !acceptingInput;
  }

  function currentTabText() {
    if (!latestResult) return "";
    if (activeTab === "assembly") return latestResult.assembly || "No assembly was generated.";
    if (activeTab === "log") return compilerLog(latestResult) || "No compiler messages.";

    if (!latestResult.execution) {
      return latestResult.ok
        ? 'Compilation completed. Choose "Compile & run" to execute the program.'
        : compilerLog(latestResult);
    }
    const execution = latestResult.execution;
    const parts = [];
    if (execution.stdout) parts.push(execution.running ? execution.stdout : execution.stdout.trimEnd());
    if (execution.stderr) parts.push(execution.running ? execution.stderr : execution.stderr.trimEnd());
    if (execution.exitCode !== null && execution.exitCode !== undefined) {
      parts.push(`Process exited with code ${execution.exitCode}`);
    }
    if (parts.some(Boolean)) return parts.filter(Boolean).join("\n\n");
    return execution.running ? "Program is running." : "Program finished without output.";
  }

  function renderResult() {
    const text = currentTabText();
    const stickToBottom = activeTab === "output"
      && !elements.resultView.hidden
      && elements.resultView.scrollTop + elements.resultView.clientHeight >= elements.resultView.scrollHeight - 24;
    elements.emptyState.hidden = Boolean(latestResult);
    elements.resultView.hidden = !latestResult;
    elements.resultView.textContent = text;
    if (stickToBottom) elements.resultView.scrollTop = elements.resultView.scrollHeight;
    elements.copyButton.disabled = !text;
    elements.downloadButton.disabled = !latestResult?.assembly;
    updateInteractiveControls();

    if (!latestResult) return;
    const duration = (latestResult.compiler?.durationMs || 0) + (latestResult.execution?.durationMs || 0);
    const arch = latestResult.architecture?.toUpperCase() || selectedArchitecture().toUpperCase();
    const state = latestResult.execution?.running ? "running" : "ready";
    elements.resultMeta.textContent = `${arch} · ${latestResult.optimise ? "optimised" : "unoptimised"} · ${state} · ${duration} ms`;
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

  function compilePayload(run) {
    syncOptimisationAvailability();
    return {
      source: elements.sourceEditor.value,
      architecture: selectedArchitecture(),
      optimise: effectiveOptimise(),
      run,
      stdin: elements.stdinInput.value,
    };
  }

  async function compile(run) {
    if (run) {
      await startInteractiveRun();
      return;
    }

    setBusy(true, run);
    try {
      const result = await compileProgram(compilePayload(run));
      latestResult = result;
      updateResultBadge(result, "compile");

      if (!result.ok) switchTab(result.phase === "run-environment" ? "output" : "log");
      else switchTab("assembly");
    } catch (error) {
      latestResult = failureResult(error);
      elements.resultBadge.className = "result-badge failure";
      elements.resultBadge.textContent = "Failed";
      switchTab("log");
    } finally {
      setBusy(false, run);
      renderResult();
    }
  }

  function failureResult(error) {
    return {
      ok: false,
      message: error.message,
      compiler: { stdout: "", stderr: "", durationMs: 0 },
      assembly: "",
      execution: null,
      architecture: selectedArchitecture(),
      optimise: effectiveOptimise(),
    };
  }

  function finishRunIfNeeded(result) {
    if (result.execution?.running) return false;
    activeRunId = null;
    stopRunPolling();
    setBusy(false, false);
    return true;
  }

  async function pollActiveRun() {
    if (!activeRunId || pollingRun) return;
    pollingRun = true;
    const id = activeRunId;

    try {
      const result = await fetchRun(id);
      if (id !== activeRunId) return;

      latestResult = result;
      updateResultBadge(result, "run");
      finishRunIfNeeded(result);
      renderResult();
    } catch (error) {
      stopRunPolling();
      if (id === activeRunId) {
        activeRunId = null;
        showToast(error.message);
        setBusy(false, false);
        renderResult();
      }
    } finally {
      pollingRun = false;
    }
  }

  function startRunPolling() {
    stopRunPolling();
    pollActiveRun();
    runPollTimer = setInterval(pollActiveRun, 350);
  }

  async function startInteractiveRun() {
    setBusy(true, true);
    try {
      const result = await startRun(compilePayload(true));
      latestResult = result;
      updateResultBadge(result, "run");
      if (result.sessionId && result.execution?.running) {
        activeRunId = result.sessionId;
        switchTab("output");
        startRunPolling();
        setTimeout(() => elements.interactiveInput.focus(), 0);
      } else {
        finishRunIfNeeded(result);
        if (!result.ok) switchTab(result.phase === "run-environment" ? "output" : "log");
        else switchTab("output");
      }
    } catch (error) {
      latestResult = failureResult(error);
      activeRunId = null;
      stopRunPolling();
      elements.resultBadge.className = "result-badge failure";
      elements.resultBadge.textContent = "Failed";
      switchTab("log");
    } finally {
      setBusy(false, true);
      renderResult();
    }
  }

  async function sendInteractiveInput() {
    if (!activeRunId) return;
    const input = elements.interactiveInput.value;
    elements.interactiveInput.value = "";
    elements.sendInputButton.disabled = true;

    try {
      const result = await sendRunInput(activeRunId, { input });
      latestResult = result;
      updateResultBadge(result, "run");
      finishRunIfNeeded(result);
      renderResult();
    } catch (error) {
      showToast(error.message);
      renderResult();
    } finally {
      updateInteractiveControls();
      if (!elements.interactiveInput.disabled) elements.interactiveInput.focus();
    }
  }

  async function stopActiveRun() {
    if (!activeRunId) return;
    const id = activeRunId;
    elements.stopRunButton.disabled = true;

    try {
      const result = await stopRun(id);
      if (id !== activeRunId) return;

      latestResult = result;
      updateResultBadge(result, "run");
      if (result.execution?.running) startRunPolling();
      else finishRunIfNeeded(result);
      renderResult();
    } catch (error) {
      showToast(error.message);
      elements.stopRunButton.disabled = false;
    }
  }

  function downloadAssembly() {
    if (!latestResult?.assembly) return;
    const blob = new Blob([latestResult.assembly], { type: "text/plain" });
    const link = document.createElement("a");
    link.href = URL.createObjectURL(blob);
    link.download = `${elements.sourceTitle.textContent.replace(/\.wacc$/i, "") || "program"}.s`;
    link.click();
    URL.revokeObjectURL(link.href);
  }

  return {
    compile,
    currentTabText,
    downloadAssembly,
    renderResult,
    sendInteractiveInput,
    stopActiveRun,
    switchTab,
    syncOptimisationAvailability,
    updateOptimiseChoice() {
      if (!x86Selected()) lastSupportedOptimiseChoice = elements.optimiseInput.checked;
    },
  };
}
