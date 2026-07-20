async function parseJsonResponse(response, fallbackMessage) {
  const result = await response.json();
  if (!response.ok) throw new Error(result.error || fallbackMessage);
  return result;
}

async function postJson(path, payload) {
  const response = await fetch(path, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(payload),
  });
  return parseJsonResponse(response, "Request failed");
}

export async function fetchHealth() {
  const response = await fetch("/api/health");
  return parseJsonResponse(response, "Could not load environment status");
}

export async function listExamples() {
  const response = await fetch("/api/examples");
  return parseJsonResponse(response, "Could not load examples");
}

export async function loadExample(id) {
  const response = await fetch(`/api/examples/${encodeURIComponent(id)}`);
  return parseJsonResponse(response, "Could not load example");
}

export function compileProgram(payload) {
  return postJson("/api/compile", payload);
}

export function startRun(payload) {
  return postJson("/api/runs", payload);
}

export async function fetchRun(id) {
  const response = await fetch(`/api/runs/${encodeURIComponent(id)}`);
  return parseJsonResponse(response, "Could not refresh program output");
}

export function sendRunInput(id, payload) {
  return postJson(`/api/runs/${encodeURIComponent(id)}/input`, payload);
}

export async function stopRun(id) {
  const response = await fetch(`/api/runs/${encodeURIComponent(id)}`, { method: "DELETE" });
  return parseJsonResponse(response, "Could not stop program");
}

export async function loadSpecificationText() {
  const response = await fetch("/wacc-language-spec.md");
  if (!response.ok) throw new Error("Could not load the language specification.");
  return response.text();
}
