export const elements = {
  compileButton: document.querySelector("#compileButton"),
  copyButton: document.querySelector("#copyButton"),
  downloadButton: document.querySelector("#downloadButton"),
  emptyState: document.querySelector("#emptyState"),
  environmentStatus: document.querySelector("#environmentStatus"),
  exampleSelect: document.querySelector("#exampleSelect"),
  fileInput: document.querySelector("#fileInput"),
  interactiveInput: document.querySelector("#interactiveInput"),
  interactiveInputForm: document.querySelector("#interactiveInputForm"),
  lineNumbers: document.querySelector("#lineNumbers"),
  optimiseInput: document.querySelector("#optimiseInput"),
  resultBadge: document.querySelector("#resultBadge"),
  resultMeta: document.querySelector("#resultMeta"),
  resultView: document.querySelector("#resultView"),
  runButton: document.querySelector("#runButton"),
  sendInputButton: document.querySelector("#sendInputButton"),
  sourceEditor: document.querySelector("#sourceEditor"),
  sourceTitle: document.querySelector("#sourceTitle"),
  specButton: document.querySelector("#specButton"),
  specCloseButton: document.querySelector("#specCloseButton"),
  specContent: document.querySelector("#specContent"),
  specCopyButton: document.querySelector("#specCopyButton"),
  specDialog: document.querySelector("#specDialog"),
  stdinInput: document.querySelector("#stdinInput"),
  stopRunButton: document.querySelector("#stopRunButton"),
  toast: document.querySelector("#toast"),
};

export function selectedArchitecture() {
  return document.querySelector('input[name="architecture"]:checked').value;
}
