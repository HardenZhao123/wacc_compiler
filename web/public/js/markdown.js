export function escapeHtml(value) {
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

export function renderMarkdown(markdown) {
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
