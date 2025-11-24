// code-toggle.js
// Collapse all code blocks by default; per-block toggle; global toggle.

(function () {
  function codeLanguageFromPre(pre) {
    // Org/ox-html emits: <pre class="src src-<lang>"> or wrapped by <div class="org-src-container">.
    const cls = (pre.getAttribute("class") || "").split(/\s+/);
    const found = cls.find(c => c.startsWith("src-"));
    return found ? found.replace(/^src-/, "") : "code";
  }

  function wrapPreInDetails(pre) {
    // If already wrapped, skip
    const container = pre.closest("details[data-noty-code]");
    if (container) return container;

    // Outer details + summary
    const details = document.createElement("details");
    details.setAttribute("data-noty-code", "1");
    details.className = "noty-code";
    // Closed by default -> collapsed
    details.open = false;

    const summary = document.createElement("summary");
    const lang = codeLanguageFromPre(pre);
    summary.innerHTML = `
      <span>Code</span>
      <span class="noty-code-pill">${lang}</span>
    `;

    // If pre is inside org-src-container, move the whole container; else move the pre itself
    const orgContainer = pre.closest(".org-src-container");
    const moveNode = orgContainer || pre;

    // Build structure
    details.appendChild(summary);
    moveNode.parentNode.insertBefore(details, moveNode);
    details.appendChild(moveNode);

    return details;
  }

  function processAllCodeBlocks(root) {
    const pres = root.querySelectorAll("pre.src, .org-src-container > pre.src");
    pres.forEach(wrapPreInDetails);
  }

  function setAll(detailsList, open) {
    detailsList.forEach(d => d.open = open);
  }

  function setupGlobalToggle() {
    const btn = document.querySelector(".code-toggle-all");
    if (!btn) return;

    // Track state: start as "all collapsed"
    let expanded = false;

    const detailsList = Array.from(document.querySelectorAll("details[data-noty-code]"));
    const updateLabel = () => { btn.textContent = expanded ? "⤴ code" : "⤵ code"; };

    btn.addEventListener("click", () => {
      expanded = !expanded;
      setAll(detailsList, expanded);
      updateLabel();
    });

    updateLabel();
  }

  document.addEventListener("DOMContentLoaded", function () {
    // 1) Wrap all code blocks
    processAllCodeBlocks(document);

    // 2) Global toggle button
    setupGlobalToggle();
  });

})();
