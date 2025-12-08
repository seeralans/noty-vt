// emacs-noty link-preview.js
// Hover over internal links like Eq. (3.2), Theorem 1.4, etc. to see a small preview.

(function () {
  function createPreviewElement() {
    let el = document.getElementById("noty-link-preview");
    if (!el) {
      el = document.createElement("div");
      el.id = "noty-link-preview";
      document.body.appendChild(el);
    }
    return el;
  }

  function hidePreview(preview) {
    preview.style.display = "none";
    preview.innerHTML = "";
  }

  function positionPreview(preview, x, y) {
    // Initial position near the cursor
    preview.style.left = x + 12 + "px";
    preview.style.top = y + 12 + "px";
    preview.style.display = "block";

    // Adjust if going off-screen
    const rect = preview.getBoundingClientRect();
    const margin = 8;

    let newX = rect.left;
    let newY = rect.top;

    if (rect.right > window.innerWidth - margin) {
      newX = window.innerWidth - rect.width - margin;
    }
    if (rect.bottom > window.innerHeight - margin) {
      newY = window.innerHeight - rect.height - margin;
    }

    preview.style.left = Math.max(margin, newX) + "px";
    preview.style.top = Math.max(margin, newY) + "px";
  }

  function findPreviewBlock(target) {
    if (!target) return null;

    // If the target itself is a block we care about, use it directly
    if (target.classList.contains("eq") && target.classList.contains("env")) {
      return target;
    }

    const block = target.closest(
      ".eq.env, " +
      ".theorem, .lemma, .proposition, .corollary, " +
      ".definition, .remark, .insight, .question, .note, .todo, " +
      "div.figure, figure, table"
    );

    return block || target;
  }

  function setupLinkPreviews() {
    const preview = createPreviewElement();

    // Hide on scroll or click anywhere
    window.addEventListener("scroll", () => hidePreview(preview), { passive: true });
    document.addEventListener("click", () => hidePreview(preview));

    const links = document.querySelectorAll(".content a[href^='#']");

    links.forEach((link) => {
      // Only internal anchors like #eq:foo
      const href = link.getAttribute("href");
      if (!href || href === "#" || href === "#top") return;

      link.addEventListener("mouseenter", (e) => {
        const id = href.slice(1);
        const target = document.getElementById(id);
        if (!target) {
          hidePreview(preview);
          return;
        }

        const block = findPreviewBlock(target);
        if (!block) {
          hidePreview(preview);
          return;
        }

        // Clone the block without affecting the original DOM
        const clone = block.cloneNode(true);

        // Clean out nested anchors to avoid weird interactions
        clone.querySelectorAll("a").forEach((a) => a.removeAttribute("href"));

        preview.innerHTML = "";
        preview.appendChild(clone);

        positionPreview(preview, e.clientX, e.clientY);
      });

      link.addEventListener("mouseleave", () => {
        hidePreview(preview);
      });
    });
  }

  document.addEventListener("DOMContentLoaded", setupLinkPreviews);
})();
