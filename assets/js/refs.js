(function () {
  document.addEventListener('DOMContentLoaded', function () {
    const root = document;

    // Abbrev map for theorem-like blocks
    const ABBR = {
      theorem: 'Theorem',
      lemma: 'Lemma',
      proposition: 'Proposition',
      corollary: 'Corollary',
      definition: 'Definition',
      question: 'Question',
      insight: 'Insight',
      note: 'Note',
      todo: 'TODO',
      remark: 'Remark'
    };

    // Global map of id -> human label (so we can relabel <a> later)
    const idToLabel = new Map();

    // Helpers
    const getH2SectionNumber = (outlineDiv) => {
      // org exports: <div class="outline-2"><h2> <span class="section-number-2">1.</span> Title</h2>...
      const h2 = outlineDiv.querySelector(':scope > h2 .section-number-2');
      if (!h2) return null;
      const raw = (h2.textContent || '').trim(); // "1." or "2."
      const sect = raw.replace(/\.$/, '');       // "1" or "2"
      return sect || null;
    };

    const ensureLabelBlock = (el, text, className) => {
      if (el.querySelector('.theoremish-title, .theorem-label, .figure-title, .table-title, .eq-number')) return;
      const d = root.createElement('div');
      d.className = className;
      d.textContent = text;
      el.insertBefore(d, el.firstChild);
    };

    // Process a single section container, resetting counters
    const processOneSection = (container) => {
      const sectNo = getH2SectionNumber(container); // e.g. "1"
      // per-section counters
      const counters = {
          theorem: 0, lemma: 0, proposition: 0, question: 0, insight: 0, corollary: 0, definition: 0, remark: 0, note: 0, todo: 0,
        eq: 0, figure: 0, table: 0
      };

      // 1) Theorem-ish blocks inside this section
      Object.keys(ABBR).forEach(type => {
        container.querySelectorAll(`div.${type}[id]`).forEach(el => {
          const n = ++counters[type];
          const label = sectNo ? `${ABR(type)} ${sectNo}.${n}` : `${ABR(type)} ${n}`;
          ensureLabelBlock(el, label, `${type}-label`);
          idToLabel.set(el.id, label);
        });
      });

      // 2) Equations (<div class="eq env" id="...">)
      container.querySelectorAll('div.eq.env[id]').forEach(el => {
        const k = ++counters.eq;
        const label = sectNo ? `Eq. (${sectNo}.${k})` : `Eq. (${k})`;
        const num = root.createElement('span');
        num.className = 'eq-number';
        num.textContent = sectNo ? `(${sectNo}.${k})` : `(${k})`;
        el.appendChild(num);
        idToLabel.set(el.id, label);
      });

      // 3) Figures (<div class="figure" id="...">)
      container.querySelectorAll('div.figure[id]').forEach(el => {
        const k = ++counters.figure;
        const label = sectNo ? `Fig. ${sectNo}.${k}` : `Fig. ${k}`;
        const hasCaption = !!el.querySelector('.caption, .figure-number, figcaption, .figure-title');
        if (!hasCaption) {
          const cap = root.createElement('div');
          cap.className = 'figure-title';
          cap.textContent = label;
          el.insertBefore(cap, el.firstChild);
        }
        idToLabel.set(el.id, label);
      });

      // 4) Tables (<table id="...">)
      container.querySelectorAll('table[id]').forEach(el => {
        const k = ++counters.table;
        const label = sectNo ? `Table ${sectNo}.${k}` : `Table ${k}`;
        let cap = el.querySelector(':scope > caption');
        if (!cap) {
          cap = root.createElement('caption');
          cap.textContent = label;
          el.insertBefore(cap, el.firstChild);
        }
        idToLabel.set(el.id, label);
      });
    };

    // Turn "theorem" into "Thm.", etc.
    function ABR(type) { return ABBR[type] || type; }

    // Process every top-level section in order
    const topSections = root.querySelectorAll('div.outline-2');
    if (topSections.length) {
      topSections.forEach(processOneSection);
    } else {
      // Fallback: single “section-less” page (reset counters once)
      processOneSection(root.querySelector('#content') || root);
    }

    // Rewrite in-document links to use our labels when appropriate
    const shouldReplaceText = (txt, id) => {
      const t = (txt || '').trim();
      if (t === '' || t === id) return true;
      if (/^\s*\[BROKEN LINK:/i.test(t)) return true;
      if (/^No description for this link/i.test(t)) return true;
      return false;
    };

    root.querySelectorAll('.content a[href^="#"]').forEach(a => {
      try {
        const targetId = decodeURIComponent(a.getAttribute('href').slice(1));
        if (!targetId) return;
        const label = idToLabel.get(targetId);
        if (!label) return;
        if (shouldReplaceText(a.textContent, targetId)) {
          a.textContent = label;
        }
        if (!a.getAttribute('title')) a.setAttribute('title', label);
      } catch (_) { /* ignore malformed */ }
    });
  });
})();

function setupLinkPreviews() {
  const links = document.querySelectorAll('.content a[href^="#"]');
  // create global preview element once
  // attach mouseenter/mouseleave handlers
}
