// Simple theme + siden// Theme toggle, sidenote panel toggle, TOC builder, margin-notes panel
(function () {
  const root = document.documentElement;
  const btnTheme = document.querySelector('.theme-toggle');
  const btnNotes = document.querySelector('.sidenote-toggle');

  // --- Theme toggle + persistence ---
  if (localStorage.getItem('theme-dark') === '1') {
    root.classList.add('dark');
  }
  if (btnTheme) {
    btnTheme.addEventListener('click', () => {
      root.classList.toggle('dark');
      localStorage.setItem('theme-dark', root.classList.contains('dark') ? '1' : '0');
    });
  }

  // --- Ensure layout containers exist ---
  const pageEl = document.querySelector('.page');
  const tocEl  = document.getElementById('toc');
  const mainEl = document.querySelector('.content');

  // --- Build nested TOC (h2 -> h3) ---
  if (tocEl && mainEl) {
    const h2s = Array.from(mainEl.querySelectorAll('h2'));
    const wrap = document.createElement('div');
    const title = document.createElement('h3');
    title.textContent = 'On this page';
    const list = document.createElement('ul');

    // helper to ensure headings have ids
    const ensureId = (el) => {
      if (!el.id) {
        const base = el.textContent.trim().toLowerCase().replace(/\s+/g, '-').replace(/[^\w-]/g, '');
        let id = base || 'section';
        let n = 2;
        while (document.getElementById(id)) id = `${base}-${n++}`;
        el.id = id;
      }
      return el.id;
    };

    h2s.forEach(h2 => {
      const h2Id = ensureId(h2);
      const li2 = document.createElement('li');
      const a2 = document.createElement('a');
      a2.href = `#${h2Id}`;
      a2.textContent = h2.textContent;
      li2.appendChild(a2);

      // collect following h3s until next h2
      const sub = document.createElement('ul');
      let sib = h2.nextElementSibling;
      while (sib && sib.tagName !== 'H2') {
        if (sib.tagName === 'H3') {
          const h3Id = ensureId(sib);
          const li3 = document.createElement('li');
          const a3 = document.createElement('a');
          a3.href = `#${h3Id}`;
          a3.textContent = sib.textContent;
          li3.appendChild(a3);
          sub.appendChild(li3);
        }
        sib = sib.nextElementSibling;
      }
      if (sub.childNodes.length) li2.appendChild(sub);
      list.appendChild(li2);
    });

    if (list.childNodes.length) {
      wrap.appendChild(title);
      wrap.appendChild(list);
      tocEl.appendChild(wrap);
    }
  }

  // --- Create right margin-notes panel and mirror sidenotes there ---
  let notesPanel = document.getElementById('notes-panel');
  if (!notesPanel && pageEl) {
    notesPanel = document.createElement('aside');
    notesPanel.id = 'notes-panel';
    const head = document.createElement('div');
    head.className = 'notes-title';
    head.textContent = 'Notes';
    notesPanel.appendChild(head);
    // place after main content to match grid: [toc | content | notes]
    pageEl.appendChild(notesPanel);
  }

  if (notesPanel && mainEl) {
    const notes = mainEl.querySelectorAll('.sidenote, .marginnote');
    notes.forEach((n, i) => {
      const box = document.createElement('div');
      box.className = n.classList.contains('marginnote') ? 'marginnote' : 'sidenote';
      // copy content
      box.innerHTML = n.innerHTML;
      // optional numbering badge
      const badge = document.createElement('span');
      badge.style.fontWeight = '700';
      badge.style.marginRight = '.5rem';
      badge.textContent = `[${i + 1}]`;
      box.prepend(badge);
      notesPanel.appendChild(box);
    });
  }

  // --- Notes toggle ---
  if (btnNotes && notesPanel) {
    btnNotes.addEventListener('click', () => {
      notesPanel.hidden = !notesPanel.hidden;
    });
  }

  // --- Smooth scroll for local TOC links (optional, non-invasive) ---
  if (tocEl) {
    tocEl.addEventListener('click', (e) => {
      const a = e.target.closest('a[href^="#"]');
      if (!a) return;
      const target = document.querySelector(a.getAttribute('href'));
      if (target) {
        e.preventDefault();
        target.scrollIntoView({ behavior: 'smooth', block: 'start' });
        history.replaceState(null, '', a.getAttribute('href'));
      }
    });
  }
})();


// Inject local TeX macros for each page from a <div class="math_custom" data-macros="...">
(function () {
  const blocks = document.querySelectorAll('.math_custom[data-macros]');
  if (!blocks.length) return;

  // Resolve a relative path against the current page URL
  function resolvePath(rel) {
    try { return new URL(rel, window.location.href).toString(); }
    catch { return rel; }
  }

  blocks.forEach(div => {
    const rel = div.getAttribute('data-macros');
    if (!rel) return;
    const url = resolvePath(rel);

    fetch(url)
      .then(r => {
        if (!r.ok) throw new Error(`HTTP ${r.status}`);
        return r.text();
      })
      .then(tex => {
        // Put macros inside display math to define them globally for MathJax v3
        div.innerHTML = '$$\n' + tex + '\n$$';
        if (window.MathJax && MathJax.typesetPromise) {
          MathJax.typesetPromise([div]).catch(console.error);
        }
      })
      .catch(err => {
        console.error('Failed to load local TeX macros:', url, err);
        // Optional: leave a visible hint while developing
        // div.textContent = `⚠️ Could not load macros from ${rel}`;
      });
  });
})();

document.addEventListener('DOMContentLoaded', () => {
  const toc = document.getElementById('table-of-contents');
  if (!toc) return;

  // Where to move it
  let left = document.querySelector('.left-bar');
  if (!left) {
    // Create the left bar if it doesn't exist
    left = document.createElement('aside');
    left.className = 'left-bar';
    const page = document.querySelector('.page') || document.body;
    page.insertBefore(left, page.firstChild);
  }

  // Optional: remove the "Table of Contents" title
  const h2 = toc.querySelector('h2');
  if (h2) h2.remove();

  // Move it into the sidebar
  left.innerHTML = '';          // clear any stub content
  toc.id = 'sidebar-toc';       // give it a new id if you like
  toc.classList.add('toc');
  left.appendChild(toc);
});
