(function () {
  // key per-path so different pages don't fight
  const KEY = "scroll-pos:" + location.pathname;

  // Save on any navigation/reload
  window.addEventListener("beforeunload", function () {
    try {
      sessionStorage.setItem(
        KEY,
        JSON.stringify({ x: window.scrollX, y: window.scrollY })
      );
    } catch (_) {}
  });

  // Restore once everything is loaded (images/fonts)
  function restoreNow() {
    try {
      const raw = sessionStorage.getItem(KEY);
      if (!raw) return;
      const pos = JSON.parse(raw);
      if (!pos) return;

      // If user is on an anchor (e.g. #eq-1), let the anchor win
      if (!location.hash) {
        window.scrollTo(pos.x || 0, pos.y || 0);
      }
    } catch (_) {}
  }

  // Try after load, then again after MathJax/typesetting settles
  window.addEventListener("load", function () {
    restoreNow();

    // If MathJax v3 is present, wait for it to finish then restore again
    if (window.MathJax && MathJax.typesetPromise) {
      MathJax.typesetPromise().then(function () {
        // Give layout a tick to settle
        setTimeout(restoreNow, 0);
      });
    } else {
      // Fallback: a gentle second attempt
      setTimeout(restoreNow, 200);
    }
  });
})();
