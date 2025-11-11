(function () {
  // Adjust this path to where your custom .tex lives
  const MACROS_TEX = '/assets/tex/customcommands.tex';

  function addMacrosToMathJax(macros) {
    if (!window.MathJax || !MathJax.config || !MathJax.config.tex) return;
    Object.assign(MathJax.config.tex.macros, macros);
  }

  function parseNewCommands(tex) {
    const macros = {};

    // \newcommand{\foo}[2]{ ...#1...#2... }
    // \renewcommand{\foo}[1]{ ... }
    const reNew = /\\(?:newcommand|renewcommand)\s*{\\(\w+)}(?:\[(\d+)\])?\s*{([^}]*)}/g;
    let m;
    while ((m = reNew.exec(tex)) !== null) {
      const [, name, nArgs, body] = m;
      macros[name] = nArgs ? [body, parseInt(nArgs, 10)] : body;
    }

    // \DeclareMathOperator{\Foo}{\mathrm{Foo}}  -> \Foo → \operatorname{Foo}(#1…)
    // MathJax doesn’t implement DeclareMathOperator, so convert common cases:
    const reDecl = /\\DeclareMathOperator\s*{\\(\w+)}\s*{([^}]*)}/g;
    while ((m = reDecl.exec(tex)) !== null) {
      const [, name, opBody] = m;
      // treat as a 1-arg macro that prints as an operator with argument:
      // usage: \Foo x  -> \operatorname{Foo} x
      // If you want no-arg operator, drop the arg count.
      macros[name] = ['\\operatorname{' + opBody + '}', 0];
    }

    return macros;
  }

  fetch(MACROS_TEX)
    .then(res => res.text())
    .then(tex => {
      // Strip comments (simple, line-based)
      tex = tex.replace(/(^|[^\\])%.*/g, '$1');
      // Basic parse of supported commands
      const macros = parseNewCommands(tex);
      addMacrosToMathJax(macros);

      // If MathJax already loaded, re-typeset
      if (window.MathJax && MathJax.typesetPromise) {
        MathJax.typesetPromise().catch(console.error);
      }
    })
    .catch(err => console.error('Failed to load custom TeX macros:', err));
})();
