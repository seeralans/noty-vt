window.MathJax = {
  tex: {
    tags: 'ams',
    processRefs: true,
    useLabelIds: true,
    inlineMath: [['$', '$'], ['\\(', '\\)']],
    displayMath: [['$$', '$$'], ['\\[', '\\]']],
    macros: {
      RR: "{\\mathbb{R}}",
      EE: "{\\mathbb{E}}",
      PP: "{\\mathbb{P}}",
      floor: ["{\\lfloor #1 \\rfloor}", 1],
      abs: ["{\\lvert #1\\rvert}", 1],
      norm: ["{\\lVert #1\\rVert}", 1],
      inner: ["{\\langle #1, #2\\rangle}", 2],
      argmax: "{\\mathop{arg\\,max}}",
      argmin: "{\\mathop{arg\\,min}}"
    }
  },
  svg: { fontCache: 'global' }
};
