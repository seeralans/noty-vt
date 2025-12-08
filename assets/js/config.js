const NOTY_THEME_CONFIG = {
  // Primary theme names (must match CSS filenames in assets/css/)
  lightPureTheme: "modus-operandi",
  darkPureTheme: "modus-vivendi",

  // Optional variants for later use
  lightTheme: "modus-operandi-tinted",
  darkTheme: "modus-vivendi-tinted",

  // Raw “paper ink” theme (LaTeX black-on-white style)
  paperTheme: "paper-ink",

  // Default cycle order for the theme button
  modeCycle: ["auto", "light", "dark", "paper"],

  // Storage keys
  storage: {
    mode: "noty-theme-mode",   // stores "auto" | "light" | "dark" | "paper"
    theme: "noty-theme",       // stores actual CSS name for backwards compat
  },
};

