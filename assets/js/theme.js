const NOTY_THEME_CONFIG = {
  // Primary theme names (must match CSS filenames in assets/css/)
  lightPureTheme: "modus-operandi",
  darkPureTheme: "modus-vivendi",

  // Optional variants for later use
  lightTheme: "gruvbox-light",
  darkTheme: "gruvbox-dark",

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


/* ============================
   emacs-noty Theme Logic
   ============================ */

(function () {
  const cfg = NOTY_THEME_CONFIG;
  const linkEl = document.getElementById("noty-theme");
  const THEME_KEY = cfg.storage.theme;
  const MODE_KEY = cfg.storage.mode;

  /* --- helpers --- */

  function themePath(name) {
    return "/assets/css/" + name + ".css";
  }

  function currentThemeName() {
    if (!linkEl) return null;
    const href = linkEl.getAttribute("href") || "";
    const m = href.match(/\/([^\/]+)\.css$/);
    return m ? m[1] : null;
  }

  function setThemeHref(name) {
    if (!linkEl || !name) return;
    const href = themePath(name);
    if (linkEl.getAttribute("href") !== href) {
      linkEl.setAttribute("href", href);
    }
    localStorage.setItem(THEME_KEY, name);
  }

  function systemPrefersDark() {
    return (
      window.matchMedia &&
      window.matchMedia("(prefers-color-scheme: dark)").matches
    );
  }

  function applyThemeForMode(mode) {
    let theme;
    switch (mode) {
      case "light":
        theme = cfg.lightTheme;
        break;
      case "dark":
        theme = cfg.darkTheme;
        break;
      case "paper":
        theme = cfg.paperTheme;
        break;
      case "auto":
      default:
        theme = systemPrefersDark() ? cfg.darkTheme : cfg.lightTheme;
    }
    setThemeHref(theme);
  }

  function updateButtonLabel(btn, mode) {
    if (!btn) return;
    const icons = {
      auto: "🌓",
      light: "☀",
      dark: "🌙",
      paper: "📄",
    };
    const titles = {
      auto: "Auto (follow system)",
      light: "Light (" + cfg.lightTheme + ")",
      dark: "Dark (" + cfg.darkTheme + ")",
      paper: "Paper / ink (" + cfg.paperTheme + ")",
    };
    btn.textContent = icons[mode] || "🌓";
    btn.title = titles[mode] || "Toggle theme";
  }

  function initialMode() {
    let mode = localStorage.getItem(MODE_KEY);

    if (!mode) {
      // Backward compatibility: map legacy stored theme → mode
      const legacy = localStorage.getItem(THEME_KEY);
      if (legacy === cfg.darkTheme || legacy === cfg.darkTinted) mode = "dark";
      else if (legacy === cfg.lightTheme || legacy === cfg.lightTinted)
        mode = "light";
      else if (legacy === cfg.paperTheme) mode = "paper";
      else mode = "auto";

      localStorage.setItem(MODE_KEY, mode);
    }

    if (!cfg.modeCycle.includes(mode)) {
      mode = "auto";
      localStorage.setItem(MODE_KEY, mode);
    }

    return mode;
  }

  /* --- init --- */

  document.addEventListener("DOMContentLoaded", function () {
    const btn = document.querySelector(".theme-toggle");
    let mode = initialMode();

    // Apply chosen mode
    applyThemeForMode(mode);
    updateButtonLabel(btn, mode);

    // React to system theme changes only when in auto mode
    const media = window.matchMedia("(prefers-color-scheme: dark)");
    if (media && media.addEventListener) {
      media.addEventListener("change", () => {
        if ((localStorage.getItem(MODE_KEY) || "auto") === "auto") {
          applyThemeForMode("auto");
        }
      });
    }

    // Theme toggle cycle
    if (btn) {
      btn.addEventListener("click", function () {
        const current = localStorage.getItem(MODE_KEY) || "auto";
        const idx = cfg.modeCycle.indexOf(current);
        const next = cfg.modeCycle[(idx + 1) % cfg.modeCycle.length];

        localStorage.setItem(MODE_KEY, next);
        applyThemeForMode(next);
        updateButtonLabel(btn, next);
      });
    }
  });
})();
