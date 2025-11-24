\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{notes}[2025/03/31 Custom class for lecture or study notes]

\LoadClass[11pt,a4paper]{article}

% Packages for formatting and layout
\RequirePackage[utf8]{inputenc}
\RequirePackage[T1]{fontenc}
\RequirePackage{lmodern}
\RequirePackage{geometry}
\geometry{
  top=2cm,
  bottom=2cm,
  left=2cm,
  right=2cm
}
\RequirePackage{setspace}
\onehalfspacing

% Header/Footer
\RequirePackage{fancyhdr}
\pagestyle{fancy}
\fancyhf{}
\fancyhead[L]{\leftmark}
\fancyhead[R]{\thepage}
\renewcommand{\headrulewidth}{0.4pt}
\renewcommand{\footrulewidth}{0pt}

% Section formatting
\RequirePackage{titlesec}
\titleformat{\section}{\Large\bfseries}{\thesection}{1em}{}
\titleformat{\subsection}{\large\bfseries}{\thesubsection}{1em}{}
\titleformat{\subsubsection}{\normalsize\bfseries}{\thesubsubsection}{1em}{}

% Optional packages
\RequirePackage{graphicx}
\RequirePackage{amsmath, amssymb}
\RequirePackage{enumitem}
\setlist{itemsep=4pt, topsep=6pt}

% Hyperlinks (nice colours)
\RequirePackage[colorlinks=true, linkcolor=blue, urlcolor=teal, citecolor=blue]{hyperref}

% Title setup
\newcommand{\notestitle}[1]{%
  \title{\bfseries #1\vspace{-1ex}}
  \author{}
  \date{}
}

\endinput
