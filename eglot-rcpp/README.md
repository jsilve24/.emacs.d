# eglot-rcpp

`eglot-rcpp` is a small local Emacs package for mixed R and C/C++ package
development. It is meant for projects that have an R package root marked by
`DESCRIPTION` and C/C++ code under `src/`, `include/`, or `inst/include/`.

## What it does

- starts the missing companion Eglot server when you open either the R side or
  the C/C++ side of the same package
- installs a project-aware xref backend that merges local Eglot results with
  package-wide symbol fallbacks
- optionally augments `consult-eglot-symbols` so header declarations are still
  discoverable when `clangd` does not return them in `workspace/symbol`
- restricts symbol results to the current package root when `DESCRIPTION` is
  present
- teaches `project.el` and `projectile` to treat `DESCRIPTION` as a project
  root marker

## Requirements

- Emacs 29.1 or later
- `eglot`
- `languageserver` in R for the R server
- `clangd` for the C/C++ companion server
- `consult-eglot` if you want the symbol-search augmentation

## Installation

Add the directory to `load-path` and require the package:

```elisp
(add-to-list 'load-path (expand-file-name "eglot-rcpp" user-emacs-directory))
(require 'eglot-rcpp)
```

If you use this repository directly, that is usually all you need.

## Customization

The main user-facing options are:

- `eglot-rcpp-root-marker-file`
- `eglot-rcpp-r-modes`
- `eglot-rcpp-cpp-modes`
- `eglot-rcpp-source-directories`
- `eglot-rcpp-header-directories`
- `eglot-rcpp-source-extensions`
- `eglot-rcpp-header-extensions`
- `eglot-rcpp-auto-start-companion-servers`
- `eglot-rcpp-project-setup-retries`
- `eglot-rcpp-r-server-command`
- `eglot-rcpp-clangd-command`
- `eglot-rcpp-enable-project-root-markers`
- `eglot-rcpp-restrict-symbol-search-to-project`

## Notes

- The package prefers `DESCRIPTION` as the project root marker because that is a
  good default for R packages.
- The header fallback is intentionally heuristic. It is useful for navigation,
  but it is not a full parser.
- If symbol search feels too broad, keep
  `eglot-rcpp-restrict-symbol-search-to-project` enabled so only files inside
  the current package root contribute results.
- `consult-eglot-symbols` queries the package's running Eglot servers and adds
  a textual header fallback when `clangd` omits package headers.
- If `consult-eglot` is not installed, the core Eglot and xref behavior still
  works.

