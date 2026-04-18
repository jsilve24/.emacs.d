# eglot-rcpp

`eglot-rcpp` is an Emacs package for mixed R / Rcpp / C++ package development.
It is designed for ordinary R package projects with a `DESCRIPTION` file at the
root, R code under `R/`, and native code under `src/`, `include/`, or
`inst/include/`.

This package stays Eglot-native. It does not build a combined language server,
does not redesign the workflow around ESS, and does not make LSP optional.
Core mixed-project support depends on:

- Eglot
- the R `languageserver` package
- `clangd`

## What it does

- activates only in relevant R package projects
- uses the R language server for R buffers and `clangd` for C/C++ buffers
- keeps those servers coordinated inside one package project
- makes mixed-project xref the centerpiece:
  - go to definition
  - find references
  - search symbols
- supplements R completion with functions exposed through generated
  `RcppExports` bridge files
- treats generated bridge files as useful for completion and discovery
- deprioritizes generated bridge files as definition targets by default when
  real source locations exist

## Requirements

Required:

- Emacs 29.1 or later
- Eglot
- R with `languageserver`
- `clangd`

Optional:

- ESS
- `usethis`
- `Rcpp`
- `consult-eglot`, if you still want its normal command set alongside this
  package's xref-backed symbol search. For package-scoped symbol search,
  prefer `eglot-rcpp-consult-symbols`.

## Installation

This package is intended to live in your local `.emacs.d` tree.

```elisp
(add-to-list 'load-path (expand-file-name "eglot-rcpp" user-emacs-directory))
(require 'eglot-rcpp)
```

Requiring the package installs its hooks. Actual activation is still gated by
project checks, so unrelated buffers are left alone.

## Setup

Minimal setup:

```elisp
(require 'eglot-rcpp)
```

Optional customization:

```elisp
(setq eglot-rcpp-enable-ess-keybindings t)
```

The main user options are:

- `eglot-rcpp-root-marker-file`
- `eglot-rcpp-r-modes`
- `eglot-rcpp-cpp-modes`
- `eglot-rcpp-source-directories`
- `eglot-rcpp-header-directories`
- `eglot-rcpp-auto-start-companion-servers`
- `eglot-rcpp-r-server-command`
- `eglot-rcpp-clangd-command`
- `eglot-rcpp-restrict-xref-results-to-project`
- `eglot-rcpp-generated-file-regexps`
- `eglot-rcpp-generated-definition-policy`
- `eglot-rcpp-enable-ess-keybindings`

## Generated File Behavior

Generated Rcpp bridge files are part of the workflow here.

- `R/RcppExports.R` is used to improve R-side completion and discovery.
- `src/RcppExports.cpp` remains available as a fallback symbol source.
- xref definition results do not prefer those generated files over real source
  by default.
- Reference results keep real package code ahead of generated bridge hits.

Definition ranking is controlled by
`eglot-rcpp-generated-definition-policy`:

- `deprioritize`: real source first, generated files last
- `omit`: hide generated hits only when at least one non-generated hit exists
- `keep`: keep generated hits in normal deduped order

## Commands

Core commands:

- `eglot-rcpp-find-symbol`
- `eglot-rcpp-consult-symbols`
- `eglot-rcpp-invalidate-project-cache`

Onboarding and maintenance helpers:

- `eglot-rcpp-use-rcpp`
  - runs `usethis::use_rcpp()`
  - intended for R-only packages that you want to convert into an Rcpp workflow
- `eglot-rcpp-compile-attributes`
  - runs `Rcpp::compileAttributes()`
  - regenerates bridge files only
  - does not compile the whole package
- `eglot-rcpp-check-r-dependencies`
  - checks for `languageserver`, `Rcpp`, and `usethis`
  - can optionally prompt to install missing R packages

## ESS Integration

ESS is optional. The package does not depend on ESS for its architecture.

When `eglot-rcpp-enable-ess-keybindings` is non-nil, ESS R buffers gain a
small convenience prefix on `C-c C-e`:

- `u` for `eglot-rcpp-use-rcpp`
- `c` for `eglot-rcpp-compile-attributes`
- `d` for `eglot-rcpp-check-r-dependencies`

## Limitations

- True cross-language rename is not implemented yet.
- Rename remains whatever the current language server supports in the current
  buffer.
- Inline C++ inside R strings, such as `Rcpp::sourceCpp(code = ...)`, is not
  transparently multi-server aware yet.
- Symbol search and xref merge project servers conservatively.
- References use the current server as the base and add a conservative
  package-code textual fallback; they are not a fully semantic multi-server
  cross-language index.
- `consult-eglot-symbols` still reflects raw server workspace symbols; use
  `eglot-rcpp-find-symbol` or `eglot-rcpp-consult-symbols` for package-scoped
  results.

## Testing

The test suite covers package logic only:

- root and file discovery
- generated-file ranking and definition policies
- project-scoped xref filtering
- xref deduplication
- mixed-project reference fallback
- Rcpp export completion merging
- companion startup decisions
- textual symbol extraction
- cache invalidation
- dependency helper logic

No test requires a real `languageserver` process or a real `clangd` instance.
