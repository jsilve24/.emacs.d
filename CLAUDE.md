# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

For the human-oriented maintenance overview, see `MAINTENANCE.md`.

## Overview

This is a personal Emacs configuration using **straight.el** for reproducible package management, **use-package** for package configuration, **Evil mode** for Vim-style editing, and **general.el** for keybinding management.

## Architecture

### Entry Points

- `early-init.el` — Minimal; disables `package-enable-at-startup` so straight.el takes over
- `init.el` — Main entry point; sets feature flags, then loads all modules in order
- `core.el` — Bootstraps straight.el, use-package, and general.el; must load first

### Module Loading Order (init.el)

The load order in `init.el` is intentional and has dependencies:
1. `core.el` — straight/use-package/general (foundation)
2. `evil.el` — Vim keybindings (before UI)
3. `org.el` — **Must load before themes and completing-read** (other packages depend on it)
4. UI layer: themes, popups, completing-read
5. Tools: projects, git, term, window, etc.
6. `bindings.el` — **Must load after all packages** (references keymaps defined elsewhere)
7. Languages: python, ess, stan (after LSP and treesitter)

### Keybinding System (general.el)

Four definers are created in `core.el`:
- `jds/leader-def` — `SPC` (global: `C-SPC`) — main commands
- `jds/sub-leader-def` — `,` (global: `C-,`) — secondary commands
- `jds/localleader-def` — `m` — mode-specific bindings
- `jds/sub-localleader-def` — `\` (global: `C-\`) — mode-specific secondary

### Package Management Pattern

All packages use straight.el with use-package. Custom sources use:
```elisp
(use-package pkg
  :straight (:type git :host github :repo "user/repo"))
```
`straight-use-package-by-default t` means `:straight t` is implied unless overridden.

### Feature Flags

Set before module loading in `init.el`:
- `jds~use-wm` — load `wm.el` (EXWM window manager); enable with `--use-exwm` CLI arg
- `jds~skip-email` — skip email module loading

### Autoloads Directory

`autoloads/` contains lazily-loaded helper functions. Files here define `;;;###autoload` functions and are loaded explicitly in `init.el` after their dependencies.

### Key Packages

- **Completion**: vertico + consult + embark + orderless (in `completing-read.el`)
- **Projects**: projectile + consult-projectile (in `projects.el`)
- **Git**: magit (in `git.el`)
- **LSP**: `lsp.el` + `treesitter.el`
- **Org**: org-mode + org-roam; files live in `~/Dropbox/org/`
- **AI**: gptel + claude-code-ide + local `gptel-reinforce` development (in `ai.el` and `gptel-reinforce/`)
- **Languages**: ESS/R (`ess.el`), Python (`python.el`), Stan (`stan.el`), LaTeX (`latex.el`)
- **Snippets**: yasnippet; templates in `snippets/` organized by major mode

### Data Directories

- `var/` — runtime data (auto-save, bookmarks, history, org cache)
- `etc/` — configuration data (abbrev, eshell, yasnippet, transient state)
- `straight/` — package sources and build cache (do not edit manually)
- `eln-cache/` — native compilation cache (auto-generated)

## Adding or Modifying Configuration

- New packages belong in the most relevant existing module file, not new files
- Keybindings go in the relevant module's `use-package` block or in `bindings.el` for global bindings
- Mode-specific bindings use `jds/localleader-def` with `:keymaps 'some-mode-map`
- Custom helper functions intended for use across modules should live in a dedicated `*-helpers.el` module with `;;;###autoload`
- The `custom-set-variables` block at the bottom of `init.el` is managed by Emacs Custom — avoid editing it manually
