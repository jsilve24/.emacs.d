# Maintenance Guide

This config has grown into a modular Emacs setup with a few locally-developed subsystems. The goal of this file is to make future edits cheap: know where things live, what depends on what, and what to test after changes.

## Load Order

`init.el` is intentionally ordered. The broad phases are:

1. `core.el`
   Provides `straight.el`, `use-package`, and the `general.el` keybinding definers.
2. UI and editor foundations
   `evil.el`, `org.el`, `themes.el`, `popups.el`, `completing-read.el`, then general editing/navigation modules.
3. Global bindings
   `bindings.el` loads after packages so referenced keymaps and commands already exist.
4. Deeper integrations
   `debugging.el`, `references.el`, `org-roam.el`, `lsp.el`, `treesitter.el`, `engines.el`, `ai.el`.
5. Language modules
   `python.el`, `ess.el`, `stan.el`, snippets, then optional applications like `bitwarden.el` and `elfeed.el`.

When adding a new module, prefer placing it in the right phase instead of appending it arbitrarily.

## Keybinding Model

Bindings are centralized in `bindings.el` unless they are highly mode-local.

- `jds/leader-def`: `SPC` and `C-SPC`
- `jds/sub-leader-def`: `,` and `C-,`
- `jds/localleader-def`: `m`
- `jds/sub-localleader-def`: `\` and `C-\`

The AI prefix is under `SPC d`. Current `gptel-reinforce` workflow bindings:

- `SPC d +`: positive feedback
- `SPC d -`: negative feedback
- `SPC d 0`: neutral feedback
- `SPC d S`: summarize artifact feedback
- `SPC d U`: update artifact from summary
- `SPC d B`: rollback artifact from history

## AI Stack

`ai.el` currently owns four distinct layers:

1. `gptel`
   General chat and rewrite helpers.
2. Local `gptel-reinforce`
   An alpha package in `gptel-reinforce/` for evolving text artifacts from feedback.
3. `agent-shell`
   Coding-agent terminal workflow with Evil integration.
4. `claude-code-ide`
   Tool-based Claude workflow, including project grep and ESS/R integration.

Keep these concerns separate. If a change is about artifact history or review flow, it belongs in `gptel-reinforce/`, not in the general `gptel` helpers.

## gptel-reinforce Layout

Files in `gptel-reinforce/` are split by responsibility:

- `gptel-reinforce-core.el`: structs, registration, state paths, provenance helpers
- `gptel-reinforce-db.el`: SQLite feedback storage
- `gptel-reinforce-org.el`: `current.org`, `summary.org`, and history snapshot IO
- `gptel-reinforce-backend.el`: request structs and LLM transport abstraction
- `gptel-reinforce-ui.el`: interactive commands and review UX
- `gptel-reinforce-elfeed.el`: predefined Elfeed integration
- `tests/gptel-reinforce-tests.el`: regression coverage

Artifact state lives under `var/gptel-reinforce/<database>/`:

- `<database>.sqlite`: feedback events
- `current.org`: active artifact text and prompts
- `summary.org`: incremental feedback summary
- `history/*.org`: immutable snapshots, including rollback targets

### Review Modes

`gptel-reinforce` is alpha. Keep the review model simple:

- `nil`: apply immediately
- `diff`: read-only diff plus confirmation
- `edit`: editable candidate buffer plus live unified diff

`edit` is the default in `ai.el`. If review UX changes again, update `gptel-reinforce-ui.el`, `README.org`, and the tests together.

### Safe Change Rules

- New accepted artifact text should always go through the shared apply path, so hooks and history stay consistent.
- Rollback should create a new history entry; it should never mutate old snapshots.
- `Applied Summary` in `current.org` should reflect what the current text has already incorporated. After rollback, clearing it is usually correct.

## Common Change Patterns

### Add a new package

1. Put the package config in the closest existing module.
2. Add global bindings in `bindings.el` if needed.
3. If it introduces reusable helpers, move those into `autoloads/` or the module itself instead of `init.el`.

### Add a new gptel-reinforce artifact

1. Register a database if the domain is new.
2. Register the artifact with prompts/hooks.
3. Decide whether feedback should be item-scoped, output-scoped, or both.
4. If output feedback is needed, call `gptel-reinforce-track-output-region` where the model output is inserted.

### Change review behavior

Touch these together:

- `gptel-reinforce/gptel-reinforce-core.el`
- `gptel-reinforce/gptel-reinforce-ui.el`
- `gptel-reinforce/tests/gptel-reinforce-tests.el`
- `gptel-reinforce/README.org`

## Testing

Targeted `gptel-reinforce` regression run:

```bash
emacs -Q --batch -L gptel-reinforce \
  -l gptel-reinforce/tests/gptel-reinforce-tests.el \
  -f ert-run-tests-batch-and-exit
```

For broader config work, at minimum verify startup:

```bash
emacs -Q --batch -l init.el
```

If startup work touches network-installed packages, do that interactively rather than assuming batch startup is enough.
