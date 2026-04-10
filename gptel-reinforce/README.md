# gptel-reinforce

`gptel-reinforce` collects feedback as you work, summarizes patterns with an LLM, and incrementally improves text artifacts — AI prompts, rule sets, templates, config snippets, or any text you want to evolve from experience.

Feedback is stored locally in SQLite. No data leaves your machine except what you send to your LLM backend.

## Contents

- [Overview](#overview)
- [Quick start](#quick-start)
- [Core concepts](#core-concepts)
- [Commands](#commands)
- [Predefined integrations](#predefined-integrations)
- [Writing your own integration](#writing-your-own-integration)
- [Appendix A: Database schema](#appendix-a-database-schema)
- [Appendix B: Org file formats](#appendix-b-org-file-formats)
- [Appendix C: Configuration variables](#appendix-c-configuration-variables)

---

## Overview

The improvement loop works in three steps:

1. **Record feedback** while working. Call `gptel-reinforce-like`, `gptel-reinforce-dislike`, or `gptel-reinforce-score-item` whenever something good or bad happens. Each event is written immediately to SQLite.

2. **Summarize.** Call `gptel-reinforce-summarize` for an artifact. The LLM reads all feedback events since the last summary and updates a running `summary.org`. Only new events are sent each time.

3. **Update.** Call `gptel-reinforce-update` for an artifact. The LLM reads the summary and proposes a minimal revision to the artifact text. You review a diff and accept or reject it. Accepted revisions are archived and become the new `current.org`.

Artifacts improve gradually, driven by real usage rather than one-shot prompting.

---

## Quick start

Load the package:

```elisp
(add-to-list 'load-path (expand-file-name "gptel-reinforce" user-emacs-directory))
(require 'gptel-reinforce)
```

Register a database and then register an artifact associated with that database:

```elisp
(gptel-reinforce-register-database
 :name "my-db"
 :context-fn #'my-context-fn)

(gptel-reinforce-register-artifact
 :name "my-artifact"
 :database "my-db"
 :type "prompt")
```

Record feedback while in a registered context:

```elisp
(gptel-reinforce-like)              ; +1 for the current item
(gptel-reinforce-dislike)           ; -1
(gptel-reinforce-score-item nil 3)  ; arbitrary numeric score
```

Prefix any feedback command with `C-u` to attach a short note. Notes are treated as high-value evidence during summarization.

Then run the improvement loop:

```elisp
(gptel-reinforce-summarize "my-artifact")
(gptel-reinforce-update "my-artifact")
```

For the predefined Elfeed integration ([details](#elfeed-elfeed-score-integration)), just call:

```elisp
(gptel-reinforce-register-elfeed-module)
```

---

## Core concepts

### Databases

A **database** represents one application context — an RSS reader, email client, file navigator, etc. It owns:

- A SQLite file at `<state-root>/<name>.sqlite` for all feedback events
- A root directory for artifact org files
- A **context function** that describes what the user is looking at right now

Multiple artifacts can share one database.

### Artifacts

An **artifact** is a piece of text you want to improve — a prompt, rule set, template, or config file. It belongs to one database and lives in three files on disk:

- `current.org` — the active version of the text and metadata
- `summary.org` — the running feedback summary and `LAST_EVENT_ID` (used to prevent reprocessing already summarized feedback)
- `history/<artifact>/<timestamp>-<name>.org` — immutable snapshots of every accepted version

See [Appendix B](#appendix-b-org-file-formats) for the file formats.

You can edit `current.org` directly at any time. The `* Current Text` heading holds the artifact body. The `* Summarizer User Prompt` and `* Updater User Prompt` headings let you add task-specific guidance that is appended to the default system prompts on every LLM call to summarise the feedback database or update the artifact based on the summary respectively.

### Feedback types

There are two kinds of feedback events. The difference is *what you are reacting to*.

**Item feedback** — you are reacting to the item itself (an RSS entry, an email, a file). Call `gptel-reinforce-like` or `gptel-reinforce-dislike` while viewing the item. For example, thumbs-up on an elfeed entry means "I want to see more entries like this." This signal is **database-scoped**: every artifact in the database sees it when summarizing, because the signal is about the domain, not about any one artifact.

**Output feedback** — you are reacting to AI-generated text that a specific artifact version produced. For example, if an artifact generated a triage summary and the summary was wrong, you dislike *that output*. This signal is **artifact-scoped**: the event is stored with the producing artifact's name, and when `gptel-reinforce-summarize` runs it only includes output-feedback events that name that specific artifact — other artifacts in the same database never see it.

Output feedback requires manual wiring. There is no automatic hook into gptel. Inside your LLM request callback, after inserting the response text, call `gptel-reinforce-track-output-region` to annotate the inserted region:

```elisp
(gptel-request my-prompt
  :callback
  (lambda (response _info)
    (let ((start (point)))
      (insert response)
      (gptel-reinforce-track-output-region "my-artifact" start (point)))))
```

After that, `gptel-reinforce-like` and friends at that region automatically record output-feedback. Outside that region they record item-feedback as normal.

For the predefined elfeed integration you only ever use item feedback and none of this applies. Output feedback is only relevant if you build a custom workflow where an artifact generates text displayed to the user (i.e., LLM text completion).

### Context functions

A context function describes what the user is looking at right now. It is called on every feedback command. It must return a plist or nil. The only required key is `:item-key` — a stable string that uniquely identifies the item. All other keys are optional:

| Key             | Type   | Description                                   |
|-----------------|--------|-----------------------------------------------|
| `:item-key`     | string | **Required.** Stable unique ID for this item  |
| `:title`        | string | Human-readable label shown in messages        |
| `:primary-text` | string | Main content — excerpt, snippet, body         |
| `:meta`         | plist  | Additional structured metadata (any keys)     |

Return nil when not in a valid context. The feedback command will skip this database or prompt the user to choose one.

---

## Commands

### Feedback commands

All feedback commands accept `C-u` as a prefix to prompt for an optional note.

The auto-detect commands (`gptel-reinforce-like`, `-dislike`, `-neutral`, `-score`) check for output provenance text at point. If found, they record output-feedback; otherwise item-feedback.

| Command                         | Score | Target                            |
|---------------------------------|-------|-----------------------------------|
| `gptel-reinforce-like`          | +1    | Output at point, else current item |
| `gptel-reinforce-dislike`       | -1    | Output at point, else current item |
| `gptel-reinforce-neutral`       | 0     | Output at point, else current item |
| `gptel-reinforce-score`         | N     | Output at point, else current item |
| `gptel-reinforce-like-item`     | +1    | Current item only                  |
| `gptel-reinforce-dislike-item`  | -1    | Current item only                  |
| `gptel-reinforce-neutral-item`  | 0     | Current item only                  |
| `gptel-reinforce-score-item`    | N     | Current item only                  |
| `gptel-reinforce-score-output`  | N     | Output at point only               |

### Summarize and update

**`gptel-reinforce-summarize ARTIFACT`** — reads all feedback events for ARTIFACT since `LAST_EVENT_ID` and asks the LLM to update `summary.org`. Only new events are sent; summarization is incremental and safe to run repeatedly.

**`gptel-reinforce-update ARTIFACT`** — sends the current artifact text and `summary.org` to the LLM and requests a minimal revision. Shows a unified diff and asks for confirmation (skipped when `AUTO_UPDATE: t` in `current.org`). Accepted revisions are written to the history directory and become the new `current.org`.

### Utility

**`gptel-reinforce-set-active-database DATABASE`** — sets a buffer-local default database. Useful in mode hooks so feedback commands in that buffer always target a specific database without auto-detection.

---

## Predefined integrations

### Elfeed (elfeed-score integration)

Tracks your thumbs-up/thumbs-down feedback on RSS entries and uses it to iteratively improve your [`elfeed-score`](https://github.com/sp1ff/elfeed-score) rule file (`elfeed.score`). Each time you run `gptel-reinforce-update`, the LLM receives a summary of your recent feedback and proposes a revised score file; if you accept the diff, the file is written to disk and reloaded immediately.

```elisp
(gptel-reinforce-register-elfeed-module)
```

**Seeding**: on first call, if `elfeed.score` already exists and the artifact is empty, the file is read in automatically so the LLM has a baseline to work from.

**Validation**: before any LLM-proposed score file is accepted, a pre-update hook checks that it parses as valid Lisp. Invalid output is rejected without touching your score file.

**Reload**: after an accepted update, the post-update hook writes the new text to `gptel-reinforce-elfeed-score-file` and calls `elfeed-score-load-score-file`, so scoring takes effect immediately without restarting Emacs.

The context function works in both `elfeed-search-mode` and `elfeed-show-mode`, so you can give feedback from the search list or from inside an entry.

To re-seed the artifact from the score file after editing it manually:

```elisp
(gptel-reinforce-elfeed-seed-score-file 'force)
```

### Mu4e

Improves a prompt for email triage.

```elisp
(gptel-reinforce-register-mu4e-module)
```

The artifact is a `"prompt"` type. No hooks are needed — the text lives in `current.org` and you copy it into your triage workflow as needed.

---

## Writing your own integration

### 1. Write a context function

The context function is called in the user's current buffer on every feedback command. Return a plist or nil.

```elisp
(defun my-context ()
  "Return context for my-mode, or nil when not applicable."
  (when (derived-mode-p 'my-mode)
    (when-let* ((item (my-current-item)))
      (list :item-key     (my-item-id item)     ; required — must be stable
            :title        (my-item-title item)  ; shown in echo area
            :primary-text (my-item-body item)   ; sent to the LLM
            :meta         (list :source (my-item-source item))))))
```

Guard with `(featurep ...)` and `(derived-mode-p ...)` so the function returns nil cleanly in unrelated buffers. `:item-key` should be a stable identifier that doesn't change across sessions — a URL, message-id, hash, etc.

### 2. Register the database and artifact

```elisp
(gptel-reinforce-register-database
 :name "my-db"             ; required — unique string key
 :context-fn #'my-context) ; required

(gptel-reinforce-register-artifact
 :name "my-artifact"       ; required — unique within the database
 :database "my-db"         ; required
 :type "prompt"            ; optional — informational ("prompt", "code", "rules", …)
 :auto-update nil          ; if t, skip the diff review step
 :summarizer-user-prompt "Focus especially on X and Y."
 :updater-user-prompt     "Prefer conservative edits.")
```

See the full key reference in [Appendix C](#appendix-c-configuration-variables).

### 3. Bind feedback commands

```elisp
(define-key my-mode-map (kbd "C-c +") #'gptel-reinforce-like)
(define-key my-mode-map (kbd "C-c -") #'gptel-reinforce-dislike)
```

Or set the active database in a mode hook to resolve ambiguity when multiple databases are registered:

```elisp
(add-hook 'my-mode-hook
          (lambda () (gptel-reinforce-set-active-database "my-db")))
```

### Using hooks

**Pre-update hooks** receive `(artifact current-record candidate-text)` and should return non-nil to accept or nil to reject. Use them for validation:

```elisp
(defun my-validate (_artifact _current-record candidate)
  "Return non-nil when CANDIDATE is valid Lisp."
  (condition-case nil
      (progn (read-from-string candidate) t)
    (error nil)))
```

**Post-update hooks** receive `(artifact version-ref current-record candidate-text)` and run after the new version is written to disk. Use them to apply the artifact to its target:

```elisp
(defun my-apply (_artifact _version-ref _current-record candidate)
  "Write CANDIDATE to the config file and reload it."
  (with-temp-file my-config-file
    (insert candidate))
  (my-reload-config))
```

Register hooks at artifact creation time:

```elisp
(gptel-reinforce-register-artifact
 :name "my-artifact"
 :database "my-db"
 :pre-update-hook  #'my-validate   ; or a list of functions
 :post-update-hook #'my-apply)
```

### Tracking output provenance

To capture output-feedback that traces back to the exact artifact version that produced it, annotate the inserted text:

```elisp
(let ((start (point)))
  (insert response)
  (gptel-reinforce-track-output-region "my-artifact" start (point)))
```

After annotation, `gptel-reinforce-like` and friends at that text automatically record output-feedback rather than item-feedback.

For text not inserted into a buffer, use `gptel-reinforce-propertize-output` to get an annotated string:

```elisp
(setq annotated (gptel-reinforce-propertize-output "my-artifact" raw-text))
```

---

## Appendix A: Database schema

Each database has one SQLite file. The only table is `feedback_events`. Rows are never updated or deleted — the table is append-only.

### `feedback_events`

| Column                 | Type    | Null | Description                                                    |
|------------------------|---------|------|----------------------------------------------------------------|
| `id`                   | INTEGER | no   | Primary key, monotonically increasing                          |
| `created_at`           | TEXT    | no   | ISO-8601 timestamp                                             |
| `event_type`           | TEXT    | no   | `"item-feedback"` or `"output-feedback"`                       |
| `item_key`             | TEXT    | yes  | Stable identifier from the context function                    |
| `score`                | REAL    | no   | Numeric score (−1 / 0 / +1 or any float)                      |
| `title`                | TEXT    | yes  | Human-readable label at feedback time                          |
| `primary_text`         | TEXT    | yes  | Content excerpt at feedback time                               |
| `meta_json`            | TEXT    | yes  | JSON-encoded `:meta` plist from the context function           |
| `note`                 | TEXT    | yes  | Optional free-text note attached by the user                   |
| `artifact_name`        | TEXT    | yes  | Non-null only for `"output-feedback"` events                   |
| `artifact_version_ref` | TEXT    | yes  | History filename of the artifact version that produced output  |
| `output_id`            | TEXT    | yes  | Opaque ID linking an event to a specific output region         |

### Indexes

- `feedback_events_event_type_idx` on `event_type`
- `feedback_events_artifact_name_idx` on `artifact_name`

### Summarization query

When summarizing artifact `A` in database `D` with last-event-id `N`, the query is:

```sql
SELECT ... FROM feedback_events
WHERE id > N
  AND (event_type = 'item-feedback'
       OR (event_type = 'output-feedback' AND artifact_name = 'A'))
ORDER BY id ASC
```

All item-feedback in the database is visible to every artifact in that database. Output-feedback is scoped to the specific artifact.

---

## Appendix B: Org file formats

### `current.org`

Stores the active artifact text and metadata. Edit this file directly.

```org
:PROPERTIES:
:DATABASE:    <database-name>
:ARTIFACT:    <artifact-name>
:TYPE:        <type string, e.g. "prompt" or "code">
:VERSION_REF: <history filename for this version>
:UPDATED_AT:  <ISO-8601 timestamp>
:AUTO_UPDATE: t | nil
:END:

* Current Text
 <artifact text — each line indented by one space>

* Summarizer User Prompt
 <optional guidance appended to the summarizer system prompt>

* Updater User Prompt
 <optional guidance appended to the updater system prompt>
```

Set `AUTO_UPDATE: t` to skip the diff review step during `gptel-reinforce-update`.

`VERSION_REF` is the basename of the history file for this version, e.g. `20240915T142300-my-artifact.org`. It is used to link output-feedback events back to the exact version.

### `summary.org`

Stores the running feedback summary. Updated by `gptel-reinforce-summarize`.

```org
:PROPERTIES:
:DATABASE:      <database-name>
:ARTIFACT:      <artifact-name>
:LAST_EVENT_ID: <integer>
:UPDATED_AT:    <ISO-8601 timestamp>
:END:

* Summary
<LLM-maintained summary of stable feedback patterns>

* Uncertainty
<Mixed or weak evidence>

* Notes
<High-value observations from user notes>
```

`LAST_EVENT_ID` is the `id` of the most recent event included in this summary. Summarization reads only events with `id > LAST_EVENT_ID`, so it is always incremental.

### History snapshots

Each accepted update creates a file at `history/<artifact>/<timestamp>-<name>.org`:

```org
:PROPERTIES:
:DATABASE:                     <database-name>
:ARTIFACT:                     <artifact-name>
:TYPE:                         <type>
:UPDATED_AT:                   <ISO-8601 timestamp>
:UPDATE_MODE:                  manual-approved | auto-updated | initial
:SOURCE_SUMMARY_LAST_EVENT_ID: <last-event-id at time of update>
:END:

* Current Text
 <artifact text at this version>
```

The filename (without directory) becomes `VERSION_REF` in `current.org` and `artifact_version_ref` in the database, linking feedback events to the exact version of the artifact that produced them.

---

## Appendix C: Configuration variables

### Paths

| Variable                   | Default                              | Description                                     |
|----------------------------|--------------------------------------|-------------------------------------------------|
| `gptel-reinforce-state-root`  | `~/.emacs.d/var/gptel-reinforce/` | Root for SQLite files and history snapshots      |
| `gptel-reinforce-config-root` | `~/.emacs.d/gptel-reinforce/`     | Default root for artifact org files              |

### LLM backend

| Variable                              | Default                                      | Description                              |
|---------------------------------------|----------------------------------------------|------------------------------------------|
| `gptel-reinforce-backend-function`    | `gptel-reinforce-backend-send-with-gptel`    | Function used to send LLM requests       |
| `gptel-reinforce-default-summarizer-prompt` | (see source)                           | System prompt for summarization          |
| `gptel-reinforce-default-updater-prompt`    | (see source)                           | System prompt for artifact updates       |

To swap the LLM transport entirely, set `gptel-reinforce-backend-function` to a function with signature `(request callback)`. `request` is either a `gptel-reinforce-summary-request` or `gptel-reinforce-update-request` struct; `callback` is `(lambda (response info) ...)`.

```elisp
(setq gptel-reinforce-backend-function
      (lambda (request callback)
        ;; Build prompt from request, call your LLM, then:
        (funcall callback "revised artifact text" nil)))
```

### Registration key reference

#### `gptel-reinforce-register-database` keys

| Key                | Required | Default                           | Description                                      |
|--------------------|----------|-----------------------------------|--------------------------------------------------|
| `:name`            | yes      | —                                 | Unique string identifier                         |
| `:context-fn`      | yes      | —                                 | Function → context plist or nil                  |
| `:db-path`         | no       | `<state-root>/<name>.sqlite`      | SQLite file path                                 |
| `:root-dir`        | no       | `<config-root>/<name>/`           | Root directory for artifact org files            |
| `:legacy-root-dir` | no       | nil                               | Old root-dir path; migrated automatically        |

#### `gptel-reinforce-register-artifact` keys

| Key                          | Required | Default                                      | Description                                             |
|------------------------------|----------|----------------------------------------------|---------------------------------------------------------|
| `:name`                      | yes      | —                                            | Unique string identifier                                |
| `:database`                  | yes      | —                                            | Parent database name                                    |
| `:type`                      | no       | nil                                          | Informational type: `"prompt"`, `"code"`, `"rules"`, … |
| `:auto-update`               | no       | nil                                          | Skip diff review when t                                 |
| `:summarizer-system-prompt`  | no       | `gptel-reinforce-default-summarizer-prompt`  | Full system prompt for summarization                    |
| `:summarizer-user-prompt`    | no       | `""`                                         | Appended to system prompt as task-specific guidance     |
| `:updater-system-prompt`     | no       | `gptel-reinforce-default-updater-prompt`     | Full system prompt for artifact updates                 |
| `:updater-user-prompt`       | no       | `""`                                         | Appended to system prompt as task-specific guidance     |
| `:pre-update-hook`           | no       | nil                                          | Function or list; return nil to reject candidate        |
| `:post-update-hook`          | no       | nil                                          | Function or list; called after acceptance               |

### Elfeed-specific

| Variable                                  | Default                         | Description                                        |
|-------------------------------------------|---------------------------------|----------------------------------------------------|
| `gptel-reinforce-elfeed-score-file`       | `~/.emacs.d/elfeed.score`       | Score file written by the post-update hook         |
| `gptel-reinforce-elfeed-max-excerpt-length` | `500`                         | Max characters retained from entry content         |
