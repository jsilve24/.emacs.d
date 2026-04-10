# gptel-reinforce

`gptel-reinforce` is a local Emacs package for improving text artifacts from feedback you record while working.

It is not limited to prompts. An artifact can be a prompt, a code block, a rule set, a template, or a config snippet.

Feedback is stored in SQLite. The editable artifact state lives in Org files under `~/.emacs.d/gptel-reinforce/`.

## Quick start

Load the package from your config:

```elisp
(add-to-list 'load-path (expand-file-name "gptel-reinforce" user-emacs-directory))
(require 'gptel-reinforce)
```

Register one database and one artifact:

```elisp
(gptel-reinforce-register-database
 :name "mu4e-triage"
 :context-fn #'my-mu4e-context
 :db-path "~/.emacs.d/var/gptel-reinforce/mu4e-triage.sqlite"
 :root-dir "~/.emacs.d/gptel-reinforce/mu4e-triage/")

(gptel-reinforce-register-artifact
 :name "collaborator-triage"
 :database "mu4e-triage"
 :type "prompt"
 :auto-update nil
 :summarizer-system-prompt gptel-reinforce-default-summarizer-prompt
 :summarizer-user-prompt "Pay extra attention to collaborators and grants."
 :updater-system-prompt gptel-reinforce-default-updater-prompt
 :updater-user-prompt "Prefer small edits. Preserve structure and tone.")
```

While working in a registered context:

```elisp
(gptel-reinforce-like)
(gptel-reinforce-dislike)
(gptel-reinforce-score-item nil 2)
```

Use `C-u` with any feedback command to attach a short note.

Then summarize and update:

```elisp
(gptel-reinforce-summarize "collaborator-triage")
(gptel-reinforce-update "collaborator-triage")
```

## Predefined integrations

The generic package stays task-agnostic.

- `gptel-reinforce-mu4e.el` provides a predefined `mu4e` setup.
- `gptel-reinforce-elfeed.el` provides a predefined Elfeed setup and is loaded automatically by `gptel-reinforce`, so users can just call:

```elisp
(gptel-reinforce-register-elfeed-module)
```

The predefined Elfeed module validates updates, writes accepted artifact text back to `elfeed.score`, and can seed the artifact from that file when it is still empty.

If you generate output from an artifact and want later output feedback to point to the exact artifact version, annotate the output when you insert it:

```elisp
(let ((start (point)))
  (insert response)
  (gptel-reinforce-track-output-region "collaborator-triage" start (point)))
```

`gptel-reinforce-score-output` can then store output feedback with the artifact name and history filename that produced it.

## Files

Each database has one SQLite file under `~/.emacs.d/var/gptel-reinforce/`.

Each artifact gets:

- `current.org`
- `summary.org`
- history snapshots under `~/.emacs.d/var/gptel-reinforce/<database>/history/`

`current.org` stores the active text and metadata, including the current history filename. `summary.org` stores the running summary plus `LAST_EVENT_ID`, so summarization only sees new feedback. Historical versions are kept as files under the database history directory for manual inspection.

## Extension points

- Database `:context-fn`: extracts the current item as a plist with `:item-key`, `:title`, `:primary-text`, and `:meta`.
- Artifact prompts: `:summarizer-user-prompt` and `:updater-user-prompt` append task-specific guidance to the default system prompts.
- Hooks: `:pre-update-hook` can reject a candidate update, and `:post-update-hook` can reload or re-apply accepted text.
- Backend: `gptel-reinforce-backend-function` receives request objects, so the transport can be swapped later without rewriting the package logic.
