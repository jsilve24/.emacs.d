;;; git.el --- magit config -*- lexical-binding: t; -*-

(require 'subr-x)

(use-package magit
  :commands (magit-status magit-list-repositories)
  :init
  (setq magit-repository-directories '(("~/.homesick/repos/arch-dotfiles/" . 0)
				       ("~/Dropbox/org/roam/references/" . 0)
				       ("/home/jds6696/.local/share/ArchMatic/" . 0)
				       ("~/Dropbox/Research/src/fido/" . 0)
				       ("~/.emacs.d/" . 0)
				       ("~/Dropbox/Research/src/ALDEx3/" . 0)
				       ("~/Dropbox/Research/src/philr/" . 0))
	magit-repolist-columns '(("Name" 25 magit-repolist-column-ident nil)
				 ;; ("Version" 25 magit-repolist-column-version
				 ;;  ((:sort magit-repolist-version<)))
				 ("Flag" 8 magit-repolist-column-flag)
				 ("B<U" 3 magit-repolist-column-unpulled-from-upstream
				  ((:right-align t)
				   (:sort <)))
				 ("B>U" 3 magit-repolist-column-unpushed-to-upstream
				  ((:right-align t)
				   (:sort <)))))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (transient-append-suffix 'magit-merge "-w" '("-a" "Allow unrelated histories" "--allow-unrelated-histories"))

  ;; allow use of magit-list-repos for overviews
  ;; https://magit.vc/manual/magit/Repository-List.html (documentation)

  ;; https://mstdn.social/@matt1126/113845518438376764
  (setq magit-diff-refine-hunk 'all))

(use-package browse-at-remote)

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after magit
  :init
  (setq forge-add-default-bindings nil))

;; for easier handling of pull-requests on github
(use-package code-review
  :disabled
  :config
  (general-define-key
   :keymaps 'code-review-mode-map
   :states 'n
   "?" #'code-review-transient-api)
  (jds/localleader-def
  :keymaps 'forge-topic-mode-map
  "r" #'code-review-forge-pr-at-point))


(use-package git-timemachine
  :straight (git-timemachine :type git :host github :repo "emacsmirror/git-timemachine"))

(defcustom jds/gitignore-templates
  '(("Emacs"
     ".dir-locals.el"
     ".projectile"
     ".elc"
     "auto-save-list/"
     "tramp")
    ("LaTeX"
     "*.aux"
     "*.pdf"
     "*.bbl"
     "*.bcf"
     "*.blg"
     "*.cb"
     "*.cb2"
     "*.dvi"
     "*.fdb_latexmk"
     "*.fmt"
     "*.fls"
     "*.fot"
     "*.idx"
     "*.ilg"
     "*.ind"
     "*.lof"
     "*.log"
     "*.lot"
     "*.nav"
     "*.out"
     "*.pdfsync"
     "*.run.xml"
     "*.snm"
     "*.synctex"
     "*.synctex.gz"
     "*.toc"
     "*.vrb"
     "*-blx.aux"
     "*-blx.bib"
     "*-converted-to.*"
     ".auctex-auto/"
     "auto/"
     "latex.out/"
     "rubber.cache/"
     "_minted*/")
    ("Org"
     "*.org-id-locations"
     ".org-timestamps/")
    ("Python"
     "__pycache__/"
     "*.egg-info/"
     "*.py[cod]"
     ".coverage"
     ".eggs/"
     ".ipynb_checkpoints/"
     ".mypy_cache/"
     ".pytest_cache/"
     ".python-version"
     ".ruff_cache/"
     ".venv/"
     "build/"
     "dist/"
     "venv/")
    ("Quarto"
     ".quarto/"
     "_freeze/"
     "_site/")
    ("R"
     ".RData"
     ".RDataTmp"
     ".Rapp.history"
     ".Rhistory"
     ".Rproj.user/"
     ".Ruserdata"
     "*.Rcheck/"
     "*.tar.gz"
     "*-Ex.R"
     "*.Rproj"
     "*.knit.md"
     "*.utf8.md"
     "*_cache/"
     "/cache/"
     ".Renviron"
     ".Rprofile"
     ".httr-oauth"
     "docs/"
     "po/*~"
     "rsconnect/"
     "vignettes/*.html"
     "vignettes/*.pdf")
    ("renv"
     "renv/library/"
     "renv/python/"
     "renv/staging/")
    ("Stan"
     "*.hpp"
     "*.o"
     "*.so")
    ("Typst"
     "*.pdf"
     "*.png")
    ("macOS"
     ".DS_Store"))
  "Named `.gitignore' fragments for quick project bootstrapping.

Each entry is of the form (NAME PATTERN...).  Extend this list with
additional project types as needed."
  :type '(repeat
          (cons :tag "Template"
                (string :tag "Name")
                (repeat :tag "Patterns" (string :tag "Pattern"))))
  :group 'tools)

(defun jds/gitignore--project-root ()
  "Return the current project root, falling back to `default-directory'."
  (or (when-let ((project (project-current nil)))
        (project-root project))
      (when (fboundp 'projectile-project-root)
        (ignore-errors (projectile-project-root)))
      default-directory))

(defun jds/gitignore--template-names ()
  "Return available `jds/gitignore-templates' names."
  (mapcar #'car jds/gitignore-templates))

(defun jds/gitignore--read-templates ()
  "Prompt for one or more gitignore templates."
  (completing-read-multiple
   "Gitignore template(s): "
   (jds/gitignore--template-names)
   nil t))

(defun jds/gitignore--render-template (name)
  "Render template NAME as a gitignore fragment."
  (when-let ((patterns (cdr (assoc-string name jds/gitignore-templates t))))
    (concat "# " name "\n"
            (mapconcat #'identity patterns "\n")
            "\n")))

(defun jds/gitignore-apply-templates (templates &optional root)
  "Append missing gitignore TEMPLATES to `.gitignore' under ROOT.

When ROOT is nil, use the current project root."
  (let* ((root (file-name-as-directory (or root (jds/gitignore--project-root))))
         (target (expand-file-name ".gitignore" root))
         (selected (delete-dups (seq-filter #'identity templates))))
    (find-file target)
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (dolist (name selected)
      (when-let ((fragment (jds/gitignore--render-template name)))
        (unless (save-excursion
                  (goto-char (point-min))
                  (search-forward fragment nil t))
          (unless (or (bobp)
                      (save-excursion
                        (forward-line -1)
                        (looking-at-p "^$")))
            (insert "\n"))
          (insert fragment))))
    (save-buffer)
    target))

;;;###autoload
(defun jds/project-insert-gitignore (templates)
  "Insert gitignore TEMPLATES into the current project's `.gitignore'.

When called interactively, prompt for one or more template names and
append any missing fragments to the project's `.gitignore' file."
  (interactive (list (jds/gitignore--read-templates)))
  (let ((target (jds/gitignore-apply-templates templates)))
    (message "Updated %s with %s"
             (abbreviate-file-name target)
             (string-join templates ", "))))

(provide 'config-git)
