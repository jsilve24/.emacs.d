;;; elfeed.el --- elfeed config -*- lexical-binding: t -*-

(require 'gptel-reinforce-elfeed)

;; Short display names keyed by URL substring — edit here to rename feeds
(defvar jds/elfeed-feed-short-names
  '(("uasa20"           . "JASA")
    ("site_6513"        . "JRSSB")
    ("annualreviews"    . "AnnRevStats")
    ("nmeth"            . "NatureMethods")
    ("pmlr"             . "PMLR")
    ("jstatsoft"        . "JSS")
    ("stat.ME"          . "arXiv:stat.ME")
    ("stat.ML"          . "arXiv:stat.ML")
    ("cs.LG"            . "arXiv:cs.LG")
    ("q-bio.GN"         . "arXiv:q-bio.GN")
    ("q-bio.QM"         . "arXiv:q-bio.QM")))

(defun jds/elfeed-feed-title-short (orig-fn feed)
  "Return a short display name for FEED if known, else its real title."
  (let ((url (elfeed-feed-url feed)))
    (or (cdr (cl-find-if (lambda (pair) (string-match-p (car pair) url))
                         jds/elfeed-feed-short-names))
        (funcall orig-fn feed))))

(defun jds/elfeed-display-feed-title (feed)
  "Return the display title for FEED, preferring local abbreviations."
  (when feed
    (let ((url (elfeed-feed-url feed)))
      (or (cdr (cl-find-if (lambda (pair) (string-match-p (car pair) url))
                           jds/elfeed-feed-short-names))
          (elfeed-meta feed :title)
          (elfeed-feed-title feed)))))

(defvar jds/elfeed-min-score 0
  "Minimum elfeed-score value required in the default Elfeed view.")

(defconst jds/elfeed-score-filter-regexp
  "\\(?:^\\|[[:space:]]+\\)\\(\\^[+-]?[0-9]+\\)\\(?:[[:space:]]+\\|$\\)"
  "Regexp matching a custom Elfeed score filter token like `^-3'.")

(defun jds/elfeed-search-score-threshold (&optional filter)
  "Return the score threshold requested by FILTER, or nil if absent."
  (when-let* ((filter (or filter elfeed-search-filter))
              (token (and (string-match jds/elfeed-score-filter-regexp filter)
                          (match-string 1 filter))))
    (string-to-number (substring token 1))))

(defun jds/elfeed-strip-score-filter (filter)
  "Remove custom score filter tokens from FILTER."
  (string-trim
   (replace-regexp-in-string jds/elfeed-score-filter-regexp " " filter)))

(defun jds/elfeed-search-parse-filter-with-score (orig-fn filter)
  "Allow FILTER to contain custom score threshold tokens like `^-3'."
  (funcall orig-fn (jds/elfeed-strip-score-filter filter)))

(defun jds/elfeed-default-search-p ()
  "Return non-nil when Elfeed is showing the default inbox-style filter."
  (string= elfeed-search-filter (default-value 'elfeed-search-filter)))

(defun jds/elfeed-effective-min-score ()
  "Return the active Elfeed score threshold for the current search."
  (or (jds/elfeed-search-score-threshold)
      (and (jds/elfeed-default-search-p)
           jds/elfeed-min-score)))

(defun jds/elfeed-filter-by-score ()
  "Hide low-score entries according to the current search filter."
  (when-let* ((min-score (and (featurep 'elfeed-score)
                              (jds/elfeed-effective-min-score))))
    (setq elfeed-search-entries
          (seq-filter
           (lambda (entry)
             (>= (elfeed-score-scoring-get-score-from-entry entry)
                 min-score))
           elfeed-search-entries))))

(defun jds/elfeed-search-print-entry (entry)
  "Print ENTRY to the search buffer using abbreviated feed names."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed-title (jds/elfeed-display-feed-title (elfeed-entry-feed entry)))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat
                    (lambda (tag)
                      (propertize tag 'face 'elfeed-search-tag-face))
                    tags ","))
         (title-width (- (window-width) 10 elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               elfeed-search-title-max-width)
                        :left)))
    (when (featurep 'elfeed-score)
      (insert
       (elfeed-score-format-score
        (elfeed-score-scoring-get-score-from-entry entry))))
    (insert (propertize date 'face 'elfeed-search-date-face) " ")
    (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
    (when feed-title
      (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
    (when tags
      (insert "(" tags-str ")"))))

(defun jds/elfeed-search-mark-read-and-next ()
  "Mark the current Elfeed search entry read and advance one line."
  (interactive)
  (elfeed-search-untag-all-unread))

(defun jds/elfeed-show-mark-read-and-next ()
  "Mark the current Elfeed show entry read and display the next entry."
  (interactive)
  (let ((entry elfeed-show-entry)
        next-entry)
    (elfeed-untag entry 'unread)
    (with-current-buffer (elfeed-search-buffer)
      (elfeed-search-update-entry entry)
      (forward-line 1)
      (setq next-entry (elfeed-search-selected :ignore-region)))
    (if (elfeed-entry-p next-entry)
        (elfeed-show-entry next-entry)
      (funcall elfeed-show-entry-delete))))

(defun jds/elfeed-capture-reading-list (prefix)
  "Store the current Elfeed entry as a reading-list item.
With PREFIX, prompt for a note while recording positive feedback."
  (interactive "P")
  (require 'ol)
  (require 'org-capture)
  (gptel-reinforce-like prefix "elfeed-ranking")
  (org-store-link nil t)
  (org-capture nil "r"))

(use-package elfeed
  :config
  (setq elfeed-use-curl t)
  (setq elfeed-search-filter "@2-weeks-ago +unread")
  (setq elfeed-db-directory
        (expand-file-name "var/elfeed" user-emacs-directory))
  (setq elfeed-search-print-entry-function #'jds/elfeed-search-print-entry)
  (advice-add 'elfeed-search-parse-filter :around
              #'jds/elfeed-search-parse-filter-with-score)
  (advice-add 'elfeed-search--update-list :after #'jds/elfeed-filter-by-score)

  (advice-add 'elfeed-feed-title :around #'jds/elfeed-feed-title-short)

  ;; mu4e-like bindings in search (headers) view
  (evil-collection-define-key 'normal 'elfeed-search-mode-map
    ;; F = follow links (same as mu4e-headers F)
    "F"  #'link-hint-open-link
    ;; d = mark read and advance (cf. mu4e d = mark trash and advance)
    "d"  #'jds/elfeed-search-mark-read-and-next
    ;; R = refresh feeds (cf. mu4e gr / R for reindex)
    "R"  #'elfeed-search-update--force
    ;; ! / ? = mark read / mark unread (cf. mu4e ! / ?)
    "!"  #'elfeed-search-untag-all-unread
    "?"  #'elfeed-search-tag-all-unread)

  ;; mu4e-like bindings in show (view) view
  (evil-collection-define-key 'normal 'elfeed-show-mode-map
    "d"  #'jds/elfeed-show-mark-read-and-next
    "F"  #'link-hint-open-link))

;; prevent org-element hooks firing on the score file buffer
(add-to-list 'auto-mode-alist '("\\.score\\'" . lisp-data-mode))

(use-package elfeed-score
  :after elfeed
  :config
  (setq elfeed-score-serde-score-file
        (expand-file-name "elfeed.score" user-emacs-directory))
  (elfeed-score-enable)
  ;; show scores in search view
  (setq elfeed-score-score-format '("%d " 6 :right))
  ;; use score-based sort
  (setq elfeed-search-sort-function #'elfeed-score-sort))

(use-package elfeed-org
  :after elfeed
  :config
  (setq rmh-elfeed-org-files
        (list (expand-file-name "elfeed.org" user-emacs-directory)))
  (elfeed-org))

;;; localleader bindings (mirrors mu4e localleader pattern)
(jds/localleader-def
  :keymaps '(elfeed-search-mode-map elfeed-show-mode-map)
  "l" #'jds/elfeed-capture-reading-list
  "u" #'elfeed-update)

(with-eval-after-load 'elfeed
  (evil-collection-define-key 'normal 'elfeed-search-mode-map
    "+" #'gptel-reinforce-like
    "-" #'gptel-reinforce-dislike
    "0" #'gptel-reinforce-neutral)
  (evil-collection-define-key 'normal 'elfeed-show-mode-map
    "+" #'gptel-reinforce-like
    "-" #'gptel-reinforce-dislike
    "0" #'gptel-reinforce-neutral))
