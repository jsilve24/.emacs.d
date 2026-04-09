;;; elfeed.el --- elfeed config -*- lexical-binding: t -*-

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

(use-package elfeed
  :config
  (setq elfeed-use-curl t)
  (setq elfeed-search-filter "@2-weeks-ago +unread #5")
  (setq elfeed-db-directory
        (expand-file-name "var/elfeed" user-emacs-directory))
  (setq elfeed-search-print-entry-function #'jds/elfeed-search-print-entry)

  (advice-add 'elfeed-feed-title :around #'jds/elfeed-feed-title-short)

  ;; mu4e-like bindings in search (headers) view
  (evil-collection-define-key 'normal 'elfeed-search-mode-map
    ;; F = follow links (same as mu4e-headers F)
    "F"  #'link-hint-open-link
    ;; d = mark read and advance (cf. mu4e d = mark trash and advance)
    "d"  (lambda () (interactive)
           (elfeed-search-untag-all-unread)
           (forward-line 1))
    ;; R = refresh feeds (cf. mu4e gr / R for reindex)
    "R"  #'elfeed-search-update--force
    ;; ! / ? = mark read / mark unread (cf. mu4e ! / ?)
    "!"  #'elfeed-search-untag-all-unread
    "?"  #'elfeed-search-tag-all-unread)

  ;; mu4e-like bindings in show (view) view
  (evil-collection-define-key 'normal 'elfeed-show-mode-map
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
        (list (expand-file-name "elfeed.org" org-directory)))
  (elfeed-org))

;;; localleader bindings (mirrors mu4e localleader pattern)
(jds/localleader-def
  :keymaps '(elfeed-search-mode-map elfeed-show-mode-map)
  "l" (lambda () (interactive)
        (org-store-link nil t)
        (org-capture nil "r"))
  "u" #'elfeed-update)
