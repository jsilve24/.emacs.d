;;; gptel-reinforce-tests.el --- ERT tests for gptel-reinforce -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused regression tests for review flows and the optional Elfeed
;; integration.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gptel-reinforce-core)
(require 'gptel-reinforce-db)
(require 'gptel-reinforce-org)
(require 'gptel-reinforce-ui)
(require 'gptel-reinforce-elfeed)

(defmacro gptel-reinforce-test-with-temp-env (&rest body)
  "Run BODY with isolated gptel-reinforce state and config directories."
  (declare (indent 0) (debug t))
  `(let* ((temp-dir (make-temp-file "gptel-reinforce-test-" t))
          (gptel-reinforce-state-root (expand-file-name "state/" temp-dir))
          (gptel-reinforce--databases (make-hash-table :test #'equal))
          (gptel-reinforce--artifacts (make-hash-table :test #'equal)))
     (unwind-protect
         (progn ,@body)
       (delete-directory temp-dir t))))

(defun gptel-reinforce-test--register-database (&optional name db-path root-dir)
  "Register a test database with NAME, DB-PATH, and ROOT-DIR."
  (let ((args (list
               :name (or name "test-db")
               :candidate-fn (lambda ()
                               '(:context (:item-key "item-1" :title "Item")
                                 :priority 0
                                 :label "Test DB")))))
    (when db-path
      (setq args (append args (list :db-path db-path))))
    (when root-dir
      (setq args (append args (list :root-dir root-dir))))
    (apply #'gptel-reinforce-register-database args)))

(ert-deftest gptel-reinforce-register-database-defaults-to-self-contained-dir ()
  (gptel-reinforce-test-with-temp-env
    (let* ((database (gptel-reinforce-test--register-database))
           (root-dir (gptel-reinforce-database-root-dir database))
           (db-path (gptel-reinforce-database-db-path database)))
      (should (equal root-dir
                     (expand-file-name "test-db/" gptel-reinforce-state-root)))
      (should (equal db-path
                     (expand-file-name "test-db.sqlite" root-dir)))
      (should (file-exists-p db-path)))))

(ert-deftest gptel-reinforce-artifact-files-live-at-database-root ()
  (gptel-reinforce-test-with-temp-env
    (let* ((database (gptel-reinforce-test--register-database))
           (root-dir (gptel-reinforce-database-root-dir database)))
      (gptel-reinforce-register-artifact
       :name "artifact-1"
       :database "test-db"
       :type "prompt")
      (should (equal (gptel-reinforce-artifact-dir "artifact-1") root-dir))
      (should (equal (gptel-reinforce-artifact-current-file "artifact-1")
                     (expand-file-name "current.org" root-dir)))
      (should (equal (gptel-reinforce-artifact-summary-file "artifact-1")
                     (expand-file-name "summary.org" root-dir)))
      (should (equal (directory-file-name
                      (gptel-reinforce-artifact-history-dir "artifact-1"))
                     (directory-file-name
                      (expand-file-name "history" root-dir))))
      (should (file-exists-p (gptel-reinforce-artifact-current-file "artifact-1")))
      (should (file-exists-p (gptel-reinforce-artifact-summary-file "artifact-1"))))))

(ert-deftest gptel-reinforce-review-text-dispatches-modes ()
  (let ((diff-called nil)
        (smerge-called nil))
    (cl-letf (((symbol-function 'gptel-reinforce--show-diff-review)
               (lambda (&rest _args)
                 (setq diff-called t)
                 "diff-result"))
              ((symbol-function 'gptel-reinforce--show-smerge-review)
               (lambda (&rest _args)
                 (setq smerge-called t)
                 "smerge-result")))
      (should (equal (gptel-reinforce--review-text "title" "old" "new" nil) "new"))
      (should (equal (gptel-reinforce--review-text "title" "old" "new" 'diff) "diff-result"))
      (should (equal (gptel-reinforce--review-text "title" "old" "new" 'smerge) "smerge-result"))
      (should diff-called)
      (should smerge-called))))

(ert-deftest gptel-reinforce-summarize-writes-reviewed-summary ()
  (gptel-reinforce-test-with-temp-env
    (gptel-reinforce-test--register-database)
    (gptel-reinforce-register-artifact :name "artifact-1" :database "test-db" :type "prompt")
    (gptel-reinforce-db-record-feedback
     "test-db"
     '(:event-type "item-feedback"
       :item-key "item-1"
       :score 1
       :title "Useful item"))
    (let ((gptel-reinforce-summary-review-mode 'smerge))
      (cl-letf (((symbol-function 'gptel-reinforce-backend-send)
                 (lambda (_request callback)
                   (funcall callback "* Summary\n\nModel summary" '(:status "ok"))))
                ((symbol-function 'gptel-reinforce--review-text)
                 (lambda (&rest _args)
                   "* Summary\n\nEdited summary\n\n* Uncertainty\n\n- none\n\n* Notes\n")))
        (gptel-reinforce-summarize "artifact-1")))
    (let ((summary-record (gptel-reinforce-org-read-summary "artifact-1")))
      (should (string-match-p "Edited summary" (plist-get summary-record :body)))
      (should (= (plist-get summary-record :last-event-id) 1)))))

(ert-deftest gptel-reinforce-update-applies-reviewed-text-and-hooks ()
  (gptel-reinforce-test-with-temp-env
    (let (pre-text post-text)
      (gptel-reinforce-test--register-database)
      (gptel-reinforce-register-artifact
       :name "artifact-1"
       :database "test-db"
       :type "prompt"
       :pre-update-hook (lambda (_artifact _current-record candidate-text)
                          (setq pre-text candidate-text)
                          t)
       :post-update-hook (lambda (_artifact _version-ref _current-record candidate-text)
                           (setq post-text candidate-text)))
      (gptel-reinforce-org-write-summary
       "artifact-1"
       "* Summary\n\nNew signal\n\n* Uncertainty\n\n- none\n\n* Notes\n"
       3)
      (let ((gptel-reinforce-update-review-mode 'smerge))
        (cl-letf (((symbol-function 'gptel-reinforce-backend-send)
                   (lambda (_request callback)
                     (funcall callback "Model artifact text" '(:status "ok"))))
                  ((symbol-function 'gptel-reinforce--review-text)
                   (lambda (&rest _args)
                     "Edited artifact text")))
          (gptel-reinforce-update "artifact-1")))
      (let ((current-record (gptel-reinforce-org-read-current "artifact-1")))
        (should (equal pre-text "Edited artifact text"))
        (should (equal post-text "Edited artifact text"))
        (should (equal (plist-get current-record :text) "Edited artifact text"))
        (should (string-match-p "New signal" (plist-get current-record :applied-summary)))
        (should (stringp (plist-get current-record :version-ref)))))))

(ert-deftest gptel-reinforce-history-entries-are-listed-newest-first ()
  (gptel-reinforce-test-with-temp-env
    (gptel-reinforce-test--register-database)
    (gptel-reinforce-register-artifact :name "artifact-1" :database "test-db" :type "prompt")
    (let* ((initial-version (plist-get (gptel-reinforce-org-read-current "artifact-1") :version-ref))
           (_second-version
            (gptel-reinforce-org-write-history-entry
             "artifact-1"
             "Second text"
             :updated-at "2026-01-01T00:00:01+0000"
             :update-mode "manual-approved"))
           (entries (gptel-reinforce-org-list-history-entries "artifact-1")))
      (should (= 2 (length entries)))
      (should (equal (plist-get (car entries) :text) "Second text"))
      (should (equal (plist-get (cadr entries) :version-ref) initial-version)))))

(ert-deftest gptel-reinforce-rollback-restores-selected-history-and-runs-hooks ()
  (gptel-reinforce-test-with-temp-env
    (let (pre-text post-text rollback-source)
      (gptel-reinforce-test--register-database)
      (gptel-reinforce-register-artifact
       :name "artifact-1"
       :database "test-db"
       :type "prompt"
       :pre-update-hook (lambda (_artifact _current-record candidate-text)
                          (setq pre-text candidate-text)
                          t)
       :post-update-hook
       (lambda (artifact version-ref _current-record candidate-text)
         (setq post-text candidate-text)
         (setq rollback-source
               (plist-get
                (gptel-reinforce-org-read-history-entry artifact version-ref)
                :rollback-source-version-ref))))
      (gptel-reinforce-org-write-summary
       "artifact-1"
       "* Summary\n\nCurrent summary\n\n* Uncertainty\n\n- none\n\n* Notes\n"
       3)
      (let ((source-version
             (gptel-reinforce-org-write-history-entry
              "artifact-1"
              "Rollback target text"
              :updated-at "2026-01-01T00:00:02+0000"
              :summary-event-ref 2
              :update-mode "manual-approved")))
        (cl-letf (((symbol-function 'gptel-reinforce--review-text)
                   (lambda (&rest _args)
                     "Rollback target text")))
          (gptel-reinforce-rollback "artifact-1" source-version))
        (let* ((current-record (gptel-reinforce-org-read-current "artifact-1"))
               (new-version-ref (plist-get current-record :version-ref))
               (history-entry (gptel-reinforce-org-read-history-entry "artifact-1" new-version-ref)))
          (should (equal pre-text "Rollback target text"))
          (should (equal post-text "Rollback target text"))
          (should (equal rollback-source source-version))
          (should (equal (plist-get current-record :text) "Rollback target text"))
          (should (string-empty-p (plist-get current-record :applied-summary)))
          (should (equal (plist-get history-entry :update-mode) "rollback"))
          (should (equal (plist-get history-entry :summary-event-ref) 2))
          (should-not (equal new-version-ref source-version)))))))

(ert-deftest gptel-reinforce-elfeed-register-module-seeds-current-text ()
  (gptel-reinforce-test-with-temp-env
    (let ((gptel-reinforce-elfeed-score-file
           (expand-file-name "elfeed.score" temp-dir)))
      (make-directory (file-name-directory gptel-reinforce-elfeed-score-file) t)
      (with-temp-file gptel-reinforce-elfeed-score-file
        (insert "((title (:text \"lisp\") :value 10))\n"))
      (gptel-reinforce-register-elfeed-module)
      (should (gptel-reinforce-get-database gptel-reinforce-elfeed-database-name))
      (should (gptel-reinforce-get-artifact gptel-reinforce-elfeed-artifact-name))
      (should
       (string-match-p
        "lisp"
        (plist-get
         (gptel-reinforce-org-read-current gptel-reinforce-elfeed-artifact-name)
         :text))))))

(ert-deftest gptel-reinforce-elfeed-hooks-validate-and-write-score-file ()
  (gptel-reinforce-test-with-temp-env
    (let ((gptel-reinforce-elfeed-score-file
           (expand-file-name "elfeed.score" temp-dir)))
      (should (gptel-reinforce-elfeed-validate-score-file nil nil "((foo . 1))"))
      (should-not (gptel-reinforce-elfeed-validate-score-file nil nil "((foo . 1)"))
      (gptel-reinforce-elfeed-apply-score-file nil nil nil "((bar . 2))\n")
      (should
       (equal
        (with-temp-buffer
          (insert-file-contents gptel-reinforce-elfeed-score-file)
          (buffer-string))
        "((bar . 2))\n")))))

(ert-deftest gptel-reinforce-resolve-database-prefers-highest-priority-candidate ()
  (gptel-reinforce-test-with-temp-env
    (gptel-reinforce-register-database
     :name "low"
     :candidate-fn (lambda ()
                     '(:context (:item-key "item-low" :title "Low")
                       :priority 1
                       :label "Low priority")))
    (gptel-reinforce-register-database
     :name "high"
     :candidate-fn (lambda ()
                     '(:context (:item-key "item-high" :title "High")
                       :priority 10
                       :label "High priority")))
    (pcase-let ((`(,db . ,context)
                 (gptel-reinforce-resolve-database-and-context nil nil)))
      (should (equal (gptel-reinforce-database-name db) "high"))
      (should (equal (plist-get context :item-key) "item-high")))))

(ert-deftest gptel-reinforce-context-for-database-falls-back-to-context-fn ()
  (gptel-reinforce-test-with-temp-env
    (gptel-reinforce-register-database
     :name "ctx-db"
     :candidate-fn (lambda ()
                     '(:priority 5 :label "Context DB"))
     :context-fn (lambda ()
                   '(:item-key "item-from-context" :title "Context Title")))
    (let ((context (gptel-reinforce-context-for-database "ctx-db")))
      (should (equal (plist-get context :item-key) "item-from-context"))
      (should (equal (plist-get context :title) "Context Title")))))

(ert-deftest gptel-reinforce-db-overwrites-item-feedback-for-same-item ()
  (gptel-reinforce-test-with-temp-env
    (gptel-reinforce-test--register-database)
    (let ((first-id
           (gptel-reinforce-db-record-feedback
            "test-db"
            '(:event-type "item-feedback"
              :item-key "item-1"
              :score 1
              :title "First")))
          (second-id
           (gptel-reinforce-db-record-feedback
            "test-db"
            '(:event-type "item-feedback"
              :item-key "item-1"
              :score -1
              :title "Second"))))
      (should (= first-id second-id))
      (should (= 1 (gptel-reinforce-db-feedback-event-count "test-db")))
      (let ((event (car (gptel-reinforce-db-feedback-since "test-db" 0 nil))))
        (should (= (plist-get event :score) -1))
        (should (equal (plist-get event :title) "Second"))))))

(ert-deftest gptel-reinforce-db-overwrites-output-feedback-for-same-output ()
  (gptel-reinforce-test-with-temp-env
    (gptel-reinforce-test--register-database)
    (let ((first-id
           (gptel-reinforce-db-record-feedback
            "test-db"
            '(:event-type "output-feedback"
              :item-key "item-1"
              :artifact-name "artifact-1"
              :artifact-version-ref "v1"
              :output-id "output-1"
              :score 1
              :title "First output")))
          (second-id
           (gptel-reinforce-db-record-feedback
            "test-db"
            '(:event-type "output-feedback"
              :item-key "item-1"
              :artifact-name "artifact-1"
              :artifact-version-ref "v1"
              :output-id "output-1"
              :score 0
              :title "Updated output"))))
      (should (= first-id second-id))
      (should (= 1 (gptel-reinforce-db-feedback-event-count "test-db")))
      (let ((event (car (gptel-reinforce-db-feedback-since "test-db" 0 "artifact-1"))))
        (should (= (plist-get event :score) 0))
        (should (equal (plist-get event :output-id) "output-1"))
        (should (equal (plist-get event :title) "Updated output"))))))

(provide 'gptel-reinforce-tests)

;;; gptel-reinforce-tests.el ends here
