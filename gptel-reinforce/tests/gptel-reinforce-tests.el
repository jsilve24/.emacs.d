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
          (gptel-reinforce-config-root (expand-file-name "config/" temp-dir))
          (gptel-reinforce--databases (make-hash-table :test #'equal))
          (gptel-reinforce--artifacts (make-hash-table :test #'equal)))
     (unwind-protect
         (progn ,@body)
       (delete-directory temp-dir t))))

(defun gptel-reinforce-test--register-database (&optional name db-path root-dir)
  "Register a test database with NAME, DB-PATH, and ROOT-DIR."
  (gptel-reinforce-register-database
   :name (or name "test-db")
   :candidate-fn (lambda ()
                   '(:context (:item-key "item-1" :title "Item")
                     :priority 0
                     :label "Test DB"))
   :db-path (or db-path (expand-file-name "test-db.sqlite" gptel-reinforce-state-root))
   :root-dir (or root-dir (expand-file-name "test-db/" gptel-reinforce-config-root))))

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

(ert-deftest gptel-reinforce-elfeed-register-module-seeds-current-text ()
  (gptel-reinforce-test-with-temp-env
    (let ((gptel-reinforce-elfeed-score-file
           (expand-file-name "elfeed.score" gptel-reinforce-config-root)))
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
           (expand-file-name "elfeed.score" gptel-reinforce-config-root)))
      (should (gptel-reinforce-elfeed-validate-score-file nil nil "((foo . 1))"))
      (should-not (gptel-reinforce-elfeed-validate-score-file nil nil "((foo . 1)"))
      (gptel-reinforce-elfeed-apply-score-file nil nil nil "((bar . 2))\n")
      (should
       (equal
        (with-temp-buffer
          (insert-file-contents gptel-reinforce-elfeed-score-file)
          (buffer-string))
        "((bar . 2))\n")))))

(provide 'gptel-reinforce-tests)

;;; gptel-reinforce-tests.el ends here
