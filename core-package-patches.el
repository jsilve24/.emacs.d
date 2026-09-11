;;; core-package-patches.el --- Local package fixes -*- lexical-binding: t; -*-

(defun jds/apply-package-patch (package &rest _)
  "Apply PACKAGE's compatibility patch before Straight builds it.
Already-applied patches are left alone.  Upstream conflicts produce a
warning without overwriting package changes or interrupting startup."
  (when (member package '("avy-flash" "ol-emacs-slack" "consult-mu"))
    (let ((default-directory
           (expand-file-name (concat "straight/repos/" package "/")
                             user-emacs-directory))
          (patch (expand-file-name (concat "patches/" package ".patch")
                                   user-emacs-directory)))
      (when (file-directory-p default-directory)
        (condition-case err
            (with-temp-buffer
              (cond
               ((zerop (call-process "git" nil t nil
                                     "apply" "--reverse" "--check" patch)))
               ((and (zerop (call-process "git" nil t nil "apply" "--check" patch))
                     (zerop (call-process "git" nil t nil "apply" patch))))
               (t (display-warning 'jds-package-patches
                                   (format "Review compatibility patch: %s" patch)))))
          (error (display-warning 'jds-package-patches
                                  (error-message-string err))))))))

(add-hook 'straight-use-package-prepare-functions #'jds/apply-package-patch)

(provide 'core-package-patches)
;;; core-package-patches.el ends here
