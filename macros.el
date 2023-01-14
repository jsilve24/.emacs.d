;;; macros.el --- jds core macros -*- lexical-binding: t -*-

;;https://emacs.stackexchange.com/questions/54500/how-to-add-a-locally-override-the-message-function
;;;###autoload
(defmacro jds~with-temp-advice (fn-orig where fn-advice &rest body)
  "Execute BODY with advice temporarily enabled."
  `(let ((fn-advice-var ,fn-advice))
     (unwind-protect
	 (progn
           (advice-add ,fn-orig ,where fn-advice-var)
           ,@body)
       (advice-remove ,fn-orig fn-advice-var))))

;; https://emacs.stackexchange.com/questions/3323/is-there-any-way-to-run-a-hook-function-only-once
;;;###autoload
(defmacro jds~add-hook-run-once (hook function &optional append local)
  "Like add-hook, but remove the hook after it is called"
  (let ((sym (make-symbol "#once")))
    `(progn
       (defun ,sym ()
         (remove-hook ,hook ',sym ,local)
         (funcall ,function))
       (add-hook ,hook ',sym ,append ,local))))

;; https://emacs.stackexchange.com/questions/7653/elisp-code-to-check-for-internet-connection
;;;###autoload
(defun jds~internet-up-p (&optional host)
    (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1" 
                       (if host host "www.google.com"))))
