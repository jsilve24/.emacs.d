;;; email-maple-tests.el --- Maple mail draft regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'message)
(load (expand-file-name "email-helpers.el" (file-name-directory (or load-file-name buffer-file-name))) nil t)

(define-derived-mode jds/mu4e-test-view-mode fundamental-mode "Test-view")
(put 'jds/mu4e-test-view-mode 'derived-mode-parent 'mu4e-view-mode)

(defmacro jds/mu4e-test-with-draft (headers &rest body)
  "Run BODY in an unsent Message draft containing HEADERS."
  (declare (indent 1))
  `(with-temp-buffer
     (message-mode)
     (insert ,headers "\n" mail-header-separator "\nBody.\n")
     ,@body))

(ert-deftest jds/mu4e-maple-cc-preserves-recipients-and-body ()
  (jds/mu4e-test-with-draft "From: Justin <jds6696@psu.edu>\nTo: Bob <bob@example.org>\nCc: \"Smith, Alice\" <alice@example.org>,\n Carol <carol@example.org>\nSubject: Test"
    (goto-char (point-max))
    (let ((before (point)))
      (jds/mu4e-maple-add-cc)
      (should (string-suffix-p "Body.\n" (buffer-string)))
      (should (string-match-p "alice@example.org" (message-field-value "Cc")))
      (should (string-match-p "carol@example.org" (message-field-value "Cc")))
      (should (string-match-p jds/mu4e-maple-address (message-field-value "Cc")))
      (should (equal "Bob <bob@example.org>" (message-field-value "To")))
      (should (= (point) (point-max)))
      (should (> (point) before)))))

(ert-deftest jds/mu4e-maple-cc-idempotent-and-case-insensitive ()
  (dolist (header '("To" "Cc"))
    (jds/mu4e-test-with-draft (concat header ": MAPLE.SILVERMAN.ASSISTANT@GMAIL.COM\nSubject: Test")
      (let ((before (buffer-string)))
        (jds/mu4e-maple-add-cc)
        (jds/mu4e-maple-add-cc)
        (should (equal before (buffer-string)))))))

(ert-deftest jds/mu4e-maple-cc-creates-field-and-ignores-body-address ()
  (jds/mu4e-test-with-draft "To: bob@example.org\nSubject: Test"
    (goto-char (point-max))
    (insert jds/mu4e-maple-address)
    (jds/mu4e-maple-add-cc)
    (should (equal (message-field-value "Cc")
                   (concat "Maple <" jds/mu4e-maple-address ">")))))

(ert-deftest jds/mu4e-maple-cc-supports-org-msg-mode ()
  (jds/mu4e-test-with-draft "To: bob@example.org\nSubject: Test"
    (setq major-mode 'org-msg-edit-mode)
    (goto-char (point-max))
    (insert "#+OPTIONS: toc:nil\n:PROPERTIES:\n:attachment: (\"file.pdf\")\n:END:\n")
    (let ((body (buffer-substring-no-properties (save-excursion (message-goto-body) (point)) (point-max))))
      (jds/mu4e-maple-add-cc)
      (should (equal body (buffer-substring-no-properties (save-excursion (message-goto-body) (point)) (point-max)))))))

(ert-deftest jds/mu4e-maple-commands-reject-wrong-mode ()
  (with-temp-buffer
    (should-error (jds/mu4e-maple-add-cc) :type 'user-error)
    (should-error (jds/mu4e-maple-forward) :type 'user-error)
    (should-error (jds/mu4e-maple-reply) :type 'user-error)))

(ert-deftest jds/mu4e-maple-reply-uses-existing-reply-flow ()
  (let ((draft (generate-new-buffer " *maple-test-reply*")))
    (unwind-protect
        (progn
          (with-current-buffer draft
            (message-mode)
            (insert "To: bob@example.org\nCc: alice@example.org\nSubject: Re: Test\n"
                    mail-header-separator "\nReply\n> Original\n"))
          (with-temp-buffer
            (jds/mu4e-test-view-mode)
            (cl-letf (((symbol-function 'jds/mu4e-compose-reply) (lambda (&rest args) (should-not args) draft))
                      ((symbol-function 'message-send) (lambda (&rest _) (ert-fail "Must not send"))))
              (jds/mu4e-maple-reply)))
          (with-current-buffer draft
            (should (string-match-p jds/mu4e-maple-address (message-field-value "Cc")))
            (should (string-suffix-p "Reply\n> Original\n" (buffer-string)))))
      (kill-buffer draft))))

(ert-deftest jds/mu4e-maple-forward-preserves-mime-and-clears-only-envelope ()
  (let ((draft (generate-new-buffer " *maple-test-forward*"))
        (mu4e-compose-mode-hook '(ignore org-msg-post-setup))
        (message-forward-as-mime nil)
        (parent '(:from ((:name "Bob" :email "bob@example.org"))
                  :to ((:email "jds6696@psu.edu")) :cc ((:email "alice@example.org"))
                  :subject "Meeting" :date (0 0 0 0) :message-id "original@example.org")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (jds/mu4e-test-view-mode)
            (cl-letf (((symbol-function 'mu4e-message-at-point) (lambda (&rest _) parent))
                      ((symbol-function 'mu4e-compose-forward)
                       (lambda ()
                         (should jds/mu4e-preserve-forward-mime)
                         (should message-forward-as-mime)
                         (should-not message-forward-show-mml)
                         (should-not message-forward-ignored-headers)
                         (should (equal mu4e-compose-mode-hook '(ignore)))
                         (with-current-buffer draft
                           (message-mode)
                           (insert "From: jds6696@psu.edu\nTo: wrong@example.org\nCc: wrong@example.org\nBcc: hidden@example.org\nSubject: Fwd: Meeting\n"
                                   mail-header-separator
                                   "\n<#part type=message/rfc822>\nFrom: bob@example.org\nCc: alice@example.org\nMessage-ID: <original@example.org>\n\nAttachment bytes\n<#/part>\n"))
                         draft))
                      ((symbol-function 'message-send) (lambda (&rest _) (ert-fail "Must not send"))))
              (jds/mu4e-maple-forward)))
          (should (equal mu4e-compose-mode-hook '(ignore org-msg-post-setup)))
          (should-not message-forward-as-mime)
          (with-current-buffer draft
            (should (equal (message-field-value "To") (concat "Maple <" jds/mu4e-maple-address ">")))
            (should-not (message-field-value "Cc"))
            (should-not (message-field-value "Bcc"))
            (should (equal (message-field-value "From") "jds6696@psu.edu"))
            (should (= (point) (save-excursion (message-goto-body) (point))))
            (should (string-match-p "Message-ID: original@example.org" (buffer-string)))
            (should (string-suffix-p "<#part type=message/rfc822>\nFrom: bob@example.org\nCc: alice@example.org\nMessage-ID: <original@example.org>\n\nAttachment bytes\n<#/part>\n" (buffer-string)))))
      (kill-buffer draft))))

(ert-deftest jds/mu4e-maple-forward-restores-settings-on-error ()
  (let ((mu4e-compose-mode-hook '(org-msg-post-setup))
        (message-forward-as-mime nil))
    (with-temp-buffer
      (jds/mu4e-test-view-mode)
      (cl-letf (((symbol-function 'mu4e-message-at-point) (lambda () '(:subject "test")))
                ((symbol-function 'mu4e-compose-forward) (lambda () (error "Cancelled"))))
        (should-error (jds/mu4e-maple-forward))))
    (should (equal mu4e-compose-mode-hook '(org-msg-post-setup)))
    (should-not message-forward-as-mime)
    (should-not jds/mu4e-preserve-forward-mime)))

(ert-deftest jds/mu4e-maple-cc-restores-narrowing ()
  (jds/mu4e-test-with-draft "To: bob@example.org\nSubject: Test"
    (message-goto-body)
    (narrow-to-region (point) (point-max))
    (let ((body (buffer-string)))
      (jds/mu4e-maple-add-cc)
      (should (buffer-narrowed-p))
      (should (equal body (buffer-string))))))

(ert-deftest jds/mu4e-maple-draft-reply-preserves-existing-edits ()
  (jds/mu4e-test-with-draft "To: bob@example.org\nSubject: My draft"
    (cl-letf (((symbol-function 'jds/mu4e-compose-reply)
               (lambda () (ert-fail "Do not replace the existing draft"))))
      (jds/mu4e-maple-reply))
    (should (equal (message-field-value "To") "bob@example.org"))
    (should (string-suffix-p "Body.\n" (buffer-string)))))

(ert-deftest jds/mu4e-maple-draft-forward-requires-original ()
  (jds/mu4e-test-with-draft "To: bob@example.org\nSubject: Brand new"
    (let ((mu4e-compose-parent-message nil)
          (before (buffer-string)))
      (should-error (jds/mu4e-maple-forward) :type 'user-error)
      (should (equal before (buffer-string))))))

(ert-deftest jds/mu4e-maple-draft-forward-keeps-unsent-draft ()
  (let ((forward (generate-new-buffer " *maple-forward-copy*")))
    (unwind-protect
        (jds/mu4e-test-with-draft "To: bob@example.org\nSubject: Edited reply"
          (let* ((mu4e-compose-parent-message '(:message-id "source@psu.edu" :subject "Original"))
                 (original mu4e-compose-parent-message)
                 (before (buffer-string)))
            (cl-letf (((symbol-function 'mu4e-compose-forward)
                       (lambda ()
                         (should (equal (mu4e-message-at-point) original))
                         (with-current-buffer forward
                           (message-mode)
                           (insert "To: \nSubject: Fwd: Original\n" mail-header-separator "\nOriginal MIME\n"))
                         forward)))
              (jds/mu4e-maple-forward))
            (should (equal before (buffer-string)))))
      (kill-buffer forward))))

(ert-deftest jds/mu4e-maple-native-message-forward-keeps-source-mime ()
  ;; Exercise the real Message forwarding engine used by Mu4e.  Mu4e's
  ;; server/account/window integration still needs a live-machine smoke test.
  (let ((message-forward-as-mime t)
        (message-forward-show-mml nil)
        (message-forward-ignored-headers nil)
        (message-forward-included-headers nil)
        (message-signature nil)
        (message-user-fqdn "example.org")
        (user-mail-address "jds6696@psu.edu")
        (source (generate-new-buffer " *maple-source*"))
        draft)
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "From: Bob <bob@example.org>\nTo: jds6696@psu.edu\nCc: alice@example.org\n"
                    "Date: Thu, 19 Nov 2026 12:00:00 -0500\nSubject: Test attachment\n"
                    "Message-ID: <original@example.org>\nMIME-Version: 1.0\n"
                    "Content-Type: multipart/mixed; boundary=test-boundary\n\n"
                    "--test-boundary\nContent-Type: text/plain\n\nHello.\n"
                    "--test-boundary\nContent-Type: application/octet-stream\n"
                    "Content-Disposition: attachment; filename=sample.bin\n"
                    "Content-Transfer-Encoding: base64\n\nAAECAwQF\n--test-boundary--\n")
            (cl-letf (((symbol-function 'message-pop-to-buffer)
                       (lambda (name &rest _)
                         (setq draft (get-buffer-create name))
                         (set-buffer draft))))
              (message-forward)))
          (with-current-buffer draft
            (should (string-match-p "Message-ID: <original@example.org>" (buffer-string)))
            (should (string-match-p "Cc: alice@example.org" (buffer-string)))
            (should (string-match-p "filename=sample.bin" (buffer-string)))
            (should (string-match-p "AAECAwQF" (buffer-string)))
            (should (string-match-p "type=message/rfc822" (buffer-string)))))
      (when (buffer-live-p draft) (kill-buffer draft))
      (when (buffer-live-p source) (kill-buffer source)))))
