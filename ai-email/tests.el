;;; tests.el --- Regression tests for ai-email helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(ert-deftest jds/ai-email-sanitize-response-extracts-tagged-reply ()
  (should
   (equal
    (jds/ai-email--sanitize-response
     (concat
      "Planning text.\n\n"
      "<reply>Hi,\n\n"
      "Monday at 3:00 PM works well for me. Does that time suit you?\n\n"
      "Best,\nMe</reply>"))
    (concat
     "Hi,\n\n"
     "Monday at 3:00 PM works well for me. Does that time suit you?\n\n"
     "Best,\nMe"))))

(ert-deftest jds/ai-email-sanitize-response-keeps-final-email-shaped-draft ()
  (should
   (equal
    (jds/ai-email--sanitize-response
     (concat
      "Thanks for the update, Francesca--safe travels, and I'm sorry about the flight cancellation. Both dates work for me.\n\n"
      "I'm free Friday, April 17 after 1pm, and Monday, April 20 between 11am-1pm or after 3pm. Given your preference, how about Monday, April 20 at 3:00 PM? If that doesn't work, I'm also open to Friday afternoon.\n\n"
      "Looking forward to discussing the ideas.\n\n"
      "Hi Francesca,\n\n"
      "Thanks for letting us know about the flight situation--glad you'll be back by Monday. I can meet Monday, April 20 at 3pm if that works for you. Otherwise, Friday, April 17 afternoon also works for me.\n\n"
      "Let me know which works best.\n\n"
      "Best,\nJustin"))
    (concat
     "Hi Francesca,\n\n"
     "Thanks for letting us know about the flight situation--glad you'll be back by Monday. I can meet Monday, April 20 at 3pm if that works for you. Otherwise, Friday, April 17 afternoon also works for me.\n\n"
     "Let me know which works best.\n\n"
     "Best,\nJustin"))))

(ert-deftest jds/ai-email-sanitize-response-preserves-greetingless-reply ()
  (should
   (equal
    (jds/ai-email--sanitize-response
     (concat
      "Thanks for the update.\n\n"
      "Monday at 3:00 PM works for me.\n\n"
      "Looking forward to it."))
    (concat
     "Thanks for the update.\n\n"
     "Monday at 3:00 PM works for me.\n\n"
     "Looking forward to it."))))

(ert-deftest jds/ai-email-reply-has-multiple-drafts-detects-trailing-text-after-signoff ()
  (should
   (jds/ai-email--reply-has-multiple-drafts-p
    (concat
     "Thanks for the note about your flight--hope the rebooking works out smoothly. Monday, April 20 at 3:00 PM (in person) works well for me. Does that time suit you?\n\n"
     "Best,\n"
     "Justin\n\n"
     "Thanks for letting us know about the flight cancellation--that's frustrating. I'd like to meet before I dive deeper into the revisions.\n\n"
     "How about Monday, April 20 at 3pm? That aligns with your \"after 3pm\" window and works well for my schedule.\n\n"
     "If that doesn't suit you, Friday, April 17 at 3pm is another option, though I know you mentioned after 1pm that day.\n\n"
     "Let me know which works better."))))

(ert-deftest jds/ai-email-request-cleaned-response-runs-single-pass ()
  (let ((calls nil)
        (final-text nil))
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (prompt &rest plist)
                 (push (list :prompt prompt
                             :system (plist-get plist :system))
                       calls)
                 (let ((callback (plist-get plist :callback)))
                   (funcall callback
                            (concat
                             "Hi,\n\n"
                             "Friday at 1:10 PM works.\n\n"
                             "Best,\nMe\n\n"
                             "The user asked for 3pm.\n\n"
                             "Hi,\n\n"
                             "Monday at 3:00 PM works.\n\n"
                             "Best,\nMe")
                            '(:status "ok"))))))
      (jds/ai-email--request-cleaned-response
       "Draft a reply that prefers 3pm."
       "You are a professional email assistant."
       (current-buffer)
       (lambda (text) (setq final-text text))
       nil))
    (should (equal final-text "Hi,\n\nMonday at 3:00 PM works.\n\nBest,\nMe"))
    (should (= (length calls) 1))
    (let ((request (car calls)))
      (should (equal (plist-get request :prompt)
                     "Draft a reply that prefers 3pm."))
      (should (equal (plist-get request :system)
                     "You are a professional email assistant.")))))

(ert-deftest jds/ai-email-consume-pending-compose-request-runs-once ()
  (let ((calls 0)
        (captured nil))
    (with-temp-buffer
      (insert "Hi\n\nQuoted thread")
      (setq jds/ai-email--pending-compose-request
            (list
             :prompt-builder (lambda (_content) "Prompt")
             :system "System"
             :callback (lambda (prompt system _buf _pos _tools)
                         (setq calls (1+ calls))
                         (setq captured (list prompt system)))))
      (cl-letf (((symbol-function 'message-goto-body)
                 (lambda () (goto-char (point-min)))))
        (jds/ai-email--consume-pending-compose-request)
        (jds/ai-email--consume-pending-compose-request)))
    (should (= calls 1))
    (should (equal captured '("Prompt" "System")))
    (should-not jds/ai-email--pending-compose-request)))

(ert-deftest jds/ai-email-insert-response-after-org-msg-preamble ()
  (with-temp-buffer
    (insert "To: test@example.com\nSubject: Re: Test\n\n"
            "#+OPTIONS: html-postamble:nil num:nil\n"
            "#+STARTUP: hidestars indent inlineimages\n"
            ":PROPERTIES:\n"
            ":reply-to: nil\n"
            ":END:\n"
            "Alice writes:\n\n> Hello\n")
    (cl-letf (((symbol-function 'message-goto-body)
               (lambda ()
                 (goto-char (point-min))
                 (re-search-forward "\n\n" nil t))))
      (let ((pos (copy-marker (progn
                                (goto-char (point-min))
                                (message-goto-body)
                                (point)))))
        (jds/ai-email--insert-response-at-point
         (current-buffer) pos "Thanks, Friday works for me.")
        (should
         (equal (buffer-string)
                (concat
                 "To: test@example.com\nSubject: Re: Test\n\n"
                 "#+OPTIONS: html-postamble:nil num:nil\n"
                 "#+STARTUP: hidestars indent inlineimages\n"
                 ":PROPERTIES:\n"
                 ":reply-to: nil\n"
                 ":END:\n"
                 "Thanks, Friday works for me.\n\n"
                 "Alice writes:\n\n> Hello\n")))))))

(ert-deftest jds/ai-email-scheduling-system-prompt-requires-explicit-consensus ()
  (cl-letf (((symbol-function 'jds/ai-email--scheduling-artifact-text) (lambda () nil))
            ((symbol-function 'jds/org-calendar--format-date) (lambda (&rest _) "2026-04-14")))
    (let ((prompt (jds/ai-email--scheduling-system-prompt)))
      (should (string-match-p
               "explicit acceptance from everyone whose availability matters"
               prompt))
      (should (string-match-p
               "A proposal from one attendee is not a confirmation"
               prompt)))))

(ert-deftest jds/org-calendar-time-constraints-match-exact-and-bounds ()
  (let ((three-pm (encode-time 0 0 15 20 4 2026))
        (one-ten (encode-time 0 10 13 17 4 2026)))
    (should (jds/org-calendar--time-constraints-match-p three-pm "15:00" nil nil))
    (should-not (jds/org-calendar--time-constraints-match-p one-ten "15:00" nil nil))
    (should (jds/org-calendar--time-constraints-match-p three-pm nil "15:00" nil))
    (should-not (jds/org-calendar--time-constraints-match-p one-ten nil "15:00" nil))
    (should (jds/org-calendar--time-constraints-match-p one-ten nil "13:00" "13:30"))
    (should-not (jds/org-calendar--time-constraints-match-p three-pm nil nil "13:30"))))

(provide 'ai-email/tests)
;;; tests.el ends here
