;;; calendar-exchange-tests.el --- tests for calendar-exchange -*- lexical-binding: t; -*-

(require 'ert)
(require 'calendar-exchange)

(ert-deftest jds/exchange-calendar-state-roundtrip ()
  (let* ((directory (make-temp-file "calendar-exchange-state-" t))
         (jds/exchange-calendar-state-file
          (expand-file-name "nested/state.sexp" directory))
         (state (jds/exchange-calendar--state-empty)))
    (unwind-protect
        (progn
          (setq state
                (jds/exchange-calendar--state-put
                 state '(:local-id "org-1" :href "http://example/a.EML"
                         :etag "one" :remote-uid "exchange-1"
                         :local-hash "hash" :title "Test")))
          (jds/exchange-calendar--save-state state)
          (should (equal state (jds/exchange-calendar--load-state)))
          (should (= #o600
                     (logand #o777
                             (file-modes jds/exchange-calendar-state-file)))))
      (delete-directory directory t))))

(ert-deftest jds/exchange-calendar-replaces-only-event-uid ()
  (let* ((event "BEGIN:VEVENT\r\nUID:local-id\r\nSUMMARY:UID: in title\r\nEND:VEVENT\r\n")
         (updated (jds/exchange-calendar--replace-event-uid
                   event "remote-id")))
    (should (string-match-p "UID:remote-id" updated))
    (should (string-match-p "SUMMARY:UID: in title" updated))
    (should-not (string-match-p "UID:local-id" updated))))

(ert-deftest jds/exchange-calendar-normalizes-org-export-uid-prefix ()
  (should (equal "calendar-id"
                 (jds/exchange-calendar--org-export-uid
                  "TS12-calendar-id")))
  (should (equal "calendar-id"
                 (jds/exchange-calendar--org-export-uid "calendar-id"))))

(ert-deftest jds/exchange-calendar-plan-separates-local-and-remote-identity ()
  (let* ((href "http://localhost/calendar/AAMk.EML")
         (event '(:local-id "org-id" :title "Test" :hash "same"))
         (record `(:local-id "org-id" :title "Test" :href ,href
                   :etag "etag-1" :remote-uid "AAMk" :local-hash "same"))
         (state (list :version jds/exchange-calendar--state-version
                      :calendar-url jds/exchange-calendar-url
                      :entries (list record)))
         (remote (list :href href :etag "etag-1")))
    (should (eq 'unchanged
                (plist-get
                 (car (jds/exchange-calendar--plan
                       (list event) (list remote) state))
                 :kind)))
    (setf (plist-get event :hash) "local-change")
    (should (eq 'update-remote
                (plist-get
                 (car (jds/exchange-calendar--plan
                       (list event) (list remote) state))
                 :kind)))
    (setf (plist-get remote :etag) "etag-2")
    (should (eq 'conflict
                (plist-get
                 (car (jds/exchange-calendar--plan
                       (list event) (list remote) state))
                 :kind)))))

(ert-deftest jds/exchange-calendar-plan-detects-independent-new-items ()
  (let* ((local '(:local-id "local" :title "Local" :hash "hash"))
         (remote '(:href "http://localhost/calendar/remote.EML"
                   :etag "etag"))
         (state (jds/exchange-calendar--state-empty))
         (kinds (mapcar
                 (lambda (action) (plist-get action :kind))
                 (jds/exchange-calendar--plan
                  (list local) (list remote) state))))
    (should (equal kinds '(create-remote import-remote)))))

(ert-deftest jds/exchange-calendar-event-hash-ignores-server-metadata ()
  (let ((first "BEGIN:VEVENT\nUID:id\nDTSTAMP:20261005T120000Z\nSEQUENCE:1\nSUMMARY:Test\nEND:VEVENT\n")
        (second "BEGIN:VEVENT\nUID:id\nDTSTAMP:20261005T130000Z\nSEQUENCE:2\nSUMMARY:Test\nEND:VEVENT\n"))
    (should (equal (jds/exchange-calendar--event-hash first)
                   (jds/exchange-calendar--event-hash second)))))

(provide 'calendar-exchange-tests)

;;; calendar-exchange-tests.el ends here
