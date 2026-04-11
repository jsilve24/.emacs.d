;;; reply.el --- AI email reply workflows -*- lexical-binding: t; -*-

;;; AI email reply ---------------------------------------------------------

(defvar jds/ai-email-reply-instruction-history nil
  "Minibuffer history for custom AI email reply instructions.")

(defun jds/ai-email--read-reply-instructions ()
  "Return optional custom instructions for an AI email reply."
  (let ((instructions
         (read-string "Reply instructions (optional): "
                      nil
                      'jds/ai-email-reply-instruction-history)))
    (unless (string-empty-p (string-trim instructions))
      (string-trim instructions))))

(defun jds/mu4e-ai-draft-reply (&optional custom-instructions)
  "Reply to message at point with an AI-drafted body.

When CUSTOM-INSTRUCTIONS is non-nil, include it as extra drafting guidance."
  (interactive (list (jds/ai-email--read-reply-instructions)))
  (pcase-let* ((`(:message ,message :from ,from-str :subject ,subject . ,_)
                (jds/ai-email--mu4e-message-metadata))
               (system (concat
                        "You are a professional email assistant. Write clear, concise replies.\n"
                        "Return only the reply body text.\n"
                        "Do not include a subject line, commentary, reasoning, tool narration, or code fences."))
               (context (jds/ai-email--reinforce-context-for-message
                         message "reply")))
    (jds/ai-email--compose-mu4e-reply-with-ai
     (lambda (content)
       (format
        "Draft a professional reply to this email from %s (subject: \"%s\").%s\nCompose buffer (includes quoted original):\n\n%s\n\nReturn only the reply body text."
        from-str
        subject
        (if custom-instructions
            (format "\nAdditional instructions: %s" custom-instructions)
          "")
        content))
     system
     (lambda (response info _prompt _system buf pos)
       (jds/ai-email--insert-if-final-string
        response info buf pos jds/ai-email-reinforce-reply-artifact))
     nil
     jds/ai-email-reinforce-reply-database
     context)))


;;; AI scheduling reply ----------------------------------------------------

(defun jds/ai-email--read-scheduling-context ()
  "Prompt for scheduling context shared by scheduling helpers."
  (read-string "Scheduling context (priority, duration, notes): "))

(defun jds/ai-email--scheduling-preferences-text ()
  "Return scheduling preference lines derived from `jds/scheduling-weekday-mode-alist'."
  (let* ((day-names ["Sundays" "Mondays" "Tuesdays" "Wednesdays"
                     "Thursdays" "Fridays" "Saturdays"])
         (zoom-days nil)
         (inperson-days nil))
    (dolist (pair jds/scheduling-weekday-mode-alist)
      (let ((dow (car pair))
            (mode (cdr pair)))
        (cond
         ((equal mode "zoom")      (push (aref day-names dow) zoom-days))
         ((equal mode "in_person") (push (aref day-names dow) inperson-days)))))
    (let ((lines nil))
      (when inperson-days
        (push (format "- %s: prefer in-person meetings.\n"
                      (string-join (nreverse inperson-days) " & "))
              lines))
      (when zoom-days
        (push (format "- %s: prefer Zoom meetings.\n"
                      (string-join (nreverse zoom-days) " & "))
              lines))
      (if lines
          (concat "Scheduling preferences:\n" (string-join (nreverse lines) ""))
        ""))))

(defun jds/ai-email--scheduling-system-prompt ()
  "Return the shared system prompt for scheduling-related assistants."
  (let* ((today-time (current-time))
         (today (format-time-string "%Y-%m-%d (%A, %B %d, %Y)" today-time))
         (default-start (jds/org-calendar--format-date today-time))
         (default-end (jds/org-calendar--format-date
                       (time-add today-time
                                 (days-to-time jds/scheduling-default-search-days))))
         (prefs (jds/ai-email--scheduling-preferences-text)))
    (concat
     "You are a scheduling assistant helping draft meeting emails.\n"
     "Today is " today ".\n"
     "If the thread gives no date range, use "
     default-start " through " default-end ".\n"
     "If the meeting length is unspecified, assume 30 minutes.\n\n"
     (when (not (string-empty-p prefs)) (concat prefs "\n"))
     "Before proposing times, you must call find_free_times.\n"
     "Prefer availability_windows when they give a clearer summary than isolated slots.\n"
     "If one day has several adjacent openings, summarize them as windows.\n"
     "When listing availability windows, group them by day using one bullet per day and at most 2 windows per bullet.\n"
     "Format exactly like this: Thursday, April 9: 9:00--10:00 AM; 10:30 AM--1:00 PM\n"
     "Do not repeat the date within a bullet, use the word \"between,\" add prose inside bullets, or make bullets longer than one line.\n"
     "For these grouped availability bullets only, you may reformat availability_windows into that layout, but preserve the returned day/date text and exact start/end times.\n"
     "When offering options, prefer coverage across multiple days instead of concentrating everything on one day.\n"
     "Otherwise use returned display strings verbatim. Do not infer weekdays, do date arithmetic, invent times, invent meeting modes, or restate returned times in different words.\n"
     "If the tool returns no_availability: true, tell the user there are no free slots in that window and ask them to specify a different date range. Do not propose any times.\n")))

(defun jds/ai-email--scheduling-context-suffix (ctx)
  "Return a formatted context suffix for scheduling prompt text from CTX."
  (if (string-empty-p ctx)
      ""
    (format "\nContext: %s" ctx)))

(defun jds/ai-email--availability-snippet-prompt (ctx)
  "Return the standalone availability prompt text for CTX."
  (format
   (concat
    "Write a concise availability snippet I can paste into a message.%s\n"
    "Call find_free_times before answering.\n"
    "Prefer summarizing returned availability windows verbatim, and include multiple days when possible.\n"
    "When listing availability windows, group them by day using one bullet per day, at most 2 windows per bullet, in this exact format: Thursday, April 9: 9:00--10:00 AM; 10:30 AM--1:00 PM. Do not repeat the date within a bullet, do not use the word \"between\", do not add prose inside bullets, and keep each bullet to one line.\n"
    "Return only the availability snippet.")
   (jds/ai-email--scheduling-context-suffix ctx)))

(defun jds/ai-email--handle-availability-snippet-response
    (response info prompt system buf pos &optional retries-left)
  "Handle standalone availability snippet RESPONSE."
  (jds/ai-email--handle-retrying-insert-response
   response info prompt system buf pos retries-left
   (concat
    "Return only the availability snippet. No greeting, commentary, reasoning, tool narration, or code fences.\n"
    "Your previous response was invalid because it described what you would do instead of returning the snippet.\n"
    "You have already checked availability.\n"
    "Write the snippet now.\n"
    "If you list times, they must be exact slot display strings returned by find_free_times or grouped availability windows that preserve the returned day/date text and exact start/end times.")
   "Availability snippet suppressed: model returned planning text instead of the snippet."
   (list jds~gptel-find-free-times-tool)))

(defun jds/ai-email-insert-availability-snippet ()
  "Insert a formatted availability snippet at point."
  (interactive)
  (let* ((ctx (jds/ai-email--read-scheduling-context))
         (buf (current-buffer))
         (pos (copy-marker (point)))
         (prompt (jds/ai-email--availability-snippet-prompt ctx))
         (system (concat
                  (jds/ai-email--scheduling-system-prompt)
                  "Return only the availability snippet. No greeting, commentary, reasoning, tool narration, or code fences.\n")))
    (jds/ai-email--request-inserting-response
     prompt system buf
     (lambda (response info)
       (jds/ai-email--handle-availability-snippet-response
        response info prompt system buf pos 1))
     (list jds~gptel-find-free-times-tool))))

(defun jds/mu4e-ai-scheduling-reply ()
  "Reply to message at point with an AI-drafted scheduling response.
Prompts for custom context. Uses validated availability slots rather than raw calendar dumps."
  (interactive)
  (let* ((ctx      (jds/ai-email--read-scheduling-context))
         (meta     (jds/ai-email--mu4e-message-metadata))
         (message  (plist-get meta :message))
         (from-str (plist-get meta :from))
         (subject  (plist-get meta :subject))
         (context  (jds/ai-email--reinforce-context-for-message
                    message "scheduling-reply"))
         (system   (concat
                    (jds/ai-email--scheduling-system-prompt)
                    "Return only the reply body text. No subject line, commentary, reasoning, tool narration, or code fences.")))
    (jds/ai-email--compose-mu4e-reply-with-ai
     (lambda (content)
       (format
        (concat
         "Draft a scheduling reply for this email from %s (subject: \"%s\").%s\n"
         "Compose buffer (includes quoted original):\n\n%s\n\n"
         "If you propose times, call find_free_times first.\n"
         "Prefer summarizing returned availability windows verbatim, and include multiple days when possible.\n"
         "When listing availability windows, group them by day using one bullet per day, at most 2 windows per bullet, in this exact format: Thursday, April 9: 9:00--10:00 AM; 10:30 AM--1:00 PM. Do not repeat the date within a bullet, do not use the word \"between\", do not add prose inside bullets, and keep each bullet to one line.\n"
         "Return only the reply body.")
        from-str subject
        (jds/ai-email--scheduling-context-suffix ctx)
        content))
     system
     (lambda (response info prompt system buf pos)
       (jds/ai-email--handle-retrying-insert-response
        response info prompt system buf pos 1
        (concat
         "\nYour previous response was invalid because it described what you would do instead of drafting the email.\n"
         "You have already checked availability.\n"
         "Write the email now.\n"
         "If you propose times, they must be exact slot display strings returned by find_free_times and copied verbatim.")
        "Scheduling draft suppressed: model returned planning text instead of an email."
        (list jds~gptel-find-free-times-tool)
        jds/ai-email-reinforce-scheduling-reply-artifact))
     (list jds~gptel-find-free-times-tool)
     jds/ai-email-reinforce-scheduling-reply-database
     context)))
