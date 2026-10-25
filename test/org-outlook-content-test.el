;;; org-outlook-content-test.el --- Personal content and capture tests -*- lexical-binding: t; -*-

(require 'org-outlook-sync-test)
(require 'org-capture)

(defmacro org-outlook-test-with-native-files (&rest body)
  "Use the real event renderer with temporary Org files."
  (declare (indent 0))
  `(let ((renderer (symbol-function 'org-outlook-build-element)))
     (org-outlook-test-with-files
       (cl-letf (((symbol-function 'org-outlook-build-element) renderer))
         ,@body))))

(defun org-outlook-test-install-native-event (file id)
  "Insert an actual rendered event into FILE."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-max))
    (insert (org-outlook-build-element (org-outlook-test-event id "old")))
    (save-buffer)))

(ert-deftest org-outlook-content-preserves-headline-notes-clocks-and-children ()
  (org-outlook-test-with-native-files
    (org-outlook-test-install-native-event refiled "meeting")
    (with-current-buffer (get-file-buffer refiled)
      (goto-char (point-min)) (re-search-forward "^\\*\\* MEETING")
      (beginning-of-line)
      (delete-region (point) (line-end-position))
      (insert "** TODO [#A] My personal label :project:review:")
      (org-entry-put nil "CUSTOM" "keep me")
      (org-end-of-meta-data)
      (insert "Personal notes stay here.\n:LOGBOOK:\nCLOCK: [2026-10-20 Tue 11:00]--[2026-10-20 Tue 12:00] =>  1:00\n:END:\n")
      (goto-char (point-max))
      (insert "*** TODO Follow-up :mine:\nChild notes\n")
      (save-buffer))
    (let ((event (org-outlook-test-event "meeting")))
      (setf (alist-get 'subject event) "Renamed remotely"
            (alist-get 'categories event) ["remote"])
      (org-outlook--apply-sync-batch (org-outlook-test-batch (list event)) nil))
    (let ((text (org-outlook-test-file-text refiled)))
      (dolist (kept '("** TODO [#A] My personal label :project:review:"
                      ":CUSTOM:   keep me" "Personal notes stay here."
                      "CLOCK: [2026-10-20 Tue 11:00]--[2026-10-20 Tue 12:00] =>  1:00"
                      "*** TODO Follow-up :mine:" "Child notes"))
        (should (string-match-p (regexp-quote kept) text)))
      (should (string-match-p ":OUTLOOK-SUBJECT: Renamed remotely" text))
      (should (string-match-p "^\\*\\*\\* Outlook: Renamed remotely" text))
      (should (string-match-p ":OUTLOOK-CATEGORIES: \\[\"remote\"\\]" text)))))

(ert-deftest org-outlook-content-migrates-legacy-without-discarding-text ()
  (org-outlook-test-with-native-files
    (with-current-buffer (get-file-buffer org-outlook-file)
      (goto-char (point-max))
      (insert "** TODO Legacy :mine:\n:PROPERTIES:\n:ID: "
              (secure-hash 'sha256 "legacy")
              "\n:OUTLOOK-ID: legacy\n:OUTLOOK-ID-TYPE: immutable\n:CHANGEKEY: old\n:CUSTOM: retained\n:END:\n"
              "<2026-10-22 Thu 09:00-10:00>\nLegacy body and personal notes\n"
              "*** Personal child\nKeep this child\n")
      (save-buffer))
    ;; Matching changeKey must still migrate an entry with no version marker.
    (let ((batch (org-outlook-test-batch (list (org-outlook-test-event "legacy" "old")))))
      (org-outlook--apply-sync-batch batch nil)
      (let ((once (org-outlook-test-file-text org-outlook-file)))
        (should (string-match-p "COMMENT Preserved legacy text" once))
        (should (string-match-p "Legacy body and personal notes" once))
        (should (string-match-p "Keep this child" once))
        (should (string-match-p "^\\*\\* TODO Legacy :mine:" once))
        (org-outlook--apply-sync-batch batch nil)
        (should (equal once (org-outlook-test-file-text org-outlook-file)))))))

(ert-deftest org-outlook-content-retirement-retains-notes-and-can-resurrect ()
  (org-outlook-test-with-native-files
    (org-outlook-test-install-native-event org-outlook-file "meeting")
    (with-current-buffer (get-file-buffer org-outlook-file)
      (goto-char (point-min)) (re-search-forward "^\\*\\* MEETING")
      (org-back-to-heading)
      (org-end-of-meta-data)
      (insert "Notes survive cancellation.\n")
      (goto-char (point-max)) (insert "*** Action item\nKeep working on this.\n")
      (save-buffer))
    (org-outlook--apply-sync-batch (org-outlook-test-batch nil) t)
    (let ((retired (org-outlook-test-file-text org-outlook-file)))
      (should (string-match-p ":OUTLOOK-RETIRED-ID: meeting" retired))
      (should (string-match-p "Notes survive cancellation" retired))
      (should (string-match-p "Keep working on this" retired))
      (should-not (string-match-p ":OUTLOOK-MANAGED-ID:" retired))
      (should-not (string-match-p "<2026-10-22" retired)))
    (org-outlook--apply-sync-batch
     (org-outlook-test-batch (list (org-outlook-test-event "meeting"))) nil)
    (let ((text (org-outlook-test-file-text org-outlook-file)))
      (should (string-match-p ":OUTLOOK-ID: meeting" text))
      (should-not (string-match-p ":OUTLOOK-RETIRED-ID:" text))
      (should (string-match-p "Notes survive cancellation" text))
      (with-temp-buffer
        (insert text)
        (should (= 1 (how-many ":ID: " (point-min) (point-max))))))))

(ert-deftest org-outlook-content-respects-refiled-heading-level ()
  (org-outlook-test-with-native-files
    (with-current-buffer (get-file-buffer refiled)
      (goto-char (point-max))
      (insert "** Project\n*** Subproject\n"
              (org-outlook--nest-body
               (org-outlook-build-element (org-outlook-test-event "deep" "old")) 3))
      (save-buffer))
    (org-outlook--apply-sync-batch
     (org-outlook-test-batch (list (org-outlook-test-event "deep"))) nil)
    (let ((text (org-outlook-test-file-text refiled)))
      (should (string-match-p "^\\*\\*\\*\\* MEETING deep" text))
      (should (string-match-p "^\\*\\*\\*\\*\\* Outlook: deep" text)))))

(ert-deftest org-outlook-content-body-headings-stay-inside-generated-child ()
  (let ((event (org-outlook-test-event "meeting")))
    (setf (alist-get 'body event)
          '((contentType . "text") (content . "* Topic\n** Detail\n")))
    (let ((text (org-outlook-build-element event)))
      (should (string-match-p "^\\*\\*\\*\\* Topic" text))
      (should (string-match-p "^\\*\\*\\*\\*\\* Detail" text))
      (should-not (string-match-p "CLOCK:" text)))))

(ert-deftest org-outlook-content-decline-retains-personal-content ()
  (org-outlook-test-with-native-files
    (org-outlook-test-install-native-event org-outlook-file "meeting")
    (with-current-buffer (get-file-buffer org-outlook-file)
      (goto-char (point-min)) (re-search-forward "^\\*\\* MEETING")
      (org-back-to-heading)
      (let ((position (point)))
        (org-end-of-meta-data) (insert "Personal decline notes\n") (save-buffer)
        (cl-letf (((symbol-function 'org-id-find)
                   (lambda (_) (cons org-outlook-file position))))
          (org-outlook-delete-by-id (secure-hash 'sha256 "meeting")))))
    (should (string-match-p "Personal decline notes" (org-outlook-test-file-text org-outlook-file)))
    (should (string-match-p ":OUTLOOK-RETIRED-ID:" (org-outlook-test-file-text org-outlook-file)))))

(defmacro org-outlook-test-with-capture-entry (teams &rest body)
  "Run BODY at an editable capture entry with TEAMS set to yes or no."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert "** Planning meeting\n:PROPERTIES:\n:MEETING-TIME: <2026-10-22 Thu 09:00-10:00>\n"
             ":INVITEES: one@example.com two@example.com\n:LOCATION: Office\n:TEAMS-MEETING: "
             ,teams "\n:END:\nMeeting agenda\n")
     (goto-char (point-min))
     (cl-letf (((symbol-function 'org-msg-export-as-html)
                (lambda (text) (concat "<p>" text "</p>"))))
       ,@body)))

(defun org-outlook-test-calendar-response (&optional teams)
  "Return a successful event response, optionally with a Teams join URL."
  (let ((event (org-outlook-test-event "created-event")))
    (when teams
      (setf (alist-get 'onlineMeeting event)
            '((joinUrl . "https://teams.microsoft.com/l/meetup-join/server-link"))))
    (org-outlook-test-response 201 (json-encode event))))

(defun org-outlook-test-create-with-local-id (file local-id)
  "Create a mocked calendar event in FILE while preserving LOCAL-ID."
  (let ((text
         (org-outlook-test-with-capture-entry "no"
           (org-entry-put nil "ID" local-id)
           (org-outlook-test-with-responses (list (org-outlook-test-calendar-response))
             (org-outlook-calendar-create-or-update-event))
           (buffer-string))))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-max))
      (let ((begin (point)))
        (insert text)
        (save-buffer)
        begin))))

(ert-deftest org-outlook-capture-existing-org-id-syncs-without-duplicates ()
  (org-outlook-test-with-native-files
    (let* ((local-id "7b82d734-88de-491b-acfe-8fc6d675e3b4")
           (begin (org-outlook-test-create-with-local-id refiled local-id))
           (event (org-outlook-test-event "created-event" "updated")))
      (setf (alist-get 'body event)
            '((contentType . "text") (content . "Updated remote description")))
      (dolist (full '(nil t))
        (org-outlook--apply-sync-batch (org-outlook-test-batch (list event)) full)
        (with-current-buffer (get-file-buffer refiled)
          (goto-char begin)
          (should (equal local-id (org-entry-get nil "ID")))
          (should (equal "updated" (org-entry-get nil "CHANGEKEY")))
          (should (= 1 (length (org-outlook--managed-entries
                               (current-buffer) "2026-10-01" "2026-12-31")))))
        (let ((text (org-outlook-test-file-text refiled)))
          (should (string-match-p "Meeting agenda" text))
          (should (string-match-p "Updated remote description" text)))
        (should-not (string-match-p ":OUTLOOK-ID: created-event"
                                    (org-outlook-test-file-text org-outlook-file)))))))

(ert-deftest org-outlook-capture-existing-org-id-without-marker-recovers ()
  (org-outlook-test-with-native-files
    (let* ((local-id "7b82d734-88de-491b-acfe-8fc6d675e3b4")
           (begin (org-outlook-test-create-with-local-id refiled local-id)))
      ;; Simulate a capture saved before local IDs were recorded explicitly.
      (with-current-buffer (get-file-buffer refiled)
        (goto-char begin)
        (org-entry-delete nil "OUTLOOK-LOCAL-ID")
        (save-buffer))
      (org-outlook--apply-sync-batch (org-outlook-test-batch nil) t)
      (with-current-buffer (get-file-buffer refiled)
        (goto-char begin)
        (should (equal local-id (org-entry-get nil "ID")))
        (should (equal local-id (org-entry-get nil "OUTLOOK-LOCAL-ID")))
        (should (equal "created-event" (org-entry-get nil "OUTLOOK-RETIRED-ID")))
        (should (= 1 (length (org-outlook--managed-entries
                             (current-buffer) "2026-10-01" "2026-12-31")))))
      (should (string-match-p "Meeting agenda" (org-outlook-test-file-text refiled))))))

(ert-deftest org-outlook-capture-existing-org-id-retires-and-resurrects ()
  (org-outlook-test-with-native-files
    (let* ((local-id "7b82d734-88de-491b-acfe-8fc6d675e3b4")
           (begin (org-outlook-test-create-with-local-id refiled local-id)))
      (cl-letf (((symbol-function 'org-id-find)
                 (lambda (id)
                   (should (equal local-id id))
                   (cons refiled begin))))
        (org-outlook-delete-by-id local-id))
      (with-current-buffer (get-file-buffer refiled)
        (goto-char begin)
        (should-not (org-entry-get nil "OUTLOOK-ID"))
        (should (equal "created-event" (org-entry-get nil "OUTLOOK-RETIRED-ID")))
        (should (equal local-id (org-entry-get nil "ID"))))
      (org-outlook--apply-sync-batch
       (org-outlook-test-batch (list (org-outlook-test-event "created-event"))) nil)
      (with-current-buffer (get-file-buffer refiled)
        (goto-char begin)
        (should (equal local-id (org-entry-get nil "ID")))
        (should (equal "created-event" (org-entry-get nil "OUTLOOK-ID")))
        (should-not (org-entry-get nil "OUTLOOK-RETIRED-ID")))
      (should (string-match-p "Meeting agenda" (org-outlook-test-file-text refiled)))
      (should-not (string-match-p ":OUTLOOK-ID: created-event"
                                  (org-outlook-test-file-text org-outlook-file))))))

(ert-deftest org-outlook-capture-teams-uses-one-calendar-post-and-returned-url ()
  (org-outlook-test-with-capture-entry "yes"
    (org-outlook-test-with-responses (list (org-outlook-test-calendar-response t))
      (org-outlook-calendar-create-or-update-event)
      (should (= 1 (length calls)))
      (should (eq 'post (caar calls)))
      (should (equal org-outlook-events-create-url (nth 1 (car calls))))
      (let ((payload (json-read-from-string (plist-get (nth 2 (car calls)) :body))))
        (should (eq t (alist-get 'isOnlineMeeting payload)))
        (should (equal "teamsForBusiness" (alist-get 'onlineMeetingProvider payload)))
        (should (= 2 (length (alist-get 'attendees payload))))
        (should (equal "one@example.com"
                       (alist-get 'address (alist-get 'emailAddress
                                                     (aref (alist-get 'attendees payload) 0))))))
      (goto-char (point-min))
      (should (equal "created-event" (org-entry-get nil "OUTLOOK-ID")))
      (should (equal "https://teams.microsoft.com/l/meetup-join/server-link"
                     (org-entry-get nil "TEAMSURL")))
      (should (string-match-p "server-link\\]\\[Join Teams meeting" (buffer-string)))
      (should (string-match-p "Meeting agenda" (buffer-string))))))

(ert-deftest org-outlook-capture-without-teams-sends-false-and-no-provider ()
  (org-outlook-test-with-capture-entry "no"
    (org-outlook-test-with-responses (list (org-outlook-test-calendar-response))
      (org-outlook-calendar-create-or-update-event)
      (let ((payload (json-read-from-string (plist-get (nth 2 (car calls)) :body))))
        (should (eq :json-false (alist-get 'isOnlineMeeting payload)))
        (should-not (assq 'onlineMeetingProvider payload)))
      (goto-char (point-min))
      (should-not (org-entry-get nil "TEAMSURL")))))

(ert-deftest org-outlook-capture-failure-retains-transaction-for-manual-retry ()
  (org-outlook-test-with-capture-entry "yes"
    (org-outlook-test-with-responses
        (list (org-outlook-test-http-error 503) (org-outlook-test-calendar-response t))
      (should-error (org-outlook-calendar-create-or-update-event) :type 'org-outlook-graph-error)
      (let ((transaction (org-entry-get nil "OUTLOOK-TRANSACTION-ID")))
        (should transaction)
        (should-not (org-entry-get nil "OUTLOOK-ID"))
        (org-outlook-calendar-create-or-update-event)
        (dolist (call calls)
          (let ((payload (json-read-from-string (plist-get (nth 2 call) :body))))
            (should (equal transaction (alist-get 'transactionId payload)))))))))

(ert-deftest org-outlook-capture-update-does-not-upload-notes-or-reset-meeting-body ()
  (org-outlook-test-with-capture-entry "yes"
    (org-entry-put nil "OUTLOOK-ID" "existing-event")
    (org-entry-put nil "OUTLOOK-SYNC-VERSION" "1")
    (cl-letf (((symbol-function 'org-msg-export-as-html)
               (lambda (_) (error "Must not export private notes on update"))))
      (org-outlook-test-with-responses (list (org-outlook-test-calendar-response t))
        (org-outlook-calendar-create-or-update-event)
        (should (eq 'patch (caar calls)))
        (let ((payload (json-read-from-string (plist-get (nth 2 (car calls)) :body))))
          (should-not (assq 'body payload))
          (should-not (assq 'categories payload)))))))

(defmacro org-outlook-test-with-imported-subject (subject &rest body)
  "Run BODY on a real imported event with SUBJECT and a mocked update."
  (declare (indent 1))
  `(let ((event (org-outlook-test-event "meeting")))
     (setf (alist-get 'subject event) ,subject)
     (with-temp-buffer
       (org-mode)
       (insert (org-outlook-build-element event))
       (goto-char (point-min))
       (org-outlook-test-with-responses
           (list (org-outlook-test-response 200 (json-encode event)))
         ,@body))))

(ert-deftest org-outlook-update-imported-subject-ignores-generated-status ()
  (dolist (keywords '(((sequence "TODO" "|" "DONE"))
                      ((sequence "TODO" "MEETING" "REQUEST" "|" "DONE" "CANCELLED"))))
    (let ((org-todo-keywords keywords))
      (dolist (subject '("Project review" "MEETING planning" "REQUEST access" "CANCELLED plans"))
        (org-outlook-test-with-imported-subject subject
          (org-outlook-calendar-create-or-update-event)
          (let ((payload (json-read-from-string (plist-get (nth 2 (car calls)) :body))))
            (should (eq 'patch (caar calls)))
            (should (equal subject (alist-get 'subject payload)))))))))

(ert-deftest org-outlook-update-imported-subject-preserves-explicit-renames ()
  (dolist (keywords '(((sequence "TODO" "|" "DONE"))
                      ((sequence "TODO" "MEETING" "REQUEST" "|" "DONE" "CANCELLED"))))
    (let ((org-todo-keywords keywords))
      (dolist (rename '("New meeting title" "TODO [#A] New meeting title :project:"))
        (org-outlook-test-with-imported-subject "Project review"
          (delete-region (line-beginning-position) (line-end-position))
          (insert "** " rename)
          (beginning-of-line)
          (org-outlook-calendar-create-or-update-event)
          (let ((payload (json-read-from-string (plist-get (nth 2 (car calls)) :body))))
            (should (equal "New meeting title" (alist-get 'subject payload))))))))
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
    (dolist (rename '("MEETING planning" "REQUEST access" "CANCELLED plans"))
      (org-outlook-test-with-imported-subject "Project review"
        (delete-region (line-beginning-position) (line-end-position))
        (insert "** " rename)
        (beginning-of-line)
        (org-outlook-calendar-create-or-update-event)
        (let ((payload (json-read-from-string (plist-get (nth 2 (car calls)) :body))))
          (should (equal rename (alist-get 'subject payload))))))))

(ert-deftest org-outlook-update-unchanged-import-keeps-remote-rename ()
  (dolist (personal-todo '(nil t))
    (org-outlook-test-with-imported-subject "Original subject"
      (when personal-todo
        (let ((heading (org-get-heading t nil t t)))
          (delete-region (line-beginning-position) (line-end-position))
          (insert "** TODO [#A] " heading " :personal:")
          (beginning-of-line)))
      ;; Sync preserves the parent heading while updating the server properties.
      (org-entry-put nil "OUTLOOK-SUBJECT" "Renamed remotely")
      (org-entry-put nil "OUTLOOK-STATUS" "REQUEST")
      (org-outlook-calendar-create-or-update-event)
      (let ((payload (json-read-from-string (plist-get (nth 2 (car calls)) :body))))
        (should (equal "Renamed remotely" (alist-get 'subject payload)))))))

(ert-deftest org-outlook-update-older-import-recognizes-exact-status-prefix ()
  (dolist (status '("MEETING" "REQUEST" "CANCELLED"))
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
      (org-outlook-test-with-imported-subject "MEETING planning"
        (org-entry-delete nil "OUTLOOK-GENERATED-HEADING")
        (delete-region (line-beginning-position) (line-end-position))
        (insert "** " status " MEETING planning")
        (beginning-of-line)
        (org-outlook-calendar-create-or-update-event)
        (let ((payload (json-read-from-string (plist-get (nth 2 (car calls)) :body))))
          (should (equal "MEETING planning" (alist-get 'subject payload))))))))

(ert-deftest org-outlook-capture-registers-configured-agenda-file ()
  (dolist (agenda '(nil "/tmp/personal-agenda-list"))
    (let ((org-agenda-files agenda)
          (org-outlook-file "/tmp/configured-outlook.org"))
      (org-outlook-test-with-capture-entry "no"
        (org-outlook-test-with-responses (list (org-outlook-test-calendar-response))
          (org-outlook-calendar-create-or-update-event)
          (should (equal (if (stringp agenda) agenda (list org-outlook-file))
                         org-agenda-files)))))))

(ert-deftest org-outlook-capture-missing-join-url-keeps-created-id ()
  (org-outlook-test-with-capture-entry "yes"
    (org-outlook-test-with-responses
        (list (org-outlook-test-calendar-response) (org-outlook-test-calendar-response t))
      (should-error (org-outlook-calendar-create-or-update-event) :type 'user-error)
      (goto-char (point-min))
      (should (equal "created-event" (org-entry-get nil "OUTLOOK-ID")))
      (org-outlook-calendar-create-or-update-event)
      (should (eq 'patch (caar calls)))
      (should (eq 'post (car (cadr calls)))))))

(ert-deftest org-outlook-capture-finalization-ignores-other-templates ()
  (with-temp-buffer
    (org-mode)
    (insert "* Ordinary capture\nMEETING-TIME is just text here\n")
    (let ((org-capture-mode t))
      (cl-letf (((symbol-function 'org-capture-get) (lambda (&rest _) nil))
                ((symbol-function 'org-outlook-calendar-create-or-update-event)
                 (lambda () (error "Unexpected calendar write"))))
        (org-outlook-finalize-capture)))))

(ert-deftest org-outlook-capture-template-saves-in-calendar-file ()
  (let ((org-capture-templates nil)
        (org-capture-prepare-finalize-hook nil)
        (org-outlook-capture-key "o"))
    (org-outlook-install-capture-template)
    (org-outlook-install-capture-template)
    (let ((template (assoc "o" org-capture-templates)))
      (should (= 1 (length org-capture-templates)))
      (should (equal '(file org-outlook-file) (nth 3 template)))
      (should (plist-get (nthcdr 5 template) :org-outlook))
      (should (= 1 (cl-count #'org-outlook-finalize-capture
                             org-capture-prepare-finalize-hook))))))

(ert-deftest org-outlook-capture-integration-is-opt-in ()
  (should-not (memq #'org-outlook-finalize-capture
                    org-capture-prepare-finalize-hook))
  (should-not (seq-find #'org-outlook--capture-template-p
                        org-capture-templates)))

(ert-deftest org-outlook-capture-install-rejects-key-collision ()
  (let ((org-capture-templates '(("o" "Personal" entry (file "notes.org") "* %?")))
        (org-capture-prepare-finalize-hook nil)
        (org-outlook-capture-key "o"))
    (should-error (org-outlook-install-capture-template) :type 'user-error)
    (should (equal "Personal" (cadr (assoc "o" org-capture-templates))))
    (should-not org-capture-prepare-finalize-hook)))

(ert-deftest org-outlook-capture-install-moves-configured-key ()
  (let ((org-capture-templates nil)
        (org-capture-prepare-finalize-hook nil)
        (org-outlook-capture-key "o"))
    (org-outlook-install-capture-template)
    (setq org-outlook-capture-key "O")
    (org-outlook-install-capture-template)
    (should-not (assoc "o" org-capture-templates))
    (should (org-outlook--capture-template-p
             (assoc "O" org-capture-templates)))))

(ert-deftest org-outlook-content-join-resolves-parent-url ()
  (with-temp-buffer
    (org-mode)
    (insert "* Meeting\n:PROPERTIES:\n:TEAMSURL: https://teams.microsoft.com/returned\n:END:\n** Outlook data\n")
    (goto-char (point-max)) (org-back-to-heading)
    (let (opened)
      (cl-letf (((symbol-function 'org-outlook-teams-installed-p) (lambda () nil))
                ((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url))))
        (org-outlook-join-teams-call)
        (should (equal "https://teams.microsoft.com/returned" opened))))))

(ert-deftest org-outlook-content-cancelled-meeting-keeps-personal-todo ()
  (org-outlook-test-with-native-files
    (org-outlook-test-install-native-event org-outlook-file "cancelled")
    (let ((event (org-outlook-test-event "cancelled")))
      (setf (alist-get 'isCancelled event) t)
      (org-outlook--apply-sync-batch (org-outlook-test-batch (list event)) nil)
      (let ((text (org-outlook-test-file-text org-outlook-file)))
        (should (string-match-p "^\\*\\* MEETING cancelled" text))
        (should (string-match-p "Outlook: \\[CANCELLED\\]" text))
        (should (string-match-p ":OUTLOOK-STATUS: CANCELLED" text))
        (should-not (string-match-p "<2026-10-22" text))))))

(ert-deftest org-outlook-capture-finalization-saves-the-returned-event ()
  (org-outlook-test-with-native-files
    (let ((org-capture-templates
           `(("O" "Outlook integration" entry (file ,org-outlook-file)
              "* Planning\n:PROPERTIES:\n:MEETING-TIME: <2026-10-22 Thu 09:00-10:00>\n:TEAMS-MEETING: yes\n:END:\nPersonal capture notes\n"
              :org-outlook t)))
          (org-capture-after-finalize-hook nil)
          (org-capture-before-finalize-hook nil)
          (org-capture-prepare-finalize-hook
           '(org-outlook-finalize-capture)))
      (cl-letf (((symbol-function 'org-msg-export-as-html) #'identity))
        (org-outlook-test-with-responses (list (org-outlook-test-calendar-response t))
          (unwind-protect
              (progn
                (org-capture nil "O")
                (org-capture-finalize)
                (should (= 1 (length calls)))
                (let ((text (org-outlook-test-file-text org-outlook-file)))
                  (should (string-match-p ":OUTLOOK-ID: created-event" text))
                  (should (string-match-p "Personal capture notes" text))
                  (should (string-match-p "server-link" text))))
            (when (bound-and-true-p org-capture-mode) (org-capture-kill))))))))

(ert-deftest org-outlook-capture-abort-skips-availability-and-calendar-write ()
  (org-outlook-test-with-native-files
    (let ((org-capture-templates
           `(("O" "Outlook integration" entry (file ,org-outlook-file)
              "* Planning\n:PROPERTIES:\n:MEETING-TIME: <2026-10-22 Thu 09:00-10:00>\n:TEAMS-MEETING: no\n:END:\nPersonal capture notes\n"
              :org-outlook t)))
          (org-capture-after-finalize-hook nil)
          (org-capture-before-finalize-hook nil)
          (org-capture-prepare-finalize-hook
           '(org-outlook-finalize-capture))
          (original (org-outlook-test-file-text org-outlook-file))
          availability-checked)
      (cl-letf (((symbol-function 'org-msg-export-as-html) #'identity)
                ((symbol-function 'org-outlook-check-capture-availability)
                 (lambda () (setq availability-checked t))))
        (org-outlook-test-with-responses (list (org-outlook-test-calendar-response))
          (unwind-protect
              (progn
                (org-capture nil "O")
                (org-capture-kill)
                (should-not availability-checked)
                (should-not calls)
                (should (equal original (org-outlook-test-file-text org-outlook-file))))
            (when (bound-and-true-p org-capture-mode)
              (let ((org-capture-prepare-finalize-hook nil))
                (org-capture-kill)))))))))

(ert-deftest org-outlook-content-restores-missing-generated-section ()
  (org-outlook-test-with-native-files
    (org-outlook-test-install-native-event org-outlook-file "meeting")
    (with-current-buffer (get-file-buffer org-outlook-file)
      (goto-char (point-min)) (re-search-forward "^\\*\\* MEETING")
      (org-back-to-heading)
      (let ((child (org-outlook--managed-child "meeting")))
        (delete-region (car child) (cdr child)))
      (goto-char (point-max)) (insert "Personal notes remain.\n")
      (save-buffer))
    (org-outlook--apply-sync-batch
     (org-outlook-test-batch (list (org-outlook-test-event "meeting" "old"))) nil)
    (let ((text (org-outlook-test-file-text org-outlook-file)))
      (should (string-match-p ":OUTLOOK-MANAGED-ID: meeting" text))
      (should (string-match-p "Personal notes remain" text)))))

(ert-deftest org-outlook-capture-rejects-zero-duration-before-writing ()
  (org-outlook-test-with-capture-entry "yes"
    (org-entry-put nil "MEETING-TIME" "<2026-10-22 Thu 09:00>")
    (org-outlook-test-with-responses nil
      (should-error (org-outlook-calendar-create-or-update-event) :type 'user-error)
      (should-not calls))))

(ert-deftest org-outlook-content-retired-entry-does-not-borrow-parent-link ()
  (with-temp-buffer
    (org-mode)
    (insert "* Parent meeting\n:PROPERTIES:\n:TEAMSURL: https://teams.microsoft.com/parent\n:END:\n"
            "** Retired meeting\n:PROPERTIES:\n:OUTLOOK-RETIRED-ID: old\n:END:\n")
    (goto-char (point-max)) (org-back-to-heading)
    (should-not (org-outlook-get-appointment-property "TEAMSURL"))))

(provide 'org-outlook-content-test)
;;; org-outlook-content-test.el ends here
