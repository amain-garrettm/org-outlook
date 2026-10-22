;;; org-outlook-time-workflow-test.el --- Time and workflow tests -*- lexical-binding: t; -*-
(require 'org-outlook-content-test)
(require 'org-agenda)

(ert-deftest org-outlook-time-uses-the-meeting-date-for-dst ()
  (should (equal "2026-01-15T15:00:00Z"
                 (org-outlook--utc-string
                  (org-outlook--parse-time "2026-01-15T09:00:00" "America/Chicago" t))))
  (should (equal "2026-07-15T14:00:00Z"
                 (org-outlook--utc-string
                  (org-outlook--parse-time "2026-07-15T09:00:00" "America/Chicago" t)))))

(ert-deftest org-outlook-time-dst-gaps-and-ambiguity-are-not-guessed ()
  (dolist (time '("2026-03-08T02:30:00" "2026-11-01T01:30:00"))
    (should-error (org-outlook--parse-time time "America/Chicago" t) :type 'user-error))
  (should-not (equal (org-outlook--parse-time "2026-11-01T01:30:00-05:00")
                     (org-outlook--parse-time "2026-11-01T01:30:00-06:00"))))

(ert-deftest org-outlook-time-independent-of-system-zone ()
  (let ((original (getenv "TZ")) (org-outlook-local-timezone "America/Chicago") values)
    (unwind-protect
        (dolist (zone '("UTC" "Asia/Tokyo" "Europe/Berlin"))
          (set-time-zone-rule zone)
          (push (org-outlook-timestamp-to-list "2026-07-15T14:00:00Z") values))
      (set-time-zone-rule original))
    (should (equal '((2026 7 15 9 0) (2026 7 15 9 0) (2026 7 15 9 0)) values))))

(ert-deftest org-outlook-time-local-window-has-23-and-25-hour-days ()
  (let ((org-outlook-local-timezone "America/Chicago"))
    (dolist (case '(("2026-03-08" . 23) ("2026-11-01" . 25)))
      (should (= (* 3600 (cdr case))
                 (float-time (time-subtract (org-outlook--window-boundary (car case) t)
                                           (org-outlook--window-boundary (car case)))))))
    (should (equal "2026-03-08T06:00:00Z"
                   (org-outlook--utc-string (org-outlook--window-boundary "2026-03-08"))))))

(ert-deftest org-outlook-time-all-day-end-is-exclusive ()
  (let ((event (org-outlook-test-event "all-day")) (org-outlook-local-timezone "Europe/Berlin"))
    (setf (alist-get 'isAllDay event) t
          (alist-get 'start event) '((dateTime . "2026-10-22T00:00:00") (timeZone . "Europe/Berlin"))
          (alist-get 'end event) '((dateTime . "2026-10-23T00:00:00") (timeZone . "Europe/Berlin")))
    (should (equal "<2026-10-22>" (org-outlook--event-timestamp event)))
    (setf (alist-get 'end event) '((dateTime . "2026-10-25T00:00:00") (timeZone . "Europe/Berlin")))
    (should (equal "<2026-10-22>--<2026-10-24>" (org-outlook--event-timestamp event)))))

(ert-deftest org-outlook-time-capture-sends-date-specific-utc ()
  (let ((org-outlook-local-timezone "America/Chicago"))
    (org-outlook-test-with-capture-entry "no"
      (org-entry-put nil "MEETING-TIME" "<2026-07-15 Wed 09:00-10:00>")
      (org-outlook-test-with-responses (list (org-outlook-test-calendar-response))
        (org-outlook-calendar-create-or-update-event)
        (let* ((payload (json-read-from-string (plist-get (nth 2 (car calls)) :body)))
               (start (alist-get 'start payload)))
          (should (equal "UTC" (alist-get 'timeZone start)))
          (should (equal "2026-07-15T14:00:00Z" (alist-get 'dateTime start))))))))

(ert-deftest org-outlook-time-availability-compares-instants ()
  (let ((org-outlook-local-timezone "America/Chicago"))
    (cl-letf (((symbol-function 'org-outlook-check-availability)
               (lambda (&rest _)
                 '((value . [((scheduleId . "one@example.com")
                              (scheduleItems . [((status . "workingElsewhere")
                                                 (start . ((dateTime . "2026-07-15T14:00:00") (timeZone . "UTC")))
                                                 (end . ((dateTime . "2026-07-15T15:00:00") (timeZone . "UTC"))))]))])))))
      (should-not (org-outlook-persons-available-p "one@example.com"
                                                   "2026-07-15T09:00:00" "2026-07-15T10:00:00")))))

(ert-deftest org-outlook-time-missing-or-failed-schedules-are-unknown ()
  (dolist (data '(nil ((value . [])) ((value . [((scheduleId . "one@example.com")
                                                (error . ((code . "unknown"))))]))))
    (cl-letf (((symbol-function 'org-outlook-check-availability) (lambda (&rest _) data)))
      (should-error (org-outlook-persons-available-p "one@example.com"
                                                    "2026-07-15T09:00:00" "2026-07-15T10:00:00")
                    :type 'org-outlook-graph-error))))

(ert-deftest org-outlook-identity-migration-preserves-links-and-notes ()
  (org-outlook-test-with-native-files
    (org-outlook-test-install-native-event refiled "mutable")
    (with-current-buffer (get-file-buffer refiled)
      (goto-char (point-min)) (re-search-forward "^\\*\\* MEETING") (org-back-to-heading)
      (org-entry-delete nil "OUTLOOK-ID-TYPE")
      (org-end-of-meta-data) (insert "Existing Org links and notes must survive.\n")
      (save-buffer))
    (org-outlook-test-with-responses
        (list (org-outlook-test-response 200 "{\"value\":[{\"sourceId\":\"mutable\",\"targetId\":\"immutable\"}]}"))
      (org-outlook--apply-sync-batch
       (org-outlook-test-batch (list (org-outlook-test-event "immutable"))) t))
    (let ((text (org-outlook-test-file-text refiled)))
      (should (string-match-p (secure-hash 'sha256 "mutable") text))
      (should (string-match-p ":OUTLOOK-ID: immutable" text))
      (should (string-match-p ":OUTLOOK-LEGACY-ID: mutable" text))
      (should (string-match-p "Existing Org links and notes must survive" text))
      (should-not (string-match-p ":OUTLOOK-RETIRED-ID:" text)))
    (org-outlook--apply-sync-batch
     (org-outlook-test-batch (list (org-outlook-test-event "immutable" "later"))) nil)
    (should-not (string-match-p ":OUTLOOK-ID: immutable"
                                (org-outlook-test-file-text org-outlook-file)))))

(ert-deftest org-outlook-identity-failed-migration-does-not-prune ()
  (org-outlook-test-with-native-files
    (org-outlook-test-install-native-event org-outlook-file "mutable")
    (with-current-buffer (get-file-buffer org-outlook-file)
      (goto-char (point-min)) (re-search-forward "^\\*\\* MEETING") (org-back-to-heading)
      (org-entry-delete nil "OUTLOOK-ID-TYPE") (save-buffer))
    (let ((before (org-outlook-test-file-text org-outlook-file)))
      (org-outlook-test-with-responses (list (org-outlook-test-http-error 403))
        (should-error (org-outlook--apply-sync-batch (org-outlook-test-batch nil) t)
                      :type 'org-outlook-graph-error))
      (should (equal before (org-outlook-test-file-text org-outlook-file))))))

(ert-deftest org-outlook-identity-recurring-instances-are-distinct ()
  (org-outlook-test-with-native-files
    (let ((first (org-outlook-test-event "instance-one"))
          (second (org-outlook-test-event "instance-two")))
      (setf (alist-get 'seriesMasterId first) "master"
            (alist-get 'seriesMasterId second) "master")
      (org-outlook--apply-sync-batch (org-outlook-test-batch (list first second)) t)
      (with-temp-buffer
        (insert-file-contents org-outlook-file)
        (should (= 2 (how-many "^:ID:" (point-min) (point-max)))))
      (setf (alist-get 'changeKey first) "exception")
      (org-outlook--apply-sync-batch (org-outlook-test-batch (list first)) nil)
      (should (string-match-p ":OUTLOOK-ID: instance-two"
                              (org-outlook-test-file-text org-outlook-file))))))

(ert-deftest org-outlook-agenda-accept-resolves-child-without-moving-source ()
  (with-temp-buffer
    (org-mode)
    (insert "* Meeting\n:PROPERTIES:\n:OUTLOOK-ID: remote\n:ID: local\n:END:\n** Generated\n")
    (goto-char (point-max)) (org-back-to-heading)
    (let ((position (point)) (marker (point-marker)))
      (narrow-to-region (point) (point-max))
      (with-temp-buffer
        (org-agenda-mode)
        (let ((inhibit-read-only t))
          (insert (propertize "Meeting\n" 'org-hd-marker marker)))
        (goto-char (point-min))
        (org-outlook-test-with-responses (list (org-outlook-test-response 202))
          (org-outlook-respond-to-event "accept" "" "N")
          (should (string-suffix-p "/remote/accept" (nth 1 (car calls))))))
      (should (= position (point)))
      (should (buffer-narrowed-p)))))

(ert-deftest org-outlook-agenda-stale-marker-fails-before-network ()
  (with-temp-buffer
    (org-agenda-mode)
    (let ((inhibit-read-only t))
      (insert (propertize "Stale\n" 'org-hd-marker (make-marker))))
    (goto-char (point-min))
    (org-outlook-test-with-responses nil
      (should-error (org-outlook-get-appointment-property "OUTLOOK-ID") :type 'user-error)
      (should-not calls))))

(ert-deftest org-outlook-identity-calendar-move-and-return-keeps-notes ()
  (org-outlook-test-with-native-files
    (org-outlook-test-install-native-event refiled "stable")
    (with-current-buffer (get-file-buffer refiled)
      (goto-char (point-min)) (re-search-forward "^\\*\\* MEETING") (org-back-to-heading)
      (org-end-of-meta-data) (insert "Notes from before the calendar move.\n") (save-buffer))
    ;; Moving out of the default calendar removes it from the tracked view.
    (org-outlook--apply-sync-batch
     (org-outlook-test-batch '(((id . "stable") (@removed . ((reason . "deleted")))))) nil)
    ;; Moving back into that calendar returns the same immutable event ID.
    (org-outlook--apply-sync-batch
     (org-outlook-test-batch (list (org-outlook-test-event "stable" "moved-back"))) nil)
    (let ((text (org-outlook-test-file-text refiled)))
      (should (string-match-p "Notes from before the calendar move" text))
      (should (string-match-p ":OUTLOOK-ID: stable" text))
      (should-not (string-match-p ":OUTLOOK-RETIRED-ID:" text)))
    (should-not (string-match-p ":OUTLOOK-ID: stable"
                                (org-outlook-test-file-text org-outlook-file)))))

(ert-deftest org-outlook-agenda-join-uses-source-url ()
  (with-temp-buffer
    (org-mode)
    (insert "* Meeting\n:PROPERTIES:\n:OUTLOOK-ID: remote\n:TEAMSURL: https://teams.microsoft.com/join\n:END:\n")
    (goto-char (point-min))
    (let ((marker (point-marker)) opened)
      (with-temp-buffer
        (org-agenda-mode)
        (let ((inhibit-read-only t)) (insert (propertize "Meeting\n" 'org-hd-marker marker)))
        (goto-char (point-min))
        (cl-letf (((symbol-function 'org-outlook-teams-installed-p) (lambda () nil))
                  ((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url))))
          (org-outlook-join-teams-call)
          (should (equal "https://teams.microsoft.com/join" opened)))))))

(ert-deftest org-outlook-agenda-file-backed-list-is-not-overwritten ()
  (let ((org-agenda-files "/tmp/my-existing-agenda-list"))
    (org-outlook--register-agenda-file)
    (should (equal "/tmp/my-existing-agenda-list" org-agenda-files))))

(ert-deftest org-outlook-response-forward-uses-a-recipient-array ()
  (with-temp-buffer
    (org-mode)
    (insert "* Event\n:PROPERTIES:\n:OUTLOOK-ID: remote\n:ID: local\n:END:\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "one@example.com two@example.com")))
      (org-outlook-test-with-responses (list (org-outlook-test-response 202))
        (org-outlook-respond-to-event "forward" "" "N")
        (let ((payload (json-read-from-string (plist-get (nth 2 (car calls)) :body))))
          (should (= 2 (length (alist-get 'toRecipients payload))))
          (should-not (assq 'sendResponse payload)))))))

(ert-deftest org-outlook-graph-immutable-preference-on-reads-and-writes ()
  (org-outlook-test-with-responses
      (list (org-outlook-test-response 200 "{}") (org-outlook-test-response 202))
    (org-outlook--graph-data 'get org-outlook-events-url)
    (org-outlook--graph-data 'post org-outlook-events-create-url)
    (dolist (call calls)
      (should (equal "IdType=\\\"ImmutableId\\\""
                     (cdr (assoc "Prefer" (plist-get (nth 2 call) :headers))))))))

(ert-deftest org-outlook-time-end-at-window-start-is-outside ()
  (let ((org-outlook-local-timezone "America/Chicago"))
    (with-temp-buffer
      (org-mode)
      (insert "* Past event\n:PROPERTIES:\n:OUTLOOK-START: 2026-09-30T23:00:00-05:00\n:OUTLOOK-END: 2026-10-01T00:00:00-05:00\n:END:\n")
      (goto-char (point-min))
      (should-not (org-outlook--entry-in-window-p "2026-10-01" "2026-12-31")))))

(ert-deftest org-outlook-identity-incomplete-translation-cannot-retire-notes ()
  (org-outlook-test-with-native-files
    (org-outlook-test-install-native-event org-outlook-file "mutable")
    (with-current-buffer (get-file-buffer org-outlook-file)
      (goto-char (point-min)) (re-search-forward "^\\*\\* MEETING") (org-back-to-heading)
      (org-entry-delete nil "OUTLOOK-ID-TYPE") (save-buffer))
    (let ((before (org-outlook-test-file-text org-outlook-file)))
      (org-outlook-test-with-responses (list (org-outlook-test-response 200 "{\"value\":[]}"))
        (should-error (org-outlook--apply-sync-batch (org-outlook-test-batch nil) t)
                      :type 'org-outlook-graph-error))
      (should (equal before (org-outlook-test-file-text org-outlook-file))))))

(ert-deftest org-outlook-agenda-cancel-retires-only-after-success ()
  (with-temp-buffer
    (org-mode)
    (insert "* Meeting\n:PROPERTIES:\n:OUTLOOK-ID: remote\n:ID: local\n:END:\n")
    (goto-char (point-min))
    (let ((marker (point-marker)) retired)
      (with-temp-buffer
        (org-agenda-mode)
        (let ((inhibit-read-only t)) (insert (propertize "Meeting\n" 'org-hd-marker marker)))
        (goto-char (point-min))
        (cl-letf (((symbol-function 'org-outlook-delete-by-id) (lambda (id) (setq retired id))))
          (org-outlook-test-with-responses (list (org-outlook-test-response 202))
            (org-outlook-respond-to-event "cancel" "" "N")
            (should (equal "local" retired))
            (should-not (string-match-p "sendResponse" (or (plist-get (nth 2 (car calls)) :body) "")))))))))

(provide 'org-outlook-time-workflow-test)
;;; org-outlook-time-workflow-test.el ends here
