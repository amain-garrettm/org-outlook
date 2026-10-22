;;; org-outlook-sync-test.el --- Sync transaction tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-outlook)
(require 'org-outlook-graph-test)

(defun org-outlook-test-entry (id &optional date change)
  "Render a managed test headline for ID, DATE and CHANGE."
  (format "** MEETING %s\n:PROPERTIES:\n:ID: %s\n:OUTLOOK-ID: %s\n:CHANGEKEY: %s\n:OUTLOOK-SYNC-VERSION: 1\n:END:\n*** Outlook: %s\n:PROPERTIES:\n:OUTLOOK-MANAGED-ID: %s\n:END:\n<%s Thu 09:00-10:00>\n"
          id (secure-hash 'sha256 id) id (or change "old")
          id id
          (or date "2026-10-22")))

(defun org-outlook-test-event (id &optional change)
  "Create a minimal test event with ID and CHANGE."
  (copy-tree `((id . ,id) (changeKey . ,(or change "new")) (subject . ,id)
    (isCancelled . :json-false)
    (start . ((dateTime . "2026-10-22T09:00:00") (timeZone . "Europe/Berlin")))
    (end . ((dateTime . "2026-10-22T10:00:00") (timeZone . "Europe/Berlin")))
    (body . ((contentType . "text") (content . "Remote description"))))))

(defun org-outlook-test-batch (events)
  "Create a fetched test round with EVENTS."
  (make-org-outlook-sync-batch
   :events events :delta "https://graph.microsoft.com/new-delta"
   :start "2026-10-01" :end "2026-12-31"))

(defmacro org-outlook-test-with-files (&rest body)
  "Run BODY against temporary destination and refiled Org files.
Stub event rendering and ID persistence, but use real Org buffers and saves."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "org-outlook-sync-" t))
          (org-outlook-file (expand-file-name "outlook.org" directory))
          (refiled (expand-file-name "refiled.org" directory))
          (org-id-files (list refiled))
          (org-id-locations (make-hash-table :test #'equal))
          (org-outlook--sync-running nil)
          committed)
     (unwind-protect
         (cl-letf (((symbol-function 'org-outlook-build-element)
                    (lambda (event)
                      (org-outlook-test-entry (alist-get 'id event) nil
                                             (alist-get 'changeKey event))))
                   ((symbol-function 'org-id-update-id-locations) (lambda (&rest _) nil))
                   ((symbol-function 'org-id-locations-save) #'ignore)
                   ((symbol-function 'org-outlook--sync-state) (lambda () nil))
                   ((symbol-function 'org-outlook-set-token-field)
                    (lambda (&rest args) (setq committed args))))
           (dolist (file (list org-outlook-file refiled))
             (with-current-buffer (find-file-noselect file)
               (org-mode)
               (insert "* Calendar\n")
               (save-buffer)))
           ,@body)
       (dolist (file (list org-outlook-file refiled))
         (when (get-file-buffer file)
           (with-current-buffer (get-file-buffer file)
             (set-buffer-modified-p nil)
             (kill-buffer))))
       (delete-directory directory t))))

(defun org-outlook-test-append-entry (file id &optional date)
  "Append a managed ID to FILE at DATE and save it."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-max))
    (insert (org-outlook-test-entry id date))
    (save-buffer)))

(defun org-outlook-test-file-text (file)
  "Read FILE from disk so tests verify saved state."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest org-outlook-sync-full-reconciles-managed-ids-in-range ()
  (org-outlook-test-with-files
    (org-outlook-test-append-entry org-outlook-file "stale")
    (org-outlook-test-append-entry org-outlook-file "history" "2025-01-01")
    (org-outlook-test-append-entry refiled "refiled-stale")
    (org-outlook-test-append-entry refiled "kept")
    (with-current-buffer (find-file-noselect org-outlook-file)
      (goto-char (point-max))
      (insert "** Personal note\n<2026-10-22 Thu>\n"
              "** Unverified\n:PROPERTIES:\n:OUTLOOK-ID: manual\n:ID: manual\n:END:\n<2026-10-22 Thu>\n")
      (save-buffer))
    (org-outlook--apply-sync-batch
     (org-outlook-test-batch (list (org-outlook-test-event "kept")
                                  (org-outlook-test-event "added"))) t)
    (let ((main (org-outlook-test-file-text org-outlook-file))
          (other (org-outlook-test-file-text refiled)))
      (should-not (string-match-p ":OUTLOOK-ID: stale\n" main))
      (should-not (string-match-p ":OUTLOOK-ID: refiled-stale" other))
      (should (string-match-p ":OUTLOOK-RETIRED-ID: refiled-stale" other))
      (should (string-match-p ":OUTLOOK-ID: history" main))
      (should (string-match-p "Personal note" main))
      (should (string-match-p ":OUTLOOK-ID: manual" main))
      (should (string-match-p ":OUTLOOK-ID: added" main))
      (should (string-match-p ":OUTLOOK-ID: kept" other))
      (should (string-match-p ":CHANGEKEY: new" other)))
    (should-not committed)))

(ert-deftest org-outlook-sync-empty-snapshot-prunes-only-known-range ()
  (org-outlook-test-with-files
    (org-outlook-test-append-entry org-outlook-file "stale")
    (with-current-buffer (find-file-noselect org-outlook-file)
      (goto-char (point-max))
      (insert "** Undated\n:PROPERTIES:\n:OUTLOOK-ID: undated\n:ID: "
              (secure-hash 'sha256 "undated") "\n:END:\n")
      (save-buffer))
    (org-outlook--apply-sync-batch (org-outlook-test-batch nil) t)
    (should-not (string-match-p ":OUTLOOK-ID: stale" (org-outlook-test-file-text org-outlook-file)))
    (should (string-match-p ":OUTLOOK-ID: undated" (org-outlook-test-file-text org-outlook-file)))))

(ert-deftest org-outlook-sync-tombstones-are-idempotent-and-window-scoped ()
  (org-outlook-test-with-files
    (org-outlook-test-append-entry refiled "deleted")
    (org-outlook-test-append-entry org-outlook-file "outside" "2025-01-01")
    (let ((batch (org-outlook-test-batch
                  '(((id . "deleted") (@removed . ((reason . "deleted"))))
                    ((id . "unknown") (@removed . ((reason . "deleted"))))
                    ((id . "outside") (@removed . ((reason . "deleted"))))))))
      (org-outlook--apply-sync-batch batch nil)
      (org-outlook--apply-sync-batch batch nil)
      (should-not (string-match-p ":OUTLOOK-ID: deleted" (org-outlook-test-file-text refiled)))
      (should (string-match-p ":OUTLOOK-ID: outside" (org-outlook-test-file-text org-outlook-file))))))

(ert-deftest org-outlook-sync-render-failure-leaves-files-and-cursor-untouched ()
  (org-outlook-test-with-files
    (org-outlook-test-append-entry org-outlook-file "old")
    (let ((before (org-outlook-test-file-text org-outlook-file)))
      (cl-letf (((symbol-function 'org-outlook-build-element) (lambda (_) (error "Render failed"))))
        (should-error (org-outlook--apply-sync-batch
                       (org-outlook-test-batch (list (org-outlook-test-event "new"))) t)))
      (should (equal before (org-outlook-test-file-text org-outlook-file)))
      (should-not committed))))

(ert-deftest org-outlook-sync-refuses-unsaved-local-edits ()
  (org-outlook-test-with-files
    (org-outlook-test-append-entry org-outlook-file "old")
    (with-current-buffer (find-file-noselect org-outlook-file)
      (goto-char (point-max)) (insert "Unsaved notes\n"))
    (should-error (org-outlook--apply-sync-batch (org-outlook-test-batch nil) t))
    (with-current-buffer (get-file-buffer org-outlook-file)
      (should (buffer-modified-p))
      (should (string-match-p "Unsaved notes" (buffer-string))))
    (should-not committed)))

(ert-deftest org-outlook-sync-save-failure-rolls-back-prior-saves ()
  (org-outlook-test-with-files
    (org-outlook-test-append-entry org-outlook-file "main")
    (org-outlook-test-append-entry refiled "other")
    (let ((main-before (org-outlook-test-file-text org-outlook-file))
          (other-before (org-outlook-test-file-text refiled))
          (save (symbol-function 'save-buffer))
          (count 0))
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest args)
                   (if (= 2 (cl-incf count)) (error "Disk save failed")
                     (apply save args)))))
        (should-error (org-outlook--apply-sync-batch (org-outlook-test-batch nil) t)))
      (should (equal main-before (org-outlook-test-file-text org-outlook-file)))
      (should (equal other-before (org-outlook-test-file-text refiled)))
      (dolist (file (list org-outlook-file refiled))
        (with-current-buffer (get-file-buffer file)
          (should (equal (buffer-string) (org-outlook-test-file-text file))))))
    (should-not committed)))

(ert-deftest org-outlook-sync-commits-only-after-application-and-index ()
  (org-outlook-test-with-files
    (let ((batch (org-outlook-test-batch (list (org-outlook-test-event "new"))))
          indexed)
      (cl-letf (((symbol-function 'org-outlook-retrieve-delta-pages) (lambda (&rest _) batch))
                ((symbol-function 'org-id-locations-save) (lambda () (setq indexed t)))
                ((symbol-function 'org-outlook-set-token-field)
                 (lambda (&rest args)
                   (should indexed)
                   (should (string-match-p ":OUTLOOK-ID: new"
                                           (org-outlook-test-file-text org-outlook-file)))
                   (setq committed args))))
        (org-outlook-full-sync)
        (should (equal "https://graph.microsoft.com/new-delta"
                       (plist-get (nth 1 committed) :delta)))
        (should (plist-get (nth 1 committed) :full-sync))
        (should (equal "2026-10-01" (plist-get (nth 1 committed) :start)))))))

(ert-deftest org-outlook-sync-checkpoint-failure-can-replay ()
  (org-outlook-test-with-files
    (let ((batch (org-outlook-test-batch (list (org-outlook-test-event "new")))))
      (cl-letf (((symbol-function 'org-outlook-retrieve-delta-pages) (lambda (&rest _) batch))
                ((symbol-function 'org-outlook-set-token-field)
                 (lambda (&rest _) (error "Checkpoint save failed"))))
        (should-error (org-outlook-delta-sync-impl)))
      (let ((before (org-outlook-test-file-text org-outlook-file)))
        (org-outlook--apply-sync-batch batch nil)
        (should (equal before (org-outlook-test-file-text org-outlook-file)))))))

(ert-deftest org-outlook-sync-fetch-failure-makes-no-local-changes ()
  (org-outlook-test-with-files
    (org-outlook-test-append-entry org-outlook-file "old")
    (let ((before (org-outlook-test-file-text org-outlook-file)))
      (org-outlook-test-with-responses
          (list (org-outlook-test-response
                 200 "{\"value\":[],\"@odata.nextLink\":\"https://graph.microsoft.com/next\"}")
                (org-outlook-test-http-error 503))
        (should-error (org-outlook-full-sync) :type 'org-outlook-graph-error))
      (should (equal before (org-outlook-test-file-text org-outlook-file)))
      (should-not committed))))

(ert-deftest org-outlook-sync-final-event-version-wins ()
  (org-outlook-test-with-files
    (org-outlook--apply-sync-batch
     (org-outlook-test-batch
      (list (org-outlook-test-event "repeated" "first")
            (org-outlook-test-event "repeated" "last"))) nil)
    (let ((text (org-outlook-test-file-text org-outlook-file)))
      (should (string-match-p ":CHANGEKEY: last" text))
      (should-not (string-match-p ":CHANGEKEY: first" text)))))

(ert-deftest org-outlook-sync-initial-round-ignores-old-cursor ()
  (org-outlook-test-with-responses
      (list (org-outlook-test-response
             200 "{\"value\":[],\"@odata.deltaLink\":\"https://graph.microsoft.com/new-delta\"}"))
    (cl-letf (((symbol-function 'org-outlook-last-delta)
               (lambda () "https://graph.microsoft.com/old-delta")))
      (org-outlook-retrieve-delta-pages nil t)
      (should (string-match-p "startDateTime=" (nth 1 (car calls))))
      (should-not (string-match-p "old-delta" (nth 1 (car calls)))))))

(ert-deftest org-outlook-sync-index-failure-retains-checkpoint ()
  (org-outlook-test-with-files
    (org-outlook-test-append-entry org-outlook-file "old")
    (let ((before (org-outlook-test-file-text org-outlook-file)))
      (cl-letf (((symbol-function 'org-outlook-retrieve-delta-pages)
                 (lambda (&rest _) (org-outlook-test-batch nil)))
                ((symbol-function 'org-id-locations-save)
                 (lambda () (error "Index write failed"))))
        (should-error (org-outlook-full-sync)))
      (should (equal before (org-outlook-test-file-text org-outlook-file)))
      (should-not committed))))

(ert-deftest org-outlook-sync-window-rollover-starts-full-round ()
  (let ((full 0) (delta 0)
        (org-outlook-full-sync-interval-days nil))
    (cl-letf (((symbol-function 'org-id-locations-load) #'ignore)
              ((symbol-function 'org-outlook--sync-state)
               (lambda () '(:delta "old" :start "2026-10-01" :end "2026-12-31")))
              ((symbol-function 'org-outlook-last-full-sync-time) (lambda () nil))
              ((symbol-function 'org-outlook-start) (lambda () "2026-10-02"))
              ((symbol-function 'org-outlook-end) (lambda () "2027-01-01"))
              ((symbol-function 'org-outlook-full-sync) (lambda () (cl-incf full)))
              ((symbol-function 'org-outlook-delta-sync-impl) (lambda () (cl-incf delta))))
      (org-outlook-sync)
      (should (= 1 full))
      (should (= 0 delta)))))

(ert-deftest org-outlook-sync-only-expired-cursors-trigger-recovery ()
  (dolist (failure '(gone forbidden throttled server))
    (let ((full 0) (org-outlook-full-sync-interval-days nil))
      (cl-letf (((symbol-function 'org-id-locations-load) #'ignore)
                ((symbol-function 'org-outlook--sync-state)
                 (lambda () '(:delta "old" :start "2026-10-01" :end "2026-12-31")))
                ((symbol-function 'org-outlook-last-full-sync-time) (lambda () nil))
                ((symbol-function 'org-outlook-start) (lambda () "2026-10-01"))
                ((symbol-function 'org-outlook-end) (lambda () "2026-12-31"))
                ((symbol-function 'org-outlook-full-sync) (lambda () (cl-incf full)))
                ((symbol-function 'org-outlook-delta-sync-impl)
                 (lambda () (signal 'org-outlook-graph-error
                                    (list "Fetch failed"
                                          (make-org-outlook-graph-result :error failure))))))
        (if (eq failure 'gone)
            (progn (org-outlook-sync) (should (= 1 full)))
          (should-error (org-outlook-sync) :type 'org-outlook-graph-error)
          (should (= 0 full)))))))

(ert-deftest org-outlook-sync-nested-managed-events-survive-parent-retirement ()
  (org-outlook-test-with-files
    (org-outlook-test-append-entry org-outlook-file "parent")
    (with-current-buffer (get-file-buffer org-outlook-file)
      (goto-char (point-max))
      (insert (org-outlook--nest-body (org-outlook-test-entry "child") 2))
      (save-buffer))
    (org-outlook--apply-sync-batch
     (org-outlook-test-batch (list (org-outlook-test-event "child" "old"))) t)
    (should (string-match-p ":OUTLOOK-ID: child"
                            (org-outlook-test-file-text org-outlook-file)))
    (should (string-match-p ":OUTLOOK-RETIRED-ID: parent"
                            (org-outlook-test-file-text org-outlook-file)))))

(ert-deftest org-outlook-sync-application-error-rolls-back-buffer-edits ()
  (org-outlook-test-with-files
    (org-outlook-test-append-entry org-outlook-file "old")
    (let ((before (org-outlook-test-file-text org-outlook-file))
          (insert-fn (symbol-function 'insert))
          failed)
      (cl-letf (((symbol-function 'insert)
                 (lambda (&rest args)
                   (if (and (not failed) (stringp (car args))
                            (string-match-p ":CHANGEKEY: new" (car args)))
                       (progn (setq failed t) (error "Insertion failed"))
                     (apply insert-fn args)))))
        (should-error (org-outlook--apply-sync-batch
                       (org-outlook-test-batch (list (org-outlook-test-event "old"))) nil)))
      (should failed)
      (should (equal before (org-outlook-test-file-text org-outlook-file)))
      (with-current-buffer (get-file-buffer org-outlook-file)
        (should (equal before (buffer-string)))
        (should-not (buffer-modified-p)))
      (should-not committed))))

(ert-deftest org-outlook-sync-real-id-index-precedes-checkpoint ()
  (let ((index-fn (symbol-function 'org-id-update-id-locations))
        (save-fn (symbol-function 'org-id-locations-save)))
    (org-outlook-test-with-files
      (let ((org-id-locations-file (expand-file-name "org-id-locations" directory))
            (org-id-extra-files nil)
            (org-agenda-files nil))
        (cl-letf (((symbol-function 'org-id-update-id-locations) index-fn)
                  ((symbol-function 'org-id-locations-save) save-fn))
          (org-outlook--apply-sync-batch
           (org-outlook-test-batch (list (org-outlook-test-event "indexed"))) nil)
          (should (equal (expand-file-name org-outlook-file)
                         (gethash (secure-hash 'sha256 "indexed") org-id-locations)))
          (should (file-exists-p org-id-locations-file)))))))

(ert-deftest org-outlook-sync-missing-event-id-aborts-before-pruning ()
  (org-outlook-test-with-files
    (org-outlook-test-append-entry org-outlook-file "old")
    (let ((before (org-outlook-test-file-text org-outlook-file)))
      (should-error (org-outlook--apply-sync-batch
                     (org-outlook-test-batch '(((subject . "Malformed")))) t)
                    :type 'org-outlook-graph-error)
      (should (equal before (org-outlook-test-file-text org-outlook-file))))))

(ert-deftest org-outlook-sync-window-uses-configured-timezone ()
  (let ((org-outlook-local-timezone "Europe/Berlin"))
    (with-temp-buffer
      (org-mode)
      (insert "** Boundary\n<2026-10-01 Thu 01:00-01:30>\n")
      (goto-char (point-min))
      ;; Both times are on September 30 in UTC, outside the October window.
      (should-not (org-outlook--entry-in-window-p "2026-10-01" "2026-12-31")))))

(provide 'org-outlook-sync-test)
;;; org-outlook-sync-test.el ends here
