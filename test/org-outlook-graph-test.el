;;; org-outlook-graph-test.el --- Graph transport regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org-outlook)

(defmacro org-outlook-test-with-responses (responses &rest body)
  "Run BODY with queued plz RESPONSES and fresh in-memory credentials.
Expose CALLS and REFRESHES to BODY.  No network or token cache is used."
  (declare (indent 1))
  `(let ((queue ,responses) calls (refreshes 0) (token "old-token"))
     (cl-letf (((symbol-function 'org-outlook-bearer-token) (lambda () token))
               ((symbol-function 'org-outlook-force-token-refresh)
                (lambda () (cl-incf refreshes) (setq token "new-token")))
               ((symbol-function 'plz)
                (lambda (method url &rest args)
                  (push (list method url args) calls)
                  (unless queue (error "Unexpected request"))
                  (let ((response (pop queue)))
                    (if (plz-error-p response)
                        (signal 'plz-error (list response))
                      response)))))
       ,@body)))

(defun org-outlook-test-response (status &optional body headers)
  "Construct a plz response with STATUS, BODY and HEADERS."
  (make-plz-response :status status :body (or body "") :headers headers))

(defun org-outlook-test-http-error (status &optional body headers)
  "Construct a plz HTTP error with STATUS, BODY and HEADERS."
  (make-plz-error :response (org-outlook-test-response status body headers)))

(ert-deftest org-outlook-graph-json-and-empty-success ()
  (org-outlook-test-with-responses
      (list (org-outlook-test-response 201 "{\"id\":\"event\",\"isCancelled\":false}")
            (org-outlook-test-response 202)
            (org-outlook-test-response 204))
    (let ((result (org-outlook-graph-request 'post org-outlook-events-create-url
                                           :body '(("subject" . "Planning")))))
      (should (= 201 (org-outlook-graph-result-status result)))
      (should-not (org-outlook-graph-result-error result))
      (should (eq :json-false (alist-get 'isCancelled (org-outlook-graph-result-data result))))
      (should (equal "{\"subject\":\"Planning\"}"
                     (plist-get (nth 2 (car calls)) :body))))
    (dolist (method '(post delete))
      (let ((result (org-outlook-graph-request method org-outlook-events-create-url)))
        (should-not (org-outlook-graph-result-error result))
        (should-not (org-outlook-graph-result-data result))))))

(ert-deftest org-outlook-graph-refresh-once-and-replay-payload ()
  (org-outlook-test-with-responses
      (list (org-outlook-test-http-error 401)
            (org-outlook-test-response 200 "{\"id\":\"updated\"}"))
    (should (equal '((id . "updated"))
                   (org-outlook--graph-data 'patch org-outlook-events-create-url
                                            :body '(("subject" . "Updated")))))
    (should (= 1 refreshes))
    (should (= 2 (length calls)))
    (should (equal (plist-get (nth 2 (car calls)) :body)
                   (plist-get (nth 2 (cadr calls)) :body)))
    (should (equal "Bearer new-token"
                   (cdr (assoc "Authorization" (plist-get (nth 2 (car calls)) :headers)))))))

(ert-deftest org-outlook-graph-second-401-is-an-error ()
  (org-outlook-test-with-responses
      (list (org-outlook-test-http-error 401) (org-outlook-test-http-error 401))
    (should-error (org-outlook--graph-data 'get org-outlook-events-url)
                  :type 'org-outlook-graph-error)
    (should (= 1 refreshes))
    (should (= 2 (length calls)))))

(ert-deftest org-outlook-graph-other-errors-never-refresh-or-replay ()
  (dolist (case '((403 . forbidden) (404 . not-found) (410 . gone)
                  (429 . throttled) (500 . server) (503 . server)))
    (org-outlook-test-with-responses
        (list (org-outlook-test-http-error (car case) "{\"error\":{\"code\":\"failure\"}}"
                                           '((retry-after . "120"))))
      (let ((result (org-outlook-graph-request 'post org-outlook-events-create-url)))
        (should (eq (cdr case) (org-outlook-graph-result-error result)))
        (should (equal "120" (org-outlook-graph-result-retry-after result)))
        (should (= (car case) (org-outlook-graph-result-status result))))
      (should (= 0 refreshes))
      (should (= 1 (length calls))))))

(ert-deftest org-outlook-graph-network-and-malformed-json ()
  (org-outlook-test-with-responses
      (list (make-plz-error :curl-error '(7 . "Connection refused"))
            (org-outlook-test-response 200 "<html>gateway</html>"))
    (should (eq 'network (org-outlook-graph-result-error
                         (org-outlook-graph-request 'post org-outlook-events-create-url))))
    (should-error (org-outlook--graph-data 'get org-outlook-events-url)
                  :type 'org-outlook-graph-error)
    (should (= 0 refreshes))
    (should (= 2 (length calls)))))

(ert-deftest org-outlook-graph-rejects-untrusted-continuations ()
  (org-outlook-test-with-responses nil
    (dolist (url '("https://example.com/next" "http://graph.microsoft.com/next"
                   "https://graph.microsoft.com.evil.test/next"
                   "https://user@graph.microsoft.com/next"
                   "https://graph.microsoft.com:444/next"))
      (should-error (org-outlook-graph-request 'get url)))
    (should-not calls)))

(ert-deftest org-outlook-graph-calendar-pagination-is-opaque-and-order-independent ()
  (let ((next "https://graph.microsoft.com/v1.0/me/calendarView?$skiptoken=opaque%2Btoken&$top=7"))
    (org-outlook-test-with-responses
        (list (org-outlook-test-response
               200 (json-encode `(("@odata.nextLink" . ,next) ("value" . [((id . "one"))]))))
              (org-outlook-test-response 200 "{\"value\":[{\"id\":\"two\"}]}"))
      (should (equal '(((id . "one")) ((id . "two"))) (org-outlook-retrieve-pages)))
      (should (equal next (nth 1 (car calls))))
      (should (= 0 refreshes)))))

(ert-deftest org-outlook-graph-delta-pagination ()
  (let ((next "https://graph.microsoft.com/v1.0/me/calendarView/delta?$skiptoken=opaque")
        saved)
    (org-outlook-test-with-responses
        (list (org-outlook-test-response
               200 (json-encode `(("value" . []) ("@odata.nextLink" . ,next))))
              (org-outlook-test-response 200 "{\"@odata.deltaLink\":\"https://graph.microsoft.com/delta\",\"value\":[]}"))
      (cl-letf (((symbol-function 'org-outlook-last-delta) (lambda () nil))
                ((symbol-function 'org-outlook-set-token-field)
                 (lambda (&rest args) (setq saved args))))
        (let ((batch (org-outlook-retrieve-delta-pages)))
          (should-not (org-outlook-sync-batch-events batch))
          (should (equal "https://graph.microsoft.com/delta"
                         (org-outlook-sync-batch-delta batch))))
        (should (equal next (nth 1 (car calls))))
        (should-not saved)))))

(ert-deftest org-outlook-graph-failed-page-does-not-return-partial-events ()
  (org-outlook-test-with-responses
      (list (org-outlook-test-response
             200 "{\"value\":[{\"id\":\"one\"}],\"@odata.nextLink\":\"https://graph.microsoft.com/next\"}")
            (org-outlook-test-http-error 503))
    (should-error (org-outlook-retrieve-pages) :type 'org-outlook-graph-error)))

(ert-deftest org-outlook-graph-failed-delta-page-does-not-save-token ()
  (let (saved)
    (org-outlook-test-with-responses
        (list (org-outlook-test-response
               200 "{\"value\":[],\"@odata.nextLink\":\"https://graph.microsoft.com/next\"}")
              (org-outlook-test-http-error 410))
      (cl-letf (((symbol-function 'org-outlook-last-delta) (lambda () nil))
                ((symbol-function 'org-outlook-set-token-field)
                 (lambda (&rest _) (setq saved t))))
        (should-error (org-outlook-retrieve-delta-pages) :type 'org-outlook-graph-error)
        (should-not saved)))))

(ert-deftest org-outlook-graph-decline-cleans-up-only-after-success ()
  (dolist (status '(202 403))
    (let (deleted)
      (org-outlook-test-with-responses
          (list (if (= status 202) (org-outlook-test-response status)
                  (org-outlook-test-http-error status)))
        (cl-letf (((symbol-function 'org-outlook-get-appointment-property)
                   (lambda (property) (if (equal property "ID") "local-id" "remote-id")))
                  ((symbol-function 'org-outlook-delete-by-id)
                   (lambda (id) (setq deleted id))))
          (if (= status 202)
              (progn (org-outlook-respond-to-event "decline" "" "Y")
                     (should (equal "local-id" deleted)))
            (should-error (org-outlook-respond-to-event "decline" "" "Y")
                          :type 'org-outlook-graph-error)
            (should-not deleted)))))))

(ert-deftest org-outlook-graph-invalid-or-cyclic-pages-fail ()
  (dolist (body '("{}" "{\"value\":null}" "{\"value\":{}}"))
    (org-outlook-test-with-responses (list (org-outlook-test-response 200 body))
      (should-error (org-outlook-retrieve-pages) :type 'org-outlook-graph-error)))
  (org-outlook-test-with-responses
      (list (org-outlook-test-response 200 "{\"value\":[],\"@odata.nextLink\":\"https://graph.microsoft.com/next\"}")
            (org-outlook-test-response 200 "{\"value\":[],\"@odata.nextLink\":\"https://graph.microsoft.com/next\"}"))
    (should-error (org-outlook-retrieve-pages) :type 'org-outlook-graph-error)))

(ert-deftest org-outlook-graph-failed-full-sync-is-not-recorded ()
  (let ((org-id-locations (make-hash-table :test #'equal)) recorded)
    (org-outlook-test-with-responses (list (org-outlook-test-http-error 403))
      (cl-letf (((symbol-function 'org-outlook--commit-sync)
                 (lambda (&rest _) (setq recorded t))))
        (should-error (org-outlook-full-sync) :type 'org-outlook-graph-error)
        (should-not recorded)))))

(ert-deftest org-outlook-graph-availability-http-error-is-not-free ()
  (org-outlook-test-with-responses (list (org-outlook-test-http-error 403))
    (should-error (org-outlook-persons-available-p
                   '("colleague@example.com") "2026-10-21T09:00:00" "2026-10-21T10:00:00")
                  :type 'org-outlook-graph-error)))

(ert-deftest org-outlook-graph-timezone-header-is-escaped-for-curl-config ()
  (org-outlook-test-with-responses (list (org-outlook-test-response 200 "{\"value\":[]}"))
    (org-outlook-retrieve-events)
    (should (equal "IdType=\\\"ImmutableId\\\", outlook.timezone=\\\"UTC\\\""
                   (cdr (assoc "Prefer" (plist-get (nth 2 (car calls)) :headers)))))))

(provide 'org-outlook-graph-test)
;;; org-outlook-graph-test.el ends here
