;;; org-outlook-auth-test.el --- OAuth regression tests -*- lexical-binding: t; -*-
(require 'org-outlook-graph-test)

(ert-deftest org-outlook-auth-state-path-and-replay ()
  (let ((org-outlook--auth-running t) (org-outlook--auth-state "expected")
        org-outlook--auth-code org-outlook--auth-complete org-outlook--auth-error)
    (dolist (params '(nil (("state" . "wrong") ("code" . "secret"))
                         (("state" . "expected") ("state" . "expected") ("code" . "secret"))))
      (should (= 400 (car (org-outlook--oauth-callback 'GET "/" params))))
      (should-not org-outlook--auth-complete))
    (should (= 404 (car (org-outlook--oauth-callback 'POST "/" nil))))
    (should (= 404 (car (org-outlook--oauth-callback 'GET "/other" nil))))
    (let ((reply (org-outlook--oauth-callback
                  'GET "/" '(("state" . "expected") ("code" . "secret")))))
      (should (= 200 (car reply)))
      (should-not (string-match-p "secret" (cdr reply)))
      (should (equal "secret" org-outlook--auth-code))
      (should (= 409 (car (org-outlook--oauth-callback 'GET "/" nil)))))))

(ert-deftest org-outlook-auth-denial-terminates-with-safe-error ()
  (let ((org-outlook--auth-running t) (org-outlook--auth-state "expected")
        org-outlook--auth-code org-outlook--auth-complete org-outlook--auth-error)
    (org-outlook--oauth-callback
     'GET "/" '(("state" . "expected") ("error" . "access_denied")
                ("error_description" . "SECRET")))
    (should org-outlook--auth-complete)
    (should-not org-outlook--auth-code)
    (should-not (string-match-p "SECRET" org-outlook--auth-error))))

(ert-deftest org-outlook-auth-raw-query-decoded-once ()
  (let ((org-outlook--auth-running t) (org-outlook--auth-state "expected")
        org-outlook--auth-code org-outlook--auth-complete org-outlook--auth-error)
    (org-outlook--handle-auth-http-request
     "GET /?state=expected&code=a%2Bb%26c%3Dd HTTP/1.1\r\n\r\n")
    (should (equal "a+b&c=d" org-outlook--auth-code))))

(ert-deftest org-outlook-auth-native-request-validates-state-and-replay ()
  (let ((org-outlook--auth-running t) (org-outlook--auth-state "expected")
        org-outlook--auth-code org-outlook--auth-complete org-outlook--auth-error)
    (let ((request
           "GET /?state=expected&code=a%2Bb%26c%3Dd HTTP/1.1\r\nHost: localhost\r\n\r\n"))
      (should (= 200 (car (org-outlook--handle-auth-http-request request))))
      (should (equal "a+b&c=d" org-outlook--auth-code))
      (should (= 409 (car (org-outlook--handle-auth-http-request request)))))))

(ert-deftest org-outlook-auth-native-request-rejects-invalid-input ()
  (let ((org-outlook--auth-running t) (org-outlook--auth-state "expected")
        org-outlook--auth-code org-outlook--auth-complete org-outlook--auth-error)
    (dolist (request
             '("not HTTP"
               "GET https://example.com/?state=expected&code=secret HTTP/1.1\r\n\r\n"
               "GET /?state=wrong&code=secret HTTP/1.1\r\n\r\n"))
      (should (= 400 (car (org-outlook--handle-auth-http-request request))))
      (should-not org-outlook--auth-complete)
      (should-not org-outlook--auth-code))
    (should (= 404 (car (org-outlook--handle-auth-http-request
                         "POST /?state=expected&code=secret HTTP/1.1\r\n\r\n"))))
    (should (= 404 (car (org-outlook--handle-auth-http-request
                         "GET /other?state=expected&code=secret HTTP/1.1\r\n\r\n"))))))

(ert-deftest org-outlook-auth-starts-native-listener ()
  (cl-letf (((symbol-function 'org-outlook--make-auth-listener)
             (lambda (filter &optional service sentinel)
               (should (eq filter #'org-outlook--auth-http-filter))
               (should-not service)
               (should-not sentinel)
               'listener)))
    (should (eq 'listener (org-outlook--start-auth-code-server)))))

(ert-deftest org-outlook-auth-native-listener-is-loopback-only ()
  (let (arguments)
    (cl-letf (((symbol-function 'make-network-process)
               (lambda (&rest args)
                 (setq arguments args)
                 'listener)))
      (should (eq 'listener
                  (org-outlook--make-auth-listener #'ignore t #'ignore)))
      (should (equal "org-outlook-auth" (plist-get arguments :name)))
      (should (eq t (plist-get arguments :server)))
      (should (equal "127.0.0.1" (plist-get arguments :host)))
      (should (eq t (plist-get arguments :service)))
      (should (eq 'ipv4 (plist-get arguments :family)))
      (should (eq 'binary (plist-get arguments :coding)))
      (should (eq nil (plist-get arguments :filter-multibyte)))
      (should (eq t (plist-get arguments :noquery)))
      (should (eq #'ignore (plist-get arguments :filter)))
      (should (eq #'ignore (plist-get arguments :sentinel)))
      (should (eq #'org-outlook--auth-client-log
                  (plist-get arguments :log))))))

(ert-deftest org-outlook-auth-http-response-is-minimal-and-safe ()
  (let* ((response (org-outlook--auth-http-response '(200 . "R\u00e9\u00e7u")))
         (body-start (+ 4 (string-match "\r\n\r\n" response)))
         (body (substring response body-start)))
    (should (string-prefix-p "HTTP/1.1 200 OK\r\n" response))
    (should (string-match-p "Content-Type: text/plain; charset=utf-8\r\n" response))
    (should (string-match-p "Content-Length: 6\r\n" response))
    (should (string-match-p "Cache-Control: no-store\r\n" response))
    (should (string-match-p "Connection: close\r\n" response))
    (should (equal "R\u00e9\u00e7u" (decode-coding-string body 'utf-8)))))

(ert-deftest org-outlook-auth-http-filter-assembles-and-closes ()
  (let ((org-outlook--auth-running t) (org-outlook--auth-state "expected")
        org-outlook--auth-code org-outlook--auth-complete org-outlook--auth-error
        (properties (make-hash-table :test #'equal)) sent closed)
    (cl-letf (((symbol-function 'process-get)
               (lambda (process property)
                 (gethash (cons process property) properties)))
              ((symbol-function 'process-put)
               (lambda (process property value)
                 (puthash (cons process property) value properties)))
              ((symbol-function 'process-send-string)
               (lambda (process string)
                 (should (eq process 'client))
                 (setq sent string)))
              ((symbol-function 'delete-process)
               (lambda (process)
                 (should (eq process 'client))
                 (setq closed t))))
      (org-outlook--auth-http-filter
       'client "GET /?state=expected&code=a%2Bb%26c%3Dd HTTP/1.1\r\n")
      (should-not sent)
      (should-not closed)
      (org-outlook--auth-http-filter 'client "Host: localhost\r\n\r\n")
      (should (equal "a+b&c=d" org-outlook--auth-code))
      (should (string-prefix-p "HTTP/1.1 200 OK\r\n" sent))
      (should-not (string-match-p "a+b&c=d" sent))
      (should closed)
      (let ((first-response sent))
        (org-outlook--auth-http-filter 'client "ignored")
        (should (equal first-response sent))))))

(ert-deftest org-outlook-auth-http-filter-rejects-oversized-request ()
  (let ((org-outlook--auth-running t) (org-outlook--auth-state "expected")
        org-outlook--auth-code org-outlook--auth-complete
        (properties (make-hash-table :test #'equal)) sent closed)
    (cl-letf (((symbol-function 'process-get)
               (lambda (process property)
                 (gethash (cons process property) properties)))
              ((symbol-function 'process-put)
               (lambda (process property value)
                 (puthash (cons process property) value properties)))
              ((symbol-function 'process-send-string)
               (lambda (_ string) (setq sent string)))
              ((symbol-function 'delete-process)
               (lambda (_) (setq closed t))))
      (org-outlook--auth-http-filter 'client (make-string 8193 ?x))
      (should (string-prefix-p "HTTP/1.1 400 Bad Request\r\n" sent))
      (should closed)
      (should-not org-outlook--auth-code)
      (should-not org-outlook--auth-complete))))

(ert-deftest org-outlook-auth-native-listener-round-trip ()
  (let ((org-outlook--auth-running t) (org-outlook--auth-state "expected")
        org-outlook--auth-code org-outlook--auth-complete org-outlook--auth-error
        response client server)
    (unwind-protect
        (progn
          (setq server
                (org-outlook--make-auth-listener
                 #'org-outlook--auth-http-filter t)
                client
                (make-network-process
                 :name "org-outlook-auth-test-client"
                 :host "127.0.0.1"
                 :service (process-contact server :service)
                 :family 'ipv4
                 :coding 'binary
                 :noquery t
                 :filter (lambda (_ chunk)
                           (setq response (concat response chunk)))))
          (process-send-string
           client "GET /?state=expected&code=round-trip-secret HTTP/1.1\r\n")
          (accept-process-output nil 0.01)
          (should-not org-outlook--auth-complete)
          (process-send-string client "Host: localhost\r\n\r\n")
          (let ((deadline (+ (float-time) 2)))
            (while (and (not response) (< (float-time) deadline))
              (accept-process-output nil 0.01)))
          (should org-outlook--auth-complete)
          (should (equal "round-trip-secret" org-outlook--auth-code))
          (should (string-prefix-p "HTTP/1.1 200 OK\r\n" response))
          (should (string-match-p "Connection: close\r\n" response))
          (should-not (string-match-p "round-trip-secret" response)))
      (when (processp client) (ignore-errors (delete-process client)))
      (org-outlook--close-auth-listener server))))

(ert-deftest org-outlook-auth-listener-closes-all-partial-clients ()
  (let (server clients accepted timers)
    (unwind-protect
        (progn
          (setq server
                (org-outlook--make-auth-listener
                 #'org-outlook--auth-http-filter t))
          (dotimes (index 2)
            (push (make-network-process
                   :name (format "org-outlook-auth-partial-%d" index)
                   :host "127.0.0.1"
                   :service (process-contact server :service)
                   :family 'ipv4 :coding 'binary :noquery t)
                  clients))
          (let ((deadline (+ (float-time) 2)))
            (while (and (< (length (process-get server 'org-outlook-auth-clients)) 2)
                        (< (float-time) deadline))
              (accept-process-output nil 0.01)))
          (setq accepted
                (copy-sequence
                 (process-get server 'org-outlook-auth-clients))
                timers
                (mapcar (lambda (client)
                          (process-get client 'org-outlook-auth-timer))
                        accepted))
          (should (= 2 (length accepted)))
          (should (seq-every-p #'timerp timers))
          (org-outlook--close-auth-listener server)
          (should-not (seq-some #'process-live-p accepted))
          (should-not (seq-some (lambda (timer) (memq timer timer-list)) timers)))
      (dolist (client clients) (ignore-errors (delete-process client)))
      (org-outlook--close-auth-listener server))))

(ert-deftest org-outlook-auth-partial-client-expires-when-idle ()
  (let ((org-outlook-auth-client-timeout 0.01) server client accepted timer)
    (unwind-protect
        (progn
          (setq server
                (org-outlook--make-auth-listener
                 #'org-outlook--auth-http-filter t)
                client
                (make-network-process
                 :name "org-outlook-auth-idle-client"
                 :host "127.0.0.1"
                 :service (process-contact server :service)
                 :family 'ipv4 :coding 'binary :noquery t))
          (process-send-string client "GET /?partial")
          (let ((deadline (+ (float-time) 2)))
            (while (and (null (process-get server 'org-outlook-auth-clients))
                        (< (float-time) deadline))
              (accept-process-output nil 0.01)))
          (setq accepted (car (process-get server 'org-outlook-auth-clients))
                timer (and accepted
                           (process-get accepted 'org-outlook-auth-timer)))
          (should (processp accepted))
          (let ((deadline (+ (float-time) 2)))
            (while (and (process-live-p accepted) (< (float-time) deadline))
              (accept-process-output nil 0.01)))
          (should-not (process-live-p accepted))
          (should-not (process-get server 'org-outlook-auth-clients))
          (should-not (memq timer timer-list)))
      (when (processp client) (ignore-errors (delete-process client)))
      (org-outlook--close-auth-listener server))))

(defmacro org-outlook-test-with-login (&rest body)
  (declare (indent 0))
  `(let ((org-outlook--auth-running nil) stopped timer
         (schedule (symbol-function 'run-at-time)))
     (cl-letf (((symbol-function 'org-outlook--start-auth-code-server)
                (lambda () 'listener))
               ((symbol-function 'delete-process) (lambda (_) (setq stopped t)))
               ((symbol-function 'run-at-time)
                (lambda (&rest args) (setq timer (apply schedule args)))))
       ,@body)))

(ert-deftest org-outlook-auth-fresh-pkce-and-success-cleanup ()
  (org-outlook-test-with-login
    (let (requests)
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url &rest _)
                   (push url requests)
                   (org-outlook--oauth-callback
                    'GET "/" (list (cons "state" org-outlook--auth-state)
                                   (cons "code" "authorization"))))))
        (let* ((first (org-outlook-request-authorization))
               (second (org-outlook-request-authorization))
               (query (url-parse-query-string (cadr (split-string (car requests) "?"))))
               (expected (replace-regexp-in-string
                          "=+\\'" "" (base64url-encode-string
                                      (secure-hash 'sha256 (plist-get second :verifier) nil nil t)))))
          (should (equal "authorization" (plist-get first :code)))
          (should (= 43 (length (plist-get first :verifier))))
          (should-not (equal (plist-get first :verifier) (plist-get second :verifier)))
          (should (equal expected (cadr (assoc "code_challenge" query))))
          (should stopped)
          (should-not (memq timer timer-list))
          (should-not org-outlook--auth-running))))))

(ert-deftest org-outlook-auth-timeout-releases-listener ()
  (org-outlook-test-with-login
    (let ((org-outlook-auth-timeout 0))
      (cl-letf (((symbol-function 'browse-url) (lambda (&rest _) nil)))
        (should-error (org-outlook-request-authorization) :type 'org-outlook-auth-error)
        (should stopped)
        (should-not (memq timer timer-list))))))

(ert-deftest org-outlook-auth-browser-error-and-quit-cleanup ()
  (dolist (condition '(error quit))
    (org-outlook-test-with-login
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (&rest _) (signal condition '("Browser stopped")))))
        (condition-case err
            (progn (org-outlook-request-authorization) (ert-fail "Expected exit"))
          ((error quit) (should (eq condition (car err)))))
        (should stopped)
        (should-not (memq timer timer-list))))))

(ert-deftest org-outlook-auth-dynamic-tenant ()
  (let ((org-outlook-tenant-id "tenant.example"))
    (should (equal "https://login.microsoftonline.com/tenant.example/oauth2/v2.0/token"
                   (org-outlook--oauth-url "token"))))
  (let ((org-outlook-tenant-id "../other"))
    (should-error (org-outlook--oauth-url "token") :type 'user-error)))

(ert-deftest org-outlook-auth-revocation-fallback-and-single-cache-write ()
  (let ((queue (list (make-org-outlook-graph-result
                     :status 400 :error 'http :data '((error . "invalid_grant")))
                    (make-org-outlook-graph-result
                     :status 200 :data '((access_token . "ACCESS") (refresh_token . "ROTATED")
                                         (expires_in . 7200)))))
        forms saved (writes 0) (logins 0))
    (cl-letf (((symbol-function 'org-outlook-refresh-token) (lambda () "REVOKED"))
              ((symbol-function 'org-outlook-request-authorization)
               (lambda () (cl-incf logins) '(:code "CODE" :verifier "VERIFIER")))
              ((symbol-function 'org-outlook--exchange-token)
               (lambda (form) (push form forms) (pop queue)))
              ((symbol-function 'org-outlook-set-token-field)
               (lambda (&rest args) (cl-incf writes) (setq saved args))))
      (should (equal "ACCESS" (org-outlook-request-access-token)))
      (should (= 1 logins)) (should (= 1 writes))
      (should (equal "VERIFIER" (cdr (assoc "code_verifier" (car forms)))))
      (should-not (assoc "code_verifier" (cadr forms)))
      (should (equal "ROTATED" (plist-get (nth 2 saved) :refresh)))
      (should (> (float-time (time-subtract
                             (parse-iso8601-time-string (plist-get (nth 1 saved) :expires))
                             (current-time))) 7100)))))

(ert-deftest org-outlook-auth-transient-refresh-does-not-login-or-leak ()
  (cl-letf (((symbol-function 'org-outlook-refresh-token) (lambda () "REFRESH-SECRET"))
            ((symbol-function 'org-outlook-request-authorization)
             (lambda () (ert-fail "Unexpected login")))
            ((symbol-function 'org-outlook--exchange-token)
             (lambda (_) (make-org-outlook-graph-result
                          :status 503 :error 'server :data '((error_description . "SECRET"))))))
    (let ((err (should-error (org-outlook-request-access-token) :type 'org-outlook-auth-error)))
      (should-not (string-match-p "SECRET" (error-message-string err))))))

(ert-deftest org-outlook-auth-missing-rotation-retains-refresh ()
  (let (saved)
    (cl-letf (((symbol-function 'org-outlook-refresh-token) (lambda () "OLD"))
              ((symbol-function 'org-outlook--exchange-token)
               (lambda (_) (make-org-outlook-graph-result
                            :status 200 :data '((access_token . "ACCESS") (expires_in . 3600)))))
              ((symbol-function 'org-outlook-set-token-field)
               (lambda (&rest args) (setq saved args))))
      (org-outlook-request-access-token)
      (should (equal "OLD" (plist-get (nth 2 saved) :refresh))))))

(ert-deftest org-outlook-auth-expiry-and-tenant-scoping ()
  (cl-letf (((symbol-function 'org-outlook--oauth-cache)
             (lambda () '(:access "OLD" :expires "2000-01-01T00:00:00Z" :refresh "REFRESH"))))
    (should (org-outlook-token-expired-p))
    (should-not (org-outlook-token-expired-p "refresh")))
  (let ((org-outlook-client-id "new") (org-outlook-tenant-id "new") closed)
    (cl-letf (((symbol-function 'plstore-open) (lambda (_) 'cache))
              ((symbol-function 'plstore-get)
               (lambda (&rest _) '("oauth" :client "old" :tenant "old" :access "SECRET")))
              ((symbol-function 'plstore-close) (lambda (_) (setq closed t))))
      (should-not (org-outlook--oauth-cache)) (should closed))))

(ert-deftest org-outlook-auth-failed-save-closes-store ()
  (let ((org-outlook-token-cache-file "/tmp/org-outlook-test-unused/cache")
        (org-outlook-gpg-recipient "test@example.com") closed)
    (cl-letf (((symbol-function 'make-directory) (lambda (&rest _) nil))
              ((symbol-function 'plstore-open) (lambda (_) 'cache))
              ((symbol-function 'plstore-put) (lambda (&rest _) nil))
              ((symbol-function 'plstore-save) (lambda (_) (error "Disk failure")))
              ((symbol-function 'plstore-close) (lambda (_) (setq closed t))))
      (should-error (org-outlook-set-token-field "oauth" nil '(:access "SECRET")))
      (should closed))))

(ert-deftest org-outlook-auth-no-insecure-random-fallback ()
  (cl-letf (((symbol-function 'file-readable-p) (lambda (_) nil))
            ((symbol-function 'executable-find) (lambda (_) nil)))
    (should-error (org-outlook-generate-random-string 43) :type 'org-outlook-auth-error)))

(ert-deftest org-outlook-auth-corrupt-expiry-requires-refresh ()
  (cl-letf (((symbol-function 'org-outlook--oauth-cache)
             (lambda () '(:access "OLD" :expires "not-a-date"))))
    (should (org-outlook-token-expired-p))))

(ert-deftest org-outlook-auth-access-token-is-cached-in-memory ()
  (let ((org-outlook--access-token-cache nil)
        (expires (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                     (time-add (current-time) 3600) t))
        (reads 0))
    (cl-letf (((symbol-function 'org-outlook--oauth-cache)
               (lambda ()
                 (cl-incf reads)
                 (list :client org-outlook-client-id :tenant org-outlook-tenant-id
                       :expires expires :access "ACCESS" :refresh "REFRESH"))))
      (should (equal "ACCESS" (org-outlook-bearer-token)))
      (should (equal "ACCESS" (org-outlook-bearer-token)))
      (should (= 1 reads))
      (should-not (plist-member org-outlook--access-token-cache :refresh)))))

(ert-deftest org-outlook-auth-memory-cache-is-scoped-and-refresh-invalidates ()
  (let ((org-outlook--access-token-cache
         (list :client "old" :tenant "old" :expires "2999-01-01T00:00:00Z"
               :access "OLD"))
        (reads 0))
    (cl-letf (((symbol-function 'org-outlook--oauth-cache)
               (lambda ()
                 (cl-incf reads)
                 (list :client org-outlook-client-id :tenant org-outlook-tenant-id
                       :expires "2999-01-01T00:00:00Z" :access "CURRENT"))))
      (should (equal "CURRENT" (org-outlook-bearer-token)))
      (should (= 1 reads)))
    (cl-letf (((symbol-function 'org-outlook-request-access-token)
               (lambda ()
                 (should-not org-outlook--access-token-cache)
                 "FRESH")))
      (should (equal "FRESH" (org-outlook-force-token-refresh))))))

(provide 'org-outlook-auth-test)
;;; org-outlook-auth-test.el ends here
