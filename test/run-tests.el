;;; run-tests.el --- Batch entry point -*- lexical-binding: t; -*-
;; emacs --batch --eval "(package-initialize)" -l test/run-tests.el
(let* ((tests (file-name-directory (or load-file-name buffer-file-name)))
       (root (file-name-directory (directory-file-name tests))))
  (add-to-list 'load-path root)
  (add-to-list 'load-path tests))
(require 'org-outlook-load-test)
(require 'org-outlook-auth-test)
(require 'org-outlook-time-workflow-test)
(ert-run-tests-batch-and-exit)
