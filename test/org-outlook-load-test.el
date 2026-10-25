;;; org-outlook-load-test.el --- Clean-load regression tests -*- lexical-binding: t; -*-

(require 'ert)

(defconst org-outlook-test-html2org-loaded-before-package
  (featurep 'html2org))
(defconst org-outlook-test-org-msg-loaded-before-package
  (featurep 'org-msg))

(require 'org-outlook)

(defconst org-outlook-test-html2org-loaded-by-package
  (and (not org-outlook-test-html2org-loaded-before-package)
       (featurep 'html2org)))
(defconst org-outlook-test-org-msg-loaded-by-package
  (and (not org-outlook-test-org-msg-loaded-before-package)
       (featurep 'org-msg)))

(ert-deftest org-outlook-load-defers-feature-specific-dependencies ()
  "Loading org-outlook must defer feature-specific packages."
  (should-not org-outlook-test-html2org-loaded-by-package)
  (should-not org-outlook-test-org-msg-loaded-by-package))

(provide 'org-outlook-load-test)
;;; org-outlook-load-test.el ends here
