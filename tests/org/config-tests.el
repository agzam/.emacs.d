;;; tests/org/config-tests.el --- org module wiring specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defun org-config-tests--use-package (name)
  "Return the `use-package' form for NAME in the org module config.
The specs read source: config.el cannot load in the batch tier, where
neither `use-package' nor general (behind `map!') exists."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "modules/org/config.el" test-config-root))
    (let (found)
      (condition-case nil
          (while (not found)
            (let ((form (read (current-buffer))))
              (when (and (eq (car-safe form) 'use-package)
                         (eq (cadr form) name))
                (setq found form))))
        (end-of-file nil))
      found)))

(describe "org module config"
  (it "leaves the note graph to vulpea"
    (expect (org-config-tests--use-package 'vulpea) :to-be-truthy)
    (expect (org-config-tests--use-package 'org-roam) :to-be nil)
    (expect (org-config-tests--use-package 'org-roam-ui) :to-be nil))

  (it "keeps the org settings that notes rely on"
    (let ((config (flatten-tree
                   (use-package-body-forms
                    (cdr (org-config-tests--use-package 'org)) :config))))
      (expect (memq 'org-export-with-broken-links config) :to-be-truthy)
      (expect (memq 'org-id-link-to-org-use-id config) :to-be-truthy)
      (expect (member "roam_aliases" config) :to-be-truthy)
      (expect (member "roam_refs" config) :to-be-truthy)
      (expect (memq 'backlink-count-mode config) :to-be-truthy))))

;;; config-tests.el ends here
