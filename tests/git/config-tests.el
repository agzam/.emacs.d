;;; tests/git/config-tests.el --- modules/git/config.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defun git-config-tests--top-level-forms ()
  "Read every top-level form out of the git module's config, in file order."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "modules/git/config.el" test-config-root))
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(describe "forge bindings"
  ;; evil-collection-forge installs forge's magit bindings on evil-friendly
  ;; keys and resets this flag, with a message, whenever it finds it set.
  ;; Forge's autoloads define the flag t and consult it as soon as magit-mode
  ;; loads, and with :ensure heading `use-package-keywords' every keyword of
  ;; the use-package form (:preface included) runs inside elpaca's activation
  ;; callback, after those autoloads.  Only a top-level form ahead of the
  ;; use-package binds it early enough.
  (it "binds forge-add-default-bindings nil at top level, before use-package forge"
    (let* ((forms (git-config-tests--top-level-forms))
           (declaration (cl-position '(defvar forge-add-default-bindings nil)
                                     forms :test #'equal))
           (package (cl-position-if (lambda (f)
                                      (and (eq (car-safe f) 'use-package)
                                           (eq (cadr f) 'forge)))
                                    forms)))
      (expect declaration :not :to-be nil)
      (expect package :not :to-be nil)
      (expect declaration :to-be-less-than package))))

;;; tests/git/config-tests.el ends here
