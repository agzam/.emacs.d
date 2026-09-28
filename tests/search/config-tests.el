;;; tests/search/config-tests.el --- modules/search/config.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

(defun search-config-tests--use-package (name)
  "The `use-package' form for NAME in the search module's config."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "modules/search/config.el" test-config-root))
    (goto-char (point-min))
    (let (found)
      (condition-case nil
          (while (not found)
            (let ((form (read (current-buffer))))
              (when (and (eq (car-safe form) 'use-package)
                         (eq (cadr form) name))
                (setq found form))))
        (end-of-file nil))
      found)))

(describe "consult-zoxide integrations"
  ;; consult-zoxide registers into neither Embark nor consult-dir by itself;
  ;; without these calls its rows lose the removal key
  (it "register once Embark and consult-dir load, not before"
    (let ((after-load-alist nil)
          (load-file-name nil)
          (registered nil))
      (cl-letf (((symbol-function 'consult-zoxide-embark-register)
                 (lambda () (push 'embark registered)))
                ((symbol-function 'consult-zoxide-consult-dir-register)
                 (lambda () (push 'consult-dir registered))))
        (eval (macroexp-progn
               (use-package-body-forms
                (cddr (search-config-tests--use-package 'consult-zoxide))
                :init))
              t)
        (expect registered :to-be nil)
        (dolist (feature '(embark consult-dir))
          (mapc #'funcall (cdr (assq feature after-load-alist))))
        (expect registered :to-have-same-items-as '(embark consult-dir))))))

;;; config-tests.el ends here
