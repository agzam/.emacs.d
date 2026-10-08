;;; tests/general/config-tests.el --- modules/general/config.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)
(require 'paren)

(defvar sp-show-pair-delay)

(defun general-config-tests--top-level-forms ()
  "Read every top-level form out of the general module's config, in file order."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "modules/general/config.el" test-config-root))
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(defun general-config-tests--config-forms (package)
  "The forms under `:config' in PACKAGE's `use-package' block."
  (let ((block (cl-find-if (lambda (f)
                             (and (eq (car-safe f) 'use-package)
                                  (eq (cadr f) package)))
                           (general-config-tests--top-level-forms))))
    (use-package-body-forms (cddr block) :config)))

(describe "paren highlight delays"
  ;; Lisp modes turn show-paren off and show-pair on, so the two delays
  ;; split the buffers between them.
  :var* ((paren-delay (cl-find-if (lambda (f)
                                    (and (eq (car-safe f) 'setq)
                                         (eq (cadr f) 'show-paren-delay)))
                                  (general-config-tests--top-level-forms)))
         (pair-delay (cl-find-if (lambda (f)
                                   (and (eq (car-safe f) 'setopt)
                                        (memq 'sp-show-pair-delay f)))
                                 (general-config-tests--config-forms 'smartparens))))

  (it "sets show-paren-delay at load time, below Doom's 0.1"
    ;; top level, so the value is in place before the first buffer
    ;; (re)enables show-paren-mode and creates its idle timer
    (expect paren-delay :to-be-truthy)
    (expect (numberp (nth 2 paren-delay)) :to-be t)
    (expect (nth 2 paren-delay) :to-be-less-than 0.1))

  (it "gives show-pair in Lisp buffers the same delay"
    (expect pair-delay :to-be-truthy)
    (let ((show-paren-delay show-paren-delay)
          (sp-show-pair-delay 0.125))
      (eval paren-delay t)
      (eval pair-delay t)
      (expect sp-show-pair-delay :to-equal show-paren-delay))))

;;; config-tests.el ends here
