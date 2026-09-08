;;; tests/completion/config-tests.el --- modules/completion/config.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

(defun completion-config-tests--top-level-forms ()
  "Read every top-level form out of the completion module's config, in file order."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "modules/completion/config.el" test-config-root))
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(defun completion-config-tests--consult-settings ()
  "Plist of everything the consult `use-package' form sets under `:config'."
  (let ((consult (cl-find-if (lambda (f)
                               (and (eq (car-safe f) 'use-package)
                                    (eq (cadr f) 'consult)))
                             (completion-config-tests--top-level-forms)))
        settings)
    (dolist (form (use-package-body-forms (cddr consult) :config) settings)
      (when (eq (car-safe form) 'setopt)
        (setq settings (append settings (cdr form)))))))

(describe "consult async delays"
  ;; The delays gate how soon rg/fd spawn and how often results reach the UI.
  ;; Consult ships 0.2/0.5/0.2, which idles far longer than the search costs.
  :var* ((settings (completion-config-tests--consult-settings))
         (delay (lambda (var) (plist-get settings var))))

  (dolist (var '(consult-async-input-debounce
                 consult-async-input-throttle
                 consult-async-refresh-delay))
    (it (format "sets %s under the shipped default, and above zero" var)
      (let ((value (funcall delay var)))
        (expect value :to-be-truthy)
        (expect (numberp value) :to-be t)
        ;; zero refresh delay redisplays on every arriving chunk
        (expect value :to-be-greater-than 0)
        (expect value :to-be-less-than 0.2))))

  (it "debounces no longer than it throttles, so the throttle still caps spawns"
    (expect (funcall delay 'consult-async-input-debounce)
            :to-be-less-than (funcall delay 'consult-async-input-throttle))))

;;; config-tests.el ends here
