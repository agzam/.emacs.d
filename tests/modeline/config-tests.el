;;; tests/modeline/config-tests.el --- modules/modeline/config.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

(defun modeline-config-tests--advice-form (name)
  "Return the `defadvice!' form NAME from the config's doom-modeline block."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "modules/modeline/config.el" test-config-root))
    (goto-char (point-min))
    (let (block)
      (condition-case nil
          (while (not block)
            (let ((f (read (current-buffer))))
              (when (and (eq (car-safe f) 'use-package)
                         (eq (cadr f) 'doom-modeline))
                (setq block f))))
        (end-of-file nil))
      (cl-find-if (lambda (f) (and (eq (car-safe f) 'defadvice!)
                                   (eq (cadr f) name)))
                  (use-package-body-forms (cddr block) :config)))))

;; doom-modeline is absent from the batch tier: the stock segment reduced to
;; the branch that runs when evil holds a method outside insert state.
(defvar-local evil-input-method nil)
(defun doom-modeline-segment--input-method ()
  "Stand-in for doom-modeline's input-method segment."
  (and (bound-and-true-p evil-input-method)
       (nth 3 (assoc default-input-method input-method-alist))))

(eval (modeline-config-tests--advice-form 'doom-modeline-evil-input-method-a) t)

(describe "doom-modeline input-method segment"
  (it "names the method evil holds for the buffer, not the global default"
    (let ((default-input-method "russian-computer"))
      (with-temp-buffer
        (setq-local evil-input-method "spanish-postfix")
        (expect (doom-modeline-segment--input-method)
                :to-equal (nth 3 (assoc "spanish-postfix" input-method-alist)))))))
