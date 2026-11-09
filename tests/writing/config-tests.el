;;; tests/writing/config-tests.el --- modules/writing/config.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

(load-module-file "modules/writing/autoload/input-method.el")

;; set by the dictionary advice; special here so a `let' contains it
(defvar ispell-alternate-dictionary)

(defun writing-config-tests--forms ()
  "Read every top-level form of the writing module's config, in file order."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "modules/writing/config.el" test-config-root))
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(defun writing-config-tests--find (pred)
  "Return the first top-level config form PRED accepts."
  (cl-find-if pred (writing-config-tests--forms)))

(describe "spelling dictionary advice"
  (it "follows the input method `cycle-input-method' switches to"
    (let* ((block (writing-config-tests--find
                   (lambda (f) (and (eq (car-safe f) 'after!)
                                    (eq (cadr f) 'ispell)))))
           (advice (cl-find-if (lambda (f) (and (eq (car-safe f) 'defadvice!)
                                                (eq (cadr f) 'change-dict-after-toggle-input)))
                               block))
           (input-method-recent nil)
           (cycle-input-method--walk nil)
           (ispell-alternate-dictionary nil)
           dicts)
      (eval advice t)
      (unwind-protect
          (cl-letf (((symbol-function 'ispell-change-dictionary)
                     (lambda (dict &rest _) (push dict dicts))))
            (with-temp-buffer
              (let ((last-command nil))
                (cycle-input-method))
              (expect current-input-method :to-equal "russian-computer")
              (expect dicts :to-equal '("ru_RU"))))
        (dolist (fn '(toggle-input-method set-input-method cycle-input-method))
          (advice-remove fn #'change-dict-after-toggle-input))))))
