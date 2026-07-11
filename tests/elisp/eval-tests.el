;;; tests/elisp/eval-tests.el --- elisp/autoload/eval.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; smartparens/eros aren't installed in the batch tier; the sexp-motion
;; eval pair is exercised through stubs here, for real in the probe.

(load-module-file "modules/elisp/autoload/eval.el")

(describe "sharp-quote"
  (it "expands # to #' in code"
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((last-command-event ?#))
        (sharp-quote))
      (expect (buffer-string) :to-equal "#'")))

  (it "stays a plain # inside strings"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "\"str\"")
      (backward-char 1)
      (let ((last-command-event ?#))
        (sharp-quote))
      (expect (buffer-string) :to-equal "\"str#\"")))

  (it "does not double up before an existing quote"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "'sym")
      (goto-char (point-min))
      (let ((last-command-event ?#))
        (sharp-quote))
      (expect (buffer-string) :to-equal "#'sym"))))

(describe "eval-current-sexp"
  (it "climbs out of the enclosing sexp and evals through eros"
    (let (climbed evaled)
      (cl-letf (((symbol-function 'sp-up-sexp)
                 (lambda (&rest _) (setq climbed t)))
                ((symbol-function 'eros-eval-last-sexp)
                 (lambda () (interactive) (setq evaled t))))
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "(+ 1 2)")
          (goto-char 3)
          (eval-current-sexp))
        (expect climbed :to-be-truthy)
        (expect evaled :to-be-truthy)))))

(describe "eval-current-with-log"
  (it "pops the *Messages* delta into an *eval* buffer"
    (let (shown)
      (cl-letf (((symbol-function 'eval-current-sexp)
                 (lambda ()
                   (with-current-buffer (messages-buffer)
                     (let ((inhibit-read-only t))
                       (goto-char (point-max))
                       (insert "hello-log-42\n")))))
                ((symbol-function 'switch-to-buffer-other-window)
                 (lambda (buf) (setq shown buf))))
        (unwind-protect
            (progn
              (eval-current-with-log)
              (expect (buffer-name shown) :to-equal "*eval*")
              (expect (with-current-buffer "*eval*" (buffer-string))
                      :to-match "hello-log-42"))
          (when (get-buffer "*eval*") (kill-buffer "*eval*")))))))
