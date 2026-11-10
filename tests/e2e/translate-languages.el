;;; tests/e2e/translate-languages.el --- language keys of translate-transient -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; s, t and r read and set the languages through transient's suffix
;; objects, which only exist while the real menu is up.

(require 'cl-lib)

(defun translate-languages-e2e ()
  "Switch languages in the menu with real keys, then translate with RET."
  (require 'google-translate)
  ;; load before stubbing: the autoload firing under a stub would redefine it
  (unless (fboundp 'translate--select-source)
    (load (expand-file-name "modules/writing/autoload/google-translate.el" e2e-root)
          nil 'nomessage))
  (let* ((file (expand-file-name "translate-languages.txt" e2e-work-dir))
         (buf (find-file-noselect file))
         (targets '("en" "es" "ru"))
         (google-translate-default-source-language "auto")
         (google-translate-default-target-language "en")
         (results '())
         sent states)
    (cl-labels ((record (label ok got want)
                  (push (list :label (format "translate languages: %s" label)
                              :ok ok :got got :want want)
                        results))
                (note ()
                  (when (bound-and-true-p transient--prefix)
                    (push (cons google-translate-default-source-language
                                google-translate-default-target-language)
                          states)))
                (valid (state)
                  (member (cdr state) (remove (car state) targets)))
                (run (keys)
                  (setq sent nil states nil
                        google-translate-default-source-language "auto"
                        google-translate-default-target-language "en")
                  (switch-to-buffer buf)
                  (delete-other-windows)
                  (with-current-buffer buf (evil-force-normal-state))
                  (discard-input)
                  ;; RET prompts for a missing target; leave the prompt
                  ;; instead of idling into the watchdog
                  (let ((guard (run-with-timer
                                10 nil
                                (lambda ()
                                  (when (active-minibuffer-window)
                                    (abort-recursive-edit))))))
                    (unwind-protect
                        (condition-case e
                            (execute-kbd-macro
                             (vconcat (kbd "SPC x t t") keys (kbd "RET")))
                          ((error quit) (record (format "%s ran" keys) nil e nil)))
                      (cancel-timer guard)
                      (when (bound-and-true-p transient--prefix)
                        (ignore-errors (transient--emergency-exit)))))
                  (setq states (nreverse states))))
      (unwind-protect
          (cl-letf (((symbol-function 'google-translate-translate)
                     (lambda (src tgt &rest _) (setq sent (list src tgt)))))
            (with-current-buffer buf (erase-buffer) (insert "hola mundo"))
            (add-hook 'post-command-hook #'note)

            (run "ssss")
            (record "s walks every source, each with a target other than itself"
                    (and (equal (mapcar #'car (seq-take states 5))
                                '("auto" "en" "es" "ru" "auto"))
                         (seq-every-p #'valid states))
                    states
                    "auto, en, es, ru, auto - each with a target from en/es/ru but itself")
            (record "back on auto, RET translates into the target it kept"
                    (equal sent '("auto" "en"))
                    sent '("auto" "en"))

            (run "sttt")
            (record "t cycles the targets other than the source, never auto"
                    (and (equal (seq-take states 5)
                                '(("auto" . "en") ("en" . "es") ("en" . "ru")
                                  ("en" . "es") ("en" . "ru")))
                         (equal sent '("en" "ru")))
                    (list states sent)
                    "en with es, ru, es, ru; RET sends (\"en\" \"ru\")")

            (run "sr")
            (record "r swaps the source and the target"
                    (equal sent '("es" "en"))
                    sent '("es" "en"))

            (run "r")
            (record "r on an auto source makes the old target the source, paired"
                    (and (equal sent '("en" "es"))
                         (seq-every-p #'valid states))
                    (list states sent)
                    "en with es, no state without a target"))
        (remove-hook 'post-command-hook #'note)
        (discard-input)
        (when (bound-and-true-p transient--prefix)
          (ignore-errors (transient--emergency-exit)))
        (when (buffer-live-p buf)
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))
        (delete-other-windows)))
    (nreverse results)))

(add-to-list 'e2e-scenarios #'translate-languages-e2e)
