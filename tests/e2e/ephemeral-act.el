;;; tests/e2e/ephemeral-act.el --- Embark on a url handed in from outside -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;; Hammerspoon hands the browser's url to `embark-ephemeral-act', which acts
;; on it from a minibuffer; only a real act shows the target found there.

(require 'cl-lib)

(defun ephemeral-act-e2e--press (text keys)
  "Act on TEXT through `embark-ephemeral-act' and press KEYS.
Return the Embark actions run, as (ACTION TYPE TARGET), and the arguments
`consult-hn' got.  The stub keeps the search off the network."
  (let* ((acted nil)
         (searched 'never-called)
         (record (lambda (action target &rest _)
                   (push (list action (plist-get target :type)
                               (plist-get target :target))
                         acted)))
         ;; the act opens its prompt from a timer, so the keys come after it
         (press (run-with-timer 0.5 nil
                                (lambda ()
                                  (setq unread-command-events
                                        (listify-key-sequence (kbd keys))))))
         ;; keys that never leave the prompt would stall the run to the watchdog
         (guard (run-with-timer 10 nil (lambda ()
                                         (when (active-minibuffer-window)
                                           (abort-recursive-edit))))))
    (advice-add 'embark--act :before record)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'consult-hn)
                     (lambda (&rest args) (setq searched args))))
            (condition-case err
                (embark-ephemeral-act text)
              (quit nil)
              (error (push (list 'signalled err) acted)))
            ;; the cleanup reselects a window after the act returns
            (sleep-for 0.3)
            (list (nreverse acted) searched)))
      (cancel-timer press)
      (cancel-timer guard)
      (advice-remove 'embark--act record))))

(defun ephemeral-act-e2e ()
  "Check the target `embark-ephemeral-act' finds in a url, and its b h."
  (require 'embark)
  (let* ((url "https://example.com/x")
         (got (ephemeral-act-e2e--press url "b h"))
         (want `(((hn-discussion url ,url)) (,url :url-match t))))
    (list (list :label "ephemeral act on a url -> HN discussion (b h)"
                :ok (equal got want)
                :got (format "%S" got)
                :want (format "%S" want)))))

(add-to-list 'e2e-scenarios #'ephemeral-act-e2e)
