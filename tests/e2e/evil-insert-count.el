;;; tests/e2e/evil-insert-count.el --- stray count before insert -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; drop-insert-repeat-count-a guards insert-state exit: a digit typed
;; before `i'/`a'/`o' arms `evil-insert-count', and leaving insert state
;; then replays the whole insertion.  The digit, the state switch and
;; the exit cleanup only meet when real keypresses travel evil's state
;; maps, so this wiring lives in the e2e tier: a stray count must not
;; multiply the insertion, while operator counts and dot-repeat keep
;; their meaning.  Visual state belongs to expreg-transient, so the
;; block-insert act crosses it: C-v opens the transient, j stays inside
;; via its bypass, I leaves it for evil's block insert (whose vcount
;; fan-out the advice must leave alone).

(require 'cl-lib)

(defun evil-insert-count-e2e ()
  "Stray counts before insert commands must not multiply the insertion."
  (let* ((file (expand-file-name "insert-count.txt" e2e-work-dir))
         (buf (find-file-noselect file))
         (results '()))
    (cl-flet ((act (label keys want &optional text)
                (let (errs)
                  (with-current-buffer buf
                    (switch-to-buffer buf)
                    (delete-other-windows)
                    (erase-buffer)
                    (when text (insert text))
                    (evil-force-normal-state)
                    (goto-char (point-min)))
                  ;; a command signalling mid-macro rings the bell, which
                  ;; aborts the macro, this scenario and the ones after
                  ;; it; swallow the ding, record the culprit instead
                  (let ((command-error-function
                         (lambda (data _ctx _caller)
                           (push (list this-command (this-command-keys) data)
                                 errs))))
                    (condition-case e
                        (execute-kbd-macro keys)
                      (error (push (list 'execute-kbd-macro keys e) errs))))
                  (let ((got (with-current-buffer buf
                               (buffer-substring-no-properties
                                (point-min) (point-max)))))
                    (push (list :label (format "insert count: %s" label)
                                :ok (and (null errs) (equal got want))
                                :got got :want want :err (nreverse errs))
                          results)))))
      (unwind-protect
          (progn
            (act "3i inserts once" (vconcat "3iab" [escape]) "ab")
            (act "3o opens one line" (vconcat "3ox" [escape]) "\nx")
            (act "dot-repeat still repeats" (vconcat "iab" [escape] ".") "aabb")
            (act "operator count still counts" "3x" "def" "abcdef")
            (act "visual-block insert fans out through expreg-transient"
                 (vconcat "\C-vjjIx" [escape]) "xabc\nxabc\nxabc"
                 "abc\nabc\nabc"))
        ;; leave nothing for the next scenario: no half-armed region, no
        ;; pending input, no transient stranded by a failed act
        (when (bound-and-true-p transient--prefix)
          (ignore-errors
            (if (fboundp 'transient--emergency-exit)
                (transient--emergency-exit)
              (transient-quit-all))))
        (deactivate-mark)
        (discard-input)
        (when (buffer-live-p buf)
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))))
    (nreverse results)))

(add-to-list 'e2e-scenarios #'evil-insert-count-e2e)
