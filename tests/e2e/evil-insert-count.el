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
  ;; extension-less fixture: fundamental-mode, so the first act never
  ;; races jinx compiling its module on a cold machine (.txt would)
  (let* ((file (expand-file-name "insert-count" e2e-work-dir))
         (buf (find-file-noselect file))
         (results '()))
    (cl-flet ((act (label keys want &optional text)
                (let (errs win)
                  (with-current-buffer buf
                    (switch-to-buffer buf)
                    (delete-other-windows)
                    (erase-buffer)
                    (when text (insert text))
                    (evil-force-normal-state)
                    (goto-char (point-min)))
                  (setq win (selected-window))
                  ;; a command signalling mid-macro rings the bell, which
                  ;; aborts the macro, this scenario and the ones after
                  ;; it; swallow the ding, record the culprit instead.
                  ;; `ding' itself is overridden too: it aborts macros
                  ;; before ring-bell-function is consulted, and the
                  ;; recorded backtrace names caller and window in the
                  ;; transcript
                  (cl-letf ((command-error-function
                             (lambda (data _ctx _caller)
                               (push (list this-command (this-command-keys) data)
                                     errs)))
                            ((symbol-function 'ding)
                             (lambda (&rest _)
                               (push (list 'ding this-command
                                           (buffer-name (window-buffer))
                                           (cl-loop for f in (backtrace-frames)
                                                    for fn = (nth 1 f)
                                                    when (symbolp fn) collect fn
                                                    into fns
                                                    finally return (seq-take fns 20)))
                                     errs))))
                    ;; async pop-ups on a cold machine (native-comp
                    ;; warnings, package sub-process logs) can select
                    ;; another window between two macro commands; the next
                    ;; key is then looked up in that buffer's keymaps -
                    ;; suppress-keymap buffers ding on printable keys.
                    ;; Pin the fixture window from post-command-hook: it
                    ;; runs before the next key's lookup, and the single
                    ;; macro call keeps pending prefix counts intact
                    ;; (splitting per key drops them with the command loop)
                    (let ((pin (lambda ()
                                 (unless (window-live-p win)
                                   (setq win (frame-selected-window)))
                                 (unless (eq (selected-window) win)
                                   (select-window win))
                                 (unless (eq (window-buffer win) buf)
                                   (set-window-buffer win buf)))))
                      (unwind-protect
                          (progn
                            (add-hook 'post-command-hook pin)
                            (condition-case e
                                (execute-kbd-macro keys)
                              (error (push (list 'execute-kbd-macro keys e
                                                 (buffer-name (window-buffer)))
                                           errs))))
                        (remove-hook 'post-command-hook pin))))
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
