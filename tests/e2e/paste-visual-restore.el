;;; tests/e2e/paste-visual-restore.el --- gv after a paste -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; paste-sets-visual-selection-a feeds the paste bounds to the three
;; variables `evil-visual-restore' reads.  The batch spec drives that
;; advice through stubs; only real keypresses show whether `p' and `gv'
;; agree in a booted config, where other advice wraps the same paste
;; commands (prisma's format conversion, eca's image yank) and where
;; entering visual state opens expreg-transient (evil-select-block-a).
;; Inverting case through `~' reads the selection's extent back out of
;; the buffer: it is an expreg-transient bypass key, so it survives that
;; transient, and outside visual state it changes one character only.

(require 'cl-lib)

(defun paste-visual-restore-e2e ()
  "Press p then gv with real keys, and read the selection back through ~."
  ;; every case pastes from a register: cut the system clipboard out, or
  ;; whatever the machine holds would land in the buffer instead
  (let ((results '())
        (interprogram-paste-function nil)
        (interprogram-cut-function nil))
    (cl-flet ((act (label text macros want)
                ;; extension-less fixture: fundamental-mode, so no mode
                ;; hook races a compile on a cold machine
                (let* ((file (expand-file-name "paste-visual-restore" e2e-work-dir))
                       (buf (find-file-noselect file))
                       errs win)
                  (unwind-protect
                      (progn
                        (with-current-buffer buf
                          (switch-to-buffer buf)
                          (delete-other-windows)
                          (erase-buffer)
                          (insert text)
                          (evil-force-normal-state)
                          (goto-char (point-min)))
                        (setq win (selected-window))
                        (discard-input)
                        (cl-letf ((command-error-function
                                   (lambda (data _ctx _caller)
                                     (push (list this-command (this-command-keys) data)
                                           errs)))
                                  ((symbol-function 'ding)
                                   (lambda (&rest _)
                                     (push (list 'ding this-command) errs))))
                          ;; an async pop-up between two macro keys sends the
                          ;; rest of them to another buffer; pin the window
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
                                  ;; catch t, not error: a macro aborted by
                                  ;; quit would unwind past the harness and
                                  ;; take the whole run's results with it
                                  (dolist (keys macros)
                                    (condition-case e
                                        (execute-kbd-macro keys)
                                      (t (push (list 'execute-kbd-macro keys e)
                                               errs)))))
                              (remove-hook 'post-command-hook pin))))
                        (let ((got (with-current-buffer buf
                                     (buffer-substring-no-properties
                                      (point-min) (point-max)))))
                          (push (list :label (format "paste + gv: %s" label)
                                      :ok (and (null errs) (equal got want))
                                      :got got :want want :err (nreverse errs))
                                results)))
                    (when (buffer-live-p buf)
                      (with-current-buffer buf (set-buffer-modified-p nil))
                      (kill-buffer buf))))))
      (unwind-protect
          (progn
            (act "gv selects a charwise paste"
                 "alpha beta gamma\n"
                 (list (vconcat "yiw$pgv~"))
                 "alpha beta gammaALPHA\n")
            (act "gv selects a linewise paste"
                 "alpha\nbeta\n"
                 (list (vconcat "yyjpgv~"))
                 "alpha\nbeta\nALPHA\n")
            ;; s-v is the system paste key, and it runs plain `yank' -
            ;; the path Evil's change markers know nothing about
            (act "gv selects what the system paste key inserted"
                 "alpha beta gamma\n"
                 (list (vconcat "yiw" (kbd "s-v") "gv~"))
                 "ALPHAalpha beta gamma\n")
            ;; ESC ends its own macro: a tty reads ESC and the key after
            ;; it as that key with Meta, so mid-sequence it never reaches
            ;; evil.  Two of them: the first closes expreg-transient, the
            ;; second leaves visual state.
            (act "gv still restores a selection made by hand"
                 "alpha beta gamma\n"
                 (list (vconcat "vll") (vector 'escape) (vector 'escape)
                       (vconcat "gv~"))
                 "ALPha beta gamma\n"))
        ;; a failed act can strand the transient, and it would swallow the
        ;; keys of every scenario after this one
        (when (bound-and-true-p transient--prefix)
          (ignore-errors
            (if (fboundp 'transient--emergency-exit)
                (transient--emergency-exit)
              (transient-quit-all))))
        (evil-force-normal-state)
        (deactivate-mark)
        (discard-input)))
    (nreverse results)))

(add-to-list 'e2e-scenarios #'paste-visual-restore-e2e)
