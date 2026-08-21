;;; tests/e2e/abbrev-contractions.el --- contractions typed at speed -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; The batch suite builds both apostrophe syntax regimes by hand; only a
;; booted config shows which regime each mode actually lands in, whether
;; text-mode-hook turned abbrev-mode on there, and whether the shipped
;; abbrev_defs reached the table.  Real keypresses matter too: the
;; correction rides `self-insert-command' and `post-self-insert-hook',
;; both of which evil's insert state and smartparens also sit on.

(require 'cl-lib)

(defun abbrev-contractions-e2e ()
  "Type contractions with real keys in markdown, text and org buffers."
  (let ((results '()))
    (cl-flet ((act (ext label keys want)
                (let* ((file (expand-file-name (format "contractions.%s" ext)
                                               e2e-work-dir))
                       (buf (find-file-noselect file))
                       errs win)
                  (unwind-protect
                      (progn
                        (with-current-buffer buf
                          (switch-to-buffer buf)
                          (delete-other-windows)
                          (erase-buffer)
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
                                  (condition-case e
                                      (execute-kbd-macro
                                       (vconcat "i" keys [escape]))
                                    (error (push (list 'execute-kbd-macro e) errs))))
                              (remove-hook 'post-command-hook pin))))
                        (let ((got (with-current-buffer buf
                                     (buffer-substring-no-properties
                                      (point-min) (point-max))))
                              (mode (buffer-local-value 'major-mode buf))
                              (on (buffer-local-value 'abbrev-mode buf)))
                          (push (list :label (format "%s (%s, abbrev-mode %s): %s"
                                                     label mode (if on "on" "OFF") keys)
                                      :ok (and (null errs) on (equal got want))
                                      :got got :want want :err (nreverse errs))
                                results)))
                    (when (buffer-live-p buf)
                      (with-current-buffer buf (set-buffer-modified-p nil))
                      (kill-buffer buf))))))
      (unwind-protect
          (dolist (ext '("md" "txt" "org"))
            (act ext "apostrophe typed after the word" "dont' " "don't ")
            (act ext "plain typo" "dont " "don't ")
            (act ext "contraction typed right" "don't " "don't ")
            (act ext "possessive left alone" "dogs' " "dogs' "))
        (evil-force-normal-state)
        (discard-input)))
    (nreverse results)))

(add-to-list 'e2e-scenarios #'abbrev-contractions-e2e)
