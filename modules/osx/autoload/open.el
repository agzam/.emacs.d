;;; modules/osx/autoload/open.el -*- lexical-binding: t; -*-

;; Doom's +macos-open-with / +macos/open-in-default-program, renamed per
;; the no-plus-prefix rule.  Only the pair the bindings tree uses (SPC f O);
;; the iTerm/Transmit/LaunchBar family stayed behind.

;;;###autoload
(defun macos-open-with (&optional app-name path)
  "Send PATH to APP-NAME on macOS.
Defaults to the current buffer's file (or the file at point in dired)
and the OS default program."
  (interactive)
  (let* ((path (expand-file-name
                (replace-regexp-in-string
                 "'" "\\'"
                 (or path (if (derived-mode-p 'dired-mode)
                              (dired-get-file-for-visit)
                            (buffer-file-name)))
                 nil t)))
         (args (cons "open"
                     (append (if app-name (list "-a" app-name))
                             (list path)))))
    (message "Running: %S" args)
    (apply #'doom-call-process args)))

;;;###autoload
(defun macos-open-in-default-program ()
  "Open the current file in its default macOS program."
  (interactive)
  (macos-open-with))
