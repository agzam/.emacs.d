;;; custom/general/autoload/files.el -*- lexical-binding: t; -*-

;;;###autoload
(defun insert-file-path (arg)
  "Prompt for a file path and insert it at point.
Without prefix ARG, insert the absolute path.
With prefix ARG, insert path relative to `default-directory'.
Use `consult-dir' (C-x C-d) during the prompt for zoxide lookup."
  (interactive "P")
  (let ((path (read-file-name "Insert path: ")))
    (insert (if arg
                (file-relative-name path)
              (abbreviate-file-name path)))))

(defun current-file-path ()
  "Return the file the current buffer is meant to write to.
Falls back to `default-directory' in dired-like buffers."
  (or (buffer-file-name (buffer-base-buffer))
      (and (derived-mode-p 'dired-mode 'wdired-mode) default-directory)))

;;;###autoload
(defun sudo-file-path (file)
  "Return a TRAMP path that opens FILE as root, preserving any remote hop."
  (let ((host (or (file-remote-p file 'host) "localhost")))
    (concat "/" (when-let* ((method (file-remote-p file 'method)))
                  (concat method ":" (file-remote-p file 'user) "@" host "|"))
            "sudo:root@" host
            ":" (or (file-remote-p file 'localname) file))))

;;;###autoload
(defun sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (find-file (sudo-file-path (expand-file-name file))))

;;;###autoload
(defun sudo-this-file ()
  "Reopen the current file as root."
  (interactive)
  (find-file
   (sudo-file-path
    (or (current-file-path)
        (user-error "Current buffer not bound to a file")))))

;;;###autoload
(defun sudo-save-buffer ()
  "Write the current buffer to its file as root."
  (interactive)
  (unless buffer-file-name (user-error "Current buffer not bound to a file"))
  (write-region nil nil (sudo-file-path buffer-file-name))
  (clear-visited-file-modtime)
  (set-buffer-modified-p nil)
  (message "Saved as root: %s" (abbreviate-file-name buffer-file-name)))
