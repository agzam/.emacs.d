;;; modules/bindings/autoload.el -*- lexical-binding: t; -*-
;; Minimal ports of the +default/* commands and helpers the SPC tree binds
;; (Doom's config/default module owned these; consult-based lab versions).

;;;###autoload
(defun doom/delete-backward-word (arg)
  "Like `backward-kill-word', but doesn't affect the kill-ring."
  (interactive "p")
  (let ((kill-ring nil) (kill-ring-yank-pointer nil))
    (ignore-errors (backward-kill-word arg))))

(defun +default--search-dir (dir &optional initial)
  (require 'consult)
  (consult-ripgrep dir initial))

;;;###autoload
(defun +default/search-project ()
  "Search the current project with ripgrep."
  (interactive)
  (+default--search-dir (or (doom-project-root) default-directory)))

;;;###autoload
(defun +default/search-cwd ()
  "Search this directory recursively."
  (interactive)
  (+default--search-dir default-directory))

;;;###autoload
(defun +default/search-other-cwd ()
  "Search another directory recursively."
  (interactive)
  (+default--search-dir (read-directory-name "Search directory: ")))

;;;###autoload
(defun +default/search-project-for-symbol-at-point (symbol dir)
  "Search project for SYMBOL at point in DIR."
  (interactive
   (list (or (thing-at-point 'symbol t) "")
         (or (doom-project-root) default-directory)))
  (+default--search-dir dir symbol))

;;;###autoload
(defun +default/search-buffer ()
  "Search the current buffer; with an active region, prefill it."
  (interactive)
  (require 'consult)
  (if (region-active-p)
      (consult-line (buffer-substring-no-properties
                     (region-beginning) (region-end)))
    (consult-line)))

;;;###autoload
(defun +default/search-emacsd ()
  "Search the Emacs config directory."
  (interactive)
  (+default--search-dir user-emacs-directory))

;;;###autoload
(defun +default/find-file-under-here ()
  "Recursively find a file under the current directory."
  (interactive)
  (require 'consult)
  (consult-find default-directory))

;;;###autoload
(defun +default/dired (&optional dir)
  "Open dired in DIR (prompted)."
  (interactive (list (read-directory-name "Open dired in: " default-directory)))
  (dired dir))

;;;###autoload
(defun +default/yank-buffer-path (&optional root)
  "Copy the current buffer's path (relative to ROOT) to the kill ring."
  (interactive)
  (if-let* ((filename (or (buffer-file-name (buffer-base-buffer))
                          (bound-and-true-p list-buffers-directory))))
      (let ((path (abbreviate-file-name
                   (if root (file-relative-name filename root) filename))))
        (kill-new path)
        (message "Copied path: %s" path))
    (user-error "Buffer isn't visiting a file")))

;;;###autoload
(defun +default/yank-buffer-path-relative-to-project ()
  "Copy the current buffer's project-relative path to the kill ring."
  (interactive)
  (+default/yank-buffer-path (doom-project-root)))
