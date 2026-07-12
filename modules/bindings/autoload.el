;;; modules/bindings/autoload.el -*- lexical-binding: t; -*-
;; Minimal ports of Doom leader-tree helper commands and helpers the SPC tree binds
;; (Doom's config/default module owned these; consult-based lab versions).

;;;###autoload
(defun delete-backward-word (arg)
  "Like `backward-kill-word', but doesn't affect the kill-ring."
  (interactive "p")
  (let ((kill-ring nil) (kill-ring-yank-pointer nil))
    (ignore-errors (backward-kill-word arg))))

(defun search-in-dir (dir &optional initial)
  (require 'consult)
  (consult-ripgrep dir initial))

;;;###autoload
(defun search-project ()
  "Search the current project with ripgrep."
  (interactive)
  (search-in-dir (or (doom-project-root) default-directory)))

;;;###autoload
(defun search-other-project ()
  "Search a known project with ripgrep."
  (interactive)
  (search-in-dir (project-prompt-project-dir)))

;;;###autoload
(defun browse-project ()
  "Find a file from the project root (dired reachable, like find-file)."
  (interactive)
  (let ((default-directory (or (doom-project-root) default-directory)))
    (call-interactively #'find-file)))

;;;###autoload
(defun browse-in-other-project ()
  "Find a file from another known project's root."
  (interactive)
  (let ((default-directory (project-prompt-project-dir)))
    (call-interactively #'find-file)))

;;;###autoload
(defun find-file-in-other-project ()
  "Run `project-find-file' in another known project."
  (interactive)
  (let ((default-directory (project-prompt-project-dir)))
    (call-interactively #'project-find-file)))

;;;###autoload
(defun search-cwd ()
  "Search this directory recursively."
  (interactive)
  (search-in-dir default-directory))

;;;###autoload
(defun search-other-cwd ()
  "Search another directory recursively."
  (interactive)
  (search-in-dir (read-directory-name "Search directory: ")))

;;;###autoload
(defun search-project-for-symbol-at-point (symbol dir)
  "Search project for SYMBOL at point in DIR."
  (interactive
   (list (or (thing-at-point 'symbol t) "")
         (or (doom-project-root) default-directory)))
  (search-in-dir dir symbol))

;;;###autoload
(defun search-notes-for-symbol-at-point (symbol)
  "Search org notes for SYMBOL at point."
  (interactive (list (or (thing-at-point 'symbol t) "")))
  (require 'org)
  (search-in-dir org-directory symbol))

;;;###autoload
(defun search-buffer ()
  "Search the current buffer; with an active region, prefill it."
  (interactive)
  (require 'consult)
  (if (region-active-p)
      (consult-line (buffer-substring-no-properties
                     (region-beginning) (region-end)))
    (consult-line)))

;;;###autoload
(defun search-emacsd ()
  "Search the Emacs config directory."
  (interactive)
  (search-in-dir user-emacs-directory))

;;;###autoload
(defun find-file-under-here ()
  "Recursively find a file under the current directory."
  (interactive)
  (require 'consult)
  (consult-find default-directory))

;;;###autoload
(defun dired-prompt (&optional dir)
  "Open dired in DIR (prompted)."
  (interactive (list (read-directory-name "Open dired in: " default-directory)))
  (dired dir))

;;;###autoload
(defun yank-buffer-path (&optional root)
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
(defun yank-buffer-path-relative-to-project ()
  "Copy the current buffer's project-relative path to the kill ring."
  (interactive)
  (yank-buffer-path (doom-project-root)))
