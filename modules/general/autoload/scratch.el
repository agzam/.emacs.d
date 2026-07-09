;;; modules/general/autoload/scratch.el -*- lexical-binding: t; -*-
;; Persistent scratch buffers, ported from Doom's lisp/lib/scratch.el @8e4fbba
;; (MIT).  Renames: doom/* and doom-scratch-* -> plain scratch-*;
;; doom-scratch-buffer -> scratch-buffer-create (built-in `scratch-buffer'
;; collision); buffers are *scratch+* / *scratch+ (PROJECT)*.  Deviations:
;; projectile shim dropped (project.el), persistence under doom-data-dir,
;; upstream defvar'd doom-scratch-buffer-hook but ran ...-created-hook - only
;; the created-hook is kept.  Unported (nothing referenced them):
;; open-project-scratch-buffer, revert/delete commands, kill-on-hide hook.

(require 'cl-lib)

(defvar so-long--inhibited)

(defvar scratch-default-file "__default"
  "The file name for the project-less scratch buffer, saved in `scratch-dir'.")

(defvar scratch-dir (file-name-concat doom-data-dir "scratch/")
  "Where to save persistent scratch buffers.")

(defvar scratch-initial-major-mode nil
  "What major mode to start fresh scratch buffers in.
t inherits the current buffer's mode, nil means `fundamental-mode', any other
symbol is the mode itself.  Restored scratches keep their persisted mode.")

(defvar scratch-buffers nil
  "A list of active scratch buffers.")

(defvar scratch-current-project nil
  "The name of the project associated with the current scratch buffer.")
(put 'scratch-current-project 'permanent-local t)

(defvar scratch-buffer-created-hook ()
  "Hooks run after a scratch buffer is created.")

(defun scratch--initial-mode ()
  "Initial major mode for a new scratch buffer, per `scratch-initial-major-mode'."
  (cond ((eq scratch-initial-major-mode t)
         (unless (or buffer-read-only
                     (derived-mode-p 'special-mode)
                     (string-match-p "^ ?\\*" (buffer-name)))
           major-mode))
        ((null scratch-initial-major-mode) nil)
        ((symbolp scratch-initial-major-mode) scratch-initial-major-mode)))

(defun scratch--project-name ()
  "Current project name via project.el, or nil."
  (when-let* ((project (project-current)))
    (project-name project)))

(defun scratch--load-persisted (project-name)
  "Restore the persisted scratch of PROJECT-NAME into the current buffer."
  (setq-local scratch-current-project (or project-name scratch-default-file))
  (let ((file (expand-file-name (concat scratch-current-project ".el")
                                scratch-dir)))
    (make-directory scratch-dir t)
    (when (file-readable-p file)
      (cl-destructuring-bind (content point mode)
          (with-temp-buffer
            (save-excursion (insert-file-contents file))
            (read (current-buffer)))
        (erase-buffer)
        (funcall mode)
        (insert content)
        (goto-char point)
        t))))

;;;###autoload
(defun scratch-buffer-create (&optional dont-restore-p mode directory project-name)
  "Return a scratchpad buffer in major MODE.
Restores the persisted state unless DONT-RESTORE-P.  DIRECTORY becomes the
buffer's `default-directory'; PROJECT-NAME namespaces buffer and file."
  (let* ((buffer-name (if project-name
                          (format "*scratch+ (%s)*" project-name)
                        "*scratch+*"))
         (buffer (get-buffer buffer-name)))
    (with-current-buffer
        (or buffer (get-buffer-create buffer-name))
      (setq default-directory directory)
      (setq-local so-long--inhibited t)
      (if dont-restore-p
          (erase-buffer)
        (unless buffer
          (scratch--load-persisted project-name)
          (when (and (eq major-mode 'fundamental-mode)
                     (functionp mode))
            (funcall mode))))
      (cl-pushnew (current-buffer) scratch-buffers)
      (add-transient-hook! 'doom-switch-buffer-hook (persist-scratch-buffers-h))
      (add-transient-hook! 'doom-switch-window-hook (persist-scratch-buffers-h))
      (add-hook 'kill-buffer-hook #'persist-scratch-buffer-h nil 'local)
      (run-hooks 'scratch-buffer-created-hook)
      (current-buffer))))

(defun persist-scratch-buffer-h ()
  "Save the current scratch buffer to `scratch-dir'."
  (let ((content (buffer-substring-no-properties (point-min) (point-max)))
        (point (point))
        (mode major-mode))
    (make-directory scratch-dir t)
    (with-temp-file
        (expand-file-name (concat (or scratch-current-project
                                      scratch-default-file)
                                  ".el")
                          scratch-dir)
      (prin1 (list content point mode) (current-buffer)))))

(defun persist-scratch-buffers-h ()
  "Save all scratch buffers to `scratch-dir'."
  (setq scratch-buffers (cl-delete-if-not #'buffer-live-p scratch-buffers))
  (dolist (buffer scratch-buffers)
    (with-current-buffer buffer
      (persist-scratch-buffer-h))))

(unless noninteractive
  (add-hook 'kill-emacs-hook #'persist-scratch-buffers-h))

;;;###autoload
(defun open-scratch-buffer (&optional arg project-p same-window-p)
  "Pop up the persistent scratch buffer.
With prefix ARG, don't restore its last state.  PROJECT-P selects the
current project's scratch; SAME-WINDOW-P switches instead of popping."
  (interactive "P")
  (funcall (if same-window-p #'switch-to-buffer #'pop-to-buffer)
           (scratch-buffer-create
            arg
            (scratch--initial-mode)
            default-directory
            (when project-p (scratch--project-name)))))

;;;###autoload
(defun switch-to-scratch-buffer (&optional arg project-p)
  "Like `open-scratch-buffer', but in the current window.
With prefix ARG, don't restore its last state."
  (interactive "P")
  (open-scratch-buffer arg project-p 'same-window))

;;;###autoload
(defun switch-to-project-scratch-buffer (&optional arg)
  "Switch to the current project's scratch buffer.
With prefix ARG, don't restore its last state."
  (interactive "P")
  (switch-to-scratch-buffer arg 'project))

;;;###autoload
(defun toggle-scratch-buffer (&optional arg project-p same-window-p)
  "Toggle the persistent scratch buffer: close its window if visible, else open.
With prefix ARG, don't restore its last state.  PROJECT-P selects the
current project's scratch; SAME-WINDOW-P switches instead of popping."
  (interactive "P")
  (let* ((project-name (when project-p (scratch--project-name)))
         (buf (scratch-buffer-create arg (scratch--initial-mode)
                                     default-directory project-name))
         (win (get-buffer-window buf)))
    (if win
        (delete-window win)
      (funcall (if same-window-p #'switch-to-buffer #'pop-to-buffer) buf))))

;;;###autoload
(defun toggle-project-scratch-buffer (&optional arg same-window-p)
  "Toggle the current project's scratch buffer.
With prefix ARG, don't restore its last state."
  (interactive "P")
  (toggle-scratch-buffer arg 'project same-window-p))
