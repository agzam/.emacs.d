;;; modules/shell/autoload/shell.el --- shell-pop helpers -*- lexical-binding: t; -*-

(require 'subr-x)

;;;###autoload
(defun insert-current-filename ()
  "Insert the selected window's file or dired entry into the minibuffer."
  (interactive)
  (when (eq major-mode 'minibuffer-mode)
    (let ((fname (with-current-buffer
                     (window-buffer (minibuffer-selected-window))
                   (pcase major-mode
                     ('dired-mode
                      (dired-get-filename))
                     (_ buffer-file-name)))))
      (insert fname))))

;;;###autoload
(defun shell-pop-choose (&optional arg)
  "Pick a shell implementation, rewire `shell-pop-shell-type', then pop."
  (interactive "P")
  (let* ((shell-type (completing-read "Shell: " '(eshell ghostel shell)))
         (shell-fn (pcase shell-type
                     ("eshell" #'eshell)
                     ("ghostel" #'ghostel)
                     ("shell" #'shell))))
    (shell-pop--set-shell-type
     'shell-pop-shell-type
     `(,shell-type
       ,(format "*%s*" shell-type)
       (lambda () (,shell-fn))))
    (shell-pop arg)))

;;;###autoload
(defun shell-pop-in-project-root (&optional arg)
  "`shell-pop' with `default-directory' at the project root, if any."
  (interactive)
  (let ((default-directory (if-let* ((pr (project-current)))
                               (project-root pr)
                             default-directory)))
    (shell-pop arg)))
