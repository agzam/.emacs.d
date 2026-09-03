;;; modules/dired/autoload/subtree.el --- subtree nav + item-opening helpers -*- lexical-binding: t; -*-
;;; Commentary:
;; dired-subtree navigation extensions and open-item-in-window actions.
;; buffer-with-dired-item lost its direx branch on port (direx was never in
;; packages.el - dead code).  doom.d's dired-subtree-remove* is
;; dired-remove-subtree here (distinct name - the package owns
;; dired-subtree-remove).
;;; Code:

;;;###autoload
(defun dired-remove-subtree ()
  "Remove the subtree at point, also when point sits on its root line."
  (interactive)
  (when (dired-subtree--is-expanded-p)
    (dired-next-line 1))
  (dired-subtree-remove))

;;;###autoload
(defun dired-subtree-down-n-open ()
  "Insert the subtree at point and step into it."
  (interactive)
  (save-excursion (dired-subtree-insert))
  (when (or (dired-subtree--is-expanded-p)
            (not (eq (point)
                     (save-excursion (dired-subtree-end) (point)))))
    (dired-next-line 1)))

(defun buffer-with-dired-item ()
  "Buffer visiting the dired item at point."
  (when (eq major-mode 'dired-mode)
    (find-file-noselect (dired-get-file-for-visit))))

;;;###autoload
(defun dired-open-item-in-split (split-fn)
  "Show the dired item at point in the window SPLIT-FN creates."
  (let ((buf (buffer-with-dired-item)))
    (funcall split-fn)
    (switch-to-buffer buf)))

;;;###autoload
(defun dired-ace-action ()
  "Show the dired item at point in a window picked via ace-window."
  (interactive)
  (with-demoted-errors "%s"
    (require 'ace-window)
    (let ((buf (buffer-with-dired-item))
          (aw-dispatch-always t))
      (aw-switch-to-window (aw-select nil))
      (switch-to-buffer buf))))

;;;###autoload
(defun treemacs-icons-after-subtree-insert-a ()
  "Give the lines of a fresh dired-subtree insert treemacs icons."
  (let ((end (overlay-end (dired-subtree--get-ov))))
    (treemacs-with-writable-buffer
     (save-excursion
       (goto-char (point))
       (dired-goto-next-file)
       (while (< (point) end)
         (when (dired-move-to-filename nil)
           (let* ((file (dired-get-filename nil t))
                  (icon (if (file-directory-p file)
                            treemacs-icon-dir-closed
                          (treemacs-icon-for-file file)))
                  ;; skip lines that already carry an icon
                  (icon? (save-excursion
                           (goto-char (line-end-position))
                           (re-search-backward "  \\s-*" (line-beginning-position) t)
                           (get-text-property (point) 'display))))
             (unless icon? (insert icon))))
         (forward-line 1))
       (set-buffer-modified-p nil)))))

;;; subtree.el ends here
