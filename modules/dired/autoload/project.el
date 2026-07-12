;;; modules/dired/autoload/project.el --- project-scoped tree commands -*- lexical-binding: t; -*-
;;; Commentary:
;; Rebuilt without projectile: the treemacs toggle rides
;; `treemacs-add-and-display-current-project-exclusively' (same
;; add-to-workspace + display-exclusively dance doom.d hand-rolled),
;; the dired locator rides project.el.
;;; Code:

;;;###autoload
(defun treemacs-project-toggle ()
  "Toggle a treemacs window scoped to the current project."
  (interactive)
  (require 'treemacs)
  (if (eq (treemacs-current-visibility) 'visible)
      (delete-window (treemacs-get-local-window))
    (treemacs-add-and-display-current-project-exclusively)))

;;;###autoload
(defun dired-jump-find-in-project ()
  "Open dired at the project root, subtree-descended to the current file."
  (interactive)
  (let* ((root (if-let* ((project (project-current)))
                   (project-root project)
                 default-directory))
         (fname buffer-file-name)
         (parts (when fname
                  (split-string (string-replace root "" fname) "/"))))
    (dired root)
    (when parts
      (goto-char (point-min))
      ;; find initial dir or file
      (dired-goto-file (concat root (car parts)))
      (dolist (part parts)
        (let* ((ov (caddr dired-subtree-overlays))  ; last overlay
               (bound (when ov (overlay-end ov))))  ; search within its span
          (search-forward part bound :noerror)
          (dired-subtree-insert))))
    (recenter)))

;;; project.el ends here
