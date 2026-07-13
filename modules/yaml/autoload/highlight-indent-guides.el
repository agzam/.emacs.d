;;; modules/yaml/autoload/highlight-indent-guides.el -*- lexical-binding: t; -*-

;;;###autoload
(defun indent-guides-init-faces-h (&rest _)
  "Recompute `highlight-indent-guides' faces from the active theme.
The package derives its guide faces from the theme but cannot in a
256-color terminal, so only refresh on graphical frames (with a daemon,
after the first one is available)."
  (when (display-graphic-p)
    (highlight-indent-guides-auto-set-faces)))

;;;###autoload
(defun indent-guides-disable-maybe-h ()
  "Disable `highlight-indent-guides-mode' where `org-indent-mode' owns indent.
The two draw over the same columns; org-indent wins in org buffers."
  (and highlight-indent-guides-mode
       (bound-and-true-p org-indent-mode)
       (highlight-indent-guides-mode -1)))
