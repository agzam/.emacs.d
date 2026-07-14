;;; modules/org/autoload/org-export.el -*- lexical-binding: t; -*-
;; ox-clip consumer restored from doom.d org/autoload/org-export.el (the +org/
;; names dropped).  Mode dispatch is stub-tested; the shell/clipboard side is
;; smoke-only.

(defun org--yank-html-buffer (buffer)
  "Copy BUFFER's HTML into the system clipboard as rich text."
  (with-current-buffer buffer
    (require 'ox-clip)
    (cond ((featurep :system 'macos)
           (shell-command-on-region
            (point-min) (point-max) ox-clip-osx-cmd))
          ((featurep :system 'linux)
           (let ((html (buffer-string)))
             (with-temp-file (make-temp-file "ox-clip-md" nil ".html")
               (insert html))
             (apply #'start-process "ox-clip" "*ox-clip*"
                    (split-string ox-clip-linux-cmd " ")))))))

;;;###autoload
(defun org-export-to-clipboard-as-rich-text (beg end)
  "Export region BEG..END to HTML and copy it to the clipboard as rich text.
Handles `org-mode', `markdown-mode' and `gfm-mode'; any other mode goes
through htmlize via `ox-clip-formatted-copy'."
  (interactive "r")
  (pcase major-mode
    ((or 'markdown-mode 'gfm-mode)
     (org--yank-html-buffer (markdown)))
    (_
     ;; Omit before/after-string overlays so htmlize doesn't emit fringe
     ;; chars for flycheck/git-gutter.
     (letf! (defun htmlize-add-before-after-strings (_beg _end text) text)
       (ox-clip-formatted-copy beg end)))))
