;;; modules/git/autoload/bug-reference.el -*- lexical-binding: t; -*-

;;;###autoload
(defun init-bug-reference-mode-settings ()
  "Recognize org/repo#123 references and route them through GitHub."
  (setq
   bug-reference-bug-regexp
   (concat
    "\\b"
    "\\("
    "\\([A-Za-z0-9_.-]+\\)"  ; org
    "/"                      ; slash
    "\\([A-Za-z0-9_.-]+\\)"  ; repo
    "#\\([0-9]+\\)"          ; hash prefixed ticket number
    "\\)"))
  (setq bug-reference-url-format #'bug-reference-url-format-fn))

;;;###autoload
(defun bug-reference-url-format-fn ()
  "GitHub issue URL for the last `bug-reference-bug-regexp' match."
  (let* ((org (match-string-no-properties 2))
         (project (match-string-no-properties 3))
         (ticket (match-string-no-properties 4))
         (org (if (or (null org) (string-blank-p org))
                  bug-reference-default-org
                org)))
    (format "https://github.com/%s/%s/issues/%s" org project ticket)))
