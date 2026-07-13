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
(defun restart-server ()
  "Restart the Emacs server (renamed +default/restart-server port)."
  (interactive)
  (require 'server)
  (server-force-delete)
  (while (server-running-p)
    (sleep-for 1))
  (server-start))

;;; Text commands vendored from Doom (lisp/lib/text.el +
;;; config/default/autoload/text.el) - the global-key sweep found these
;;; bound in Doom but absent here.  Deviation: comment detection uses
;;; plain (nth 4 (syntax-ppss)) instead of Doom's doom-point-in-comment-p
;;; chain (40 lines of comment-opener edge cases, not worth vendoring).

(defun bol-bot-eot-eol (&optional pos)
  "Return (BOL BOT EOT EOL) for the line at POS.
BOT is the first non-blank char, EOT the end of the last non-blank,
non-comment char."
  (save-excursion
    (when pos (goto-char pos))
    (let* ((bol (if visual-line-mode
                    (save-excursion (beginning-of-visual-line) (point))
                  (line-beginning-position)))
           (bot (save-excursion
                  (goto-char bol)
                  (skip-chars-forward " \t\r")
                  (point)))
           (eol (if visual-line-mode
                    (save-excursion (end-of-visual-line) (point))
                  (line-end-position)))
           (eot (or (save-excursion
                      (if (not comment-use-syntax)
                          (progn
                            (goto-char bol)
                            (when (re-search-forward comment-start-skip eol t)
                              (or (match-end 1) (match-beginning 0))))
                        (goto-char eol)
                        (while (and (nth 4 (syntax-ppss))
                                    (> (point) bol))
                          (backward-char))
                        (skip-chars-backward " " bol)
                        (or (eq (char-after) 32) (eolp) (bolp)
                            (forward-char))
                        (point)))
                    eol)))
      (list bol bot eot eol))))

(defvar backward-to-bol--last-pt nil)
;;;###autoload
(defun backward-to-bol-or-indent (&optional point)
  "Jump between the indentation column and the beginning of the line."
  (interactive "^d")
  (let ((pt (or point (point))))
    (cl-destructuring-bind (bol bot _eot _eol) (bol-bot-eot-eol pt)
      (cond ((> pt bot)
             (goto-char bot))
            ((= pt bol)
             (or (and backward-to-bol--last-pt
                      (= (line-number-at-pos backward-to-bol--last-pt)
                         (line-number-at-pos pt)))
                 (setq backward-to-bol--last-pt nil))
             (goto-char (or backward-to-bol--last-pt bot))
             (setq backward-to-bol--last-pt nil))
            ((<= pt bot)
             (setq backward-to-bol--last-pt pt)
             (goto-char bol))))))

(defvar forward-to-eol--last-pt nil)
;;;###autoload
(defun forward-to-last-non-comment-or-eol (&optional point)
  "Jump between the last non-blank, non-comment char and the end of line.
Doom rot fixed on port: the original stored the bounce-back position in
the BACKWARD mover's variable, so returning from eol never worked."
  (interactive "^d")
  (let ((pt (or point (point))))
    (cl-destructuring-bind (_bol _bot eot eol) (bol-bot-eot-eol pt)
      (cond ((< pt eot)
             (goto-char eot))
            ((= pt eol)
             (goto-char (or forward-to-eol--last-pt eot))
             (setq forward-to-eol--last-pt nil))
            ((>= pt eot)
             (setq forward-to-eol--last-pt pt)
             (goto-char eol))))))

;;;###autoload
(defun backward-kill-to-bol-and-indent ()
  "Kill line to the first non-blank character."
  (interactive)
  (let ((empty-line-p (save-excursion (beginning-of-line)
                                      (looking-at-p "[ \t]*$"))))
    (funcall (if (fboundp 'evil-delete) #'evil-delete #'delete-region)
             (line-beginning-position) (point))
    (unless empty-line-p
      (indent-according-to-mode))))

;;;###autoload
(defun newline-below ()
  "Insert an indented new line after the current one."
  (interactive)
  (if (and (featurep 'evil) evil-local-mode)
      (call-interactively #'evil-open-below)
    (end-of-line)
    (newline-and-indent)))

;;;###autoload
(defun newline-above ()
  "Insert an indented new line before the current one."
  (interactive)
  (if (and (featurep 'evil) evil-local-mode)
      (call-interactively #'evil-open-above)
    (beginning-of-line)
    (save-excursion (newline))
    (indent-according-to-mode)))

;;;###autoload
(defun comment-current-line ()
  "Comment or uncomment the current line, keeping point in place."
  (interactive)
  (save-excursion (comment-line 1)))

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

;;;###autoload
(defun delete-trailing-newlines ()
  "Trim trailing newlines.
Respects `require-final-newline'.  (Doom's doom/delete-trailing-newlines;
the SPC c W row bound it void since the vendoring.)"
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (delete-blank-lines)))
