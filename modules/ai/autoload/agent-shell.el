;;; modules/ai/autoload/agent-shell.el -*- lexical-binding: t; -*-

;; agent-shell calls in here only once loaded; the test tier has no agent-shell.
(declare-function agent-shell-cwd "agent-shell-project")
(declare-function agent-shell-ui-toggle-all-fragments "agent-shell-ui")
(defvar agent-shell-ui--fold-toggle-state)

;;;###autoload
(defun agent-shell-data-subdir (subdir)
  "Return SUBDIR of the current shell's data directory under `doom-data-dir'.
Upstream puts transcripts, screenshots and worktrees in the project tree."
  (file-name-concat doom-data-dir "agent-shell"
                    (thread-last (agent-shell-cwd)
                                 directory-file-name
                                 (string-remove-prefix "/")
                                 (string-replace "/" "-"))
                    subdir))

;; `agent-shell-ui-toggle-all-fragments' flips the state it finds here, so
;; setting it first makes the toggle go one way.
;;;###autoload
(defun agent-shell-fold-all ()
  "Collapse every section of the shell, as \\`zm' does in an eca chat."
  (interactive)
  (setq agent-shell-ui--fold-toggle-state 'expanded)
  (agent-shell-ui-toggle-all-fragments))

;;;###autoload
(defun agent-shell-reveal-all ()
  "Expand every section of the shell, as \\`zr' does in an eca chat."
  (interactive)
  (setq agent-shell-ui--fold-toggle-state 'collapsed)
  (agent-shell-ui-toggle-all-fragments))
