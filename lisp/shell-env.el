;;; lisp/shell-env.el --- Import the login shell PATH into Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;; Vanilla replacement for Doom's `doom-load-envvars-file'.
;;; Code:

(defvar exec-path-from-shell-arguments)
(declare-function exec-path-from-shell-initialize "exec-path-from-shell")

(defun shell-environment-incomplete-p ()
  "Return non-nil when `rg' is missing, the sign of a truncated inherited PATH."
  (not (executable-find "rg")))

(defun import-shell-environment ()
  "Import the shell PATH when it looks truncated; a no-op once `rg' resolves."
  (when (shell-environment-incomplete-p)
    (require 'exec-path-from-shell)
    (setq exec-path-from-shell-arguments '("-l" "-i"))
    (exec-path-from-shell-initialize)))

(provide 'shell-env)
;;; lisp/shell-env.el ends here