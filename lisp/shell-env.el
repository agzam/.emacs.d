;;; lisp/shell-env.el --- Import the login shell PATH into Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;; Vanilla replacement for Doom's `doom-load-envvars-file'.
;;; Code:

(defvar exec-path-from-shell-arguments)
(declare-function exec-path-from-shell-initialize "exec-path-from-shell")

(defun shell-environment-incomplete-p ()
  "Return non-nil when Emacs did not inherit its PATH from a shell.
A graphical Emacs is started by launchd, which hands it the `path_helper'
default; a terminal Emacs already carries the shell's own PATH."
  (display-graphic-p))

(defun import-shell-environment ()
  "Import the shell PATH unless Emacs already inherited one.
The shell has to be interactive as well as a login one: ~/.zshrc, not
~/.zprofile, is where PATH gets built."
  (when (shell-environment-incomplete-p)
    (require 'exec-path-from-shell)
    (setq exec-path-from-shell-arguments '("-l" "-i"))
    (exec-path-from-shell-initialize)))

(provide 'shell-env)
;;; lisp/shell-env.el ends here