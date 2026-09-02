;;; lisp/shell-env.el --- Import the login shell PATH into Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;; Vanilla replacement for Doom's `doom-load-envvars-file'.  The shell writes
;; its variables into a cache file: a launch that has one applies it at once
;; and lets the shell rewrite it in the background; only a launch without
;; one waits for the shell.
;;; Code:

(defvar shell-environment-variables '("PATH" "MANPATH")
  "Variables imported from the shell, in cache-file order.")

(defvar shell-environment-arguments '("-l" "-i")
  "Shell flags: interactive as well as login, since ~/.zshrc, not
~/.zprofile, is where PATH gets built.")

(defvar shell-environment-cache-file
  (expand-file-name "shell-env" doom-state-dir)
  "Where the shell writes its variables, NUL-separated.")

(defun shell-environment-incomplete-p ()
  "Return non-nil when Emacs did not inherit its PATH from a shell.
A graphical Emacs is started by launchd, which hands it the `path_helper'
default; a terminal Emacs already carries the shell's own PATH."
  (display-graphic-p))

(defun shell-environment-command ()
  "The shell invocation writing `shell-environment-variables' to the cache."
  `(,shell-file-name ,@shell-environment-arguments "-c"
    ,(format "printf '%s' %s > %s"
             (mapconcat (lambda (_) "%s\\000") shell-environment-variables "")
             (mapconcat (lambda (name) (format "\"$%s\"" name))
                        shell-environment-variables " ")
             (shell-quote-argument shell-environment-cache-file))))

(defun shell-environment-start (sentinel)
  "Start the shell writing the cache; SENTINEL runs when it exits.
The shell starts from the PATH launchd hands a graphical Emacs: rc files
prepend to what they inherit, so a shell started from an earlier import
would hand it back with the prefix duplicated."
  (let ((process-environment
         (append '("PATH=/usr/bin:/bin:/usr/sbin:/sbin" "MANPATH") process-environment)))
    (make-process :name "shell-env" :command (shell-environment-command)
                  :noquery t :sentinel sentinel)))

(defun shell-environment-apply-cache ()
  "Set the variables from the cache, `exec-path' along with PATH."
  (with-temp-buffer
    (insert-file-contents shell-environment-cache-file)
    (seq-mapn (lambda (name value)
                (setenv name value)
                (when (equal name "PATH")
                  (setq exec-path (append (parse-colon-path value)
                                          (list exec-directory)))))
              shell-environment-variables
              (split-string (buffer-string) "\0"))))

(defun import-shell-environment ()
  "Import the shell's variables unless Emacs already inherited them."
  (when (shell-environment-incomplete-p)
    (make-directory (file-name-directory shell-environment-cache-file) t)
    (if (file-exists-p shell-environment-cache-file)
        (progn
          (shell-environment-apply-cache)
          (shell-environment-start
           (lambda (process _event)
             (when (and (eq (process-status process) 'exit)
                        (zerop (process-exit-status process)))
               (shell-environment-apply-cache)))))
      (let ((process (shell-environment-start #'ignore)))
        (while (process-live-p process)
          (accept-process-output process 1))
        (shell-environment-apply-cache)))))

(provide 'shell-env)
;;; lisp/shell-env.el ends here
