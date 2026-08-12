;;; scripts/check-root.el --- flag entries that leaked into the config root -*- lexical-binding: t; -*-
;; Usage: emacs -Q --batch -l scripts/check-root.el --eval '(check-root-main "DIR")'

;; Neither -Q nor --batch relocates `user-emacs-directory' - they only skip
;; loading init files - so any Emacs run without --init-directory resolves
;; every unset default (eln cache, auto-save-list, transient history,
;; package-user-dir) against the real config dir and writes there.  This is
;; the tripwire: the tracked tree plus a short sanctioned list is the whole
;; of what may sit at the root.

(require 'seq)

(defconst check-root-allowed-untracked
  '(".git" ".local" "custom.el" ".claude" ".clj-kondo" ".lsp")
  "Untracked root entries sanctioned by AGENTS.md.")

(defun check-root-tracked (dir)
  "Top-level names git tracks in DIR."
  (let ((default-directory (file-name-as-directory dir)))
    (delete-dups
     (mapcar (lambda (path) (car (split-string path "/")))
             (process-lines "git" "ls-files")))))

(defun check-root-strays (dir)
  "Entries at the root of DIR that are neither tracked nor sanctioned."
  (let ((allowed (append check-root-allowed-untracked (check-root-tracked dir))))
    (sort (seq-remove (lambda (name) (member name allowed))
                      (directory-files dir nil directory-files-no-dot-files-regexp))
          #'string<)))

(defun check-root-main (&optional dir)
  "Report strays in DIR and exit non-zero if there are any."
  (let ((strays (check-root-strays (or dir default-directory))))
    (if (null strays)
        (progn (message "check-root: clean")
               (kill-emacs 0))
      (message "Stray entries at the config root:")
      (dolist (name strays) (message "  %s" name))
      (message "\nSomething ran Emacs without --init-directory pointing at a sandbox.")
      (kill-emacs 1))))
