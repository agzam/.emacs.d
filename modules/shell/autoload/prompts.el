;;; modules/shell/autoload/prompts.el --- eshell prompt -*- lexical-binding: t; -*-

;; Folded from Doom :term eshell autoload/prompts.el, renamed.
;; Deviation: shrink-path dropped - pwd segment shows the plain
;; abbreviated path instead of the shrunk one.

(require 'cl-lib)

;;;###autoload
(defface eshell-prompt-pwd '((t (:inherit font-lock-constant-face)))
  "Face for the working-directory segment of the eshell prompt."
  :group 'eshell)

;;;###autoload
(defface eshell-prompt-git-branch '((t (:inherit font-lock-builtin-face)))
  "Face for the git branch segment of the eshell prompt."
  :group 'eshell)

(defun current-git-branch ()
  "Return \" [branch]\" for the checked-out branch or ref, else an empty string."
  (cl-destructuring-bind (status . output)
      (doom-call-process "git" "symbolic-ref" "-q" "--short" "HEAD")
    (if (equal status 0)
        (format " [%s]" output)
      (cl-destructuring-bind (status . output)
          (doom-call-process "git" "describe" "--all" "--always" "HEAD")
        (if (equal status 0)
            (format " [%s]" output)
          "")))))

;;;###autoload
(defun eshell-default-prompt-fn ()
  "Generate the prompt string for eshell; set as `eshell-prompt-function'."
  (concat (if (bobp) "" "\n")
          (propertize (abbreviate-file-name (eshell/pwd))
                      'face 'eshell-prompt-pwd)
          (propertize (current-git-branch)
                      'face 'eshell-prompt-git-branch)
          (propertize " λ" 'face (if (zerop eshell-last-command-status) 'success 'error))
          " "))
