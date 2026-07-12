;;; modules/shell/config.el --- eshell, shell-pop, ghostel -*- lexical-binding: t; -*-

;; Ported from doom.d custom/shell, adjudicated: aliases-file setq dropped
;; (the file never existed; Doom :term eshell's default alist folded in
;; below), Hyprland env refresh, mise and yuck-mode dropped, package
;; installs never gated on local binaries (smoke determinism) - only
;; activation is.

(map! :map minibuffer-local-map "C-c C-i" #'insert-current-filename)

(after! shell
  ;; Something messes up blue in terminal; setting it via the theme doesn't
  ;; take, this is the workaround.
  (add-hook! '(shell-mode-hook doom-load-theme-hook)
    (defun set-shell-colors ()
      (when (custom-theme-enabled-p 'ag-themes-base16-ocean)
        (set-face-attribute 'ansi-color-blue nil :foreground "#00bfff"))))

  (map! :map shell-mode-map
        "C-j" nil
        "C-c C-l" #'comint-clear-buffer
        :localleader
        "c" #'comint-clear-buffer))

(after! eshell
  (add-hook! 'eshell-mode-hook
    (defun set-eshell-keys-h ()
      (map! :map eshell-mode-map
            :desc "clear" "C-c C-l" #'eshell-clear-buffer
            :desc "detach" "C-<return>" #'eshell-send-detached-input
            :desc "kitty detach" "s-<return>" #'eshell-send-detached-input-to-kitty
            :i "C-u" nil
            (:localleader
             :desc "clear" "c" #'eshell-clear-buffer
             "b" #'eshell-insert-buffer-name))
      (map! :map eshell-hist-mode-map
            :desc "clear" "C-c C-l" #'eshell-clear-buffer
            :desc "output>buf" "C-c C-h" #'eshell-export-output
            (:unless (featurep 'eshell-atuin)
              :desc "history" "M-r" #'consult-history)
            (:when (featurep 'eshell-atuin)
              :desc "history" "M-r" #'eshell-atuin-history))))

  (cl-defmethod eshell-output-object-to-target :around (_obj (target marker))
    ;; immediately open the redirected buffer
    (let ((base (cl-call-next-method)))
      (when (buffer-live-p (marker-buffer target))
        (with-current-buffer (marker-buffer target)
          (ansi-color-apply-on-region (point-min) (point-max))
          (display-buffer (current-buffer))))
      base))

  ;; Folded from Doom :term eshell: with no aliases file on disk this alist
  ;; was the effective alias set. bd/cdp rows dropped - eshell-up and
  ;; cd-to-project aren't ported.
  (setq eshell-command-aliases-list
        '(("q"  "exit")
          ("f"  "find-file $1")
          ("ff" "find-file-other-window $1")
          ("d"  "dired $1")
          ("rg" "rg --color=always $*")
          ("l"  "ls -lh $*")
          ("ll" "ls -lah $*")
          ("git" "git --no-pager $*")
          ("gg" "magit-status")
          ("clear" "clear-scrollback")))
  ;; Don't let eshell persist or overwrite the elisp-defined aliases.
  (advice-add #'eshell-write-aliases-list :override #'ignore)

  (setq eshell-prompt-regexp "^[^#$\n]* [#$λ] "
        eshell-prompt-function #'eshell-default-prompt-fn))

(use-package shell-pop
  :ensure t
  :defer t
  :init
  (setq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (interactive) (eshell))))
  :config
  (setq shell-pop-window-position "bottom"))

(use-package vimrc-mode
  :ensure t
  :mode "\\.vim\\(rc\\)?\\'")

(use-package ghostel
  :ensure (ghostel :host github :repo "dakra/ghostel" :files ("lisp/*.el"))
  :defer t
  :init
  ;; Read at load time, so it must be set before ghostel loads. Keep the
  ;; auto-downloaded native module out of elpaca's build tree so package
  ;; rebuilds don't wipe it and force a re-download.
  (setq ghostel-module-directory (expand-file-name "ghostel/" doom-data-dir))
  :config
  ;; Let terminal programs drive the system clipboard via OSC52.
  ;; URL/file detection and shell integration are already on by default.
  (setopt ghostel-enable-osc52 t)

  (map! :map ghostel-mode-map
        "s-v" #'ghostel-yank)

  (add-hook! 'ghostel-mode-hook
    (defun ghostel-line-spacing-h ()
      (setq-local line-spacing 0.35))))

(use-package evil-ghostel
  :ensure (evil-ghostel :host github :repo "dakra/ghostel"
                        :files ("extensions/evil-ghostel/*.el"))
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

(use-package eshell-atuin
  :ensure t
  :defer t
  :init
  ;; Install everywhere, load and activate only where the binary exists -
  ;; featurep then arms the M-r bind in set-eshell-keys-h.
  (after! eshell
    (when (executable-find "atuin")
      (eshell-atuin-mode)))
  :config
  (setopt
   eshell-atuin-search-fields '(time duration command directory relativetime)
   eshell-atuin-history-format "%-70c %>10r %-40i "
   eshell-atuin-filter-mode 'global
   eshell-atuin-search-options nil)

  (defadvice! eshell-atuin-history-fix-sorting-a (ofn &optional arg)
    :around #'eshell-atuin-history
    (let ((vertico-sort-function nil))
      (funcall ofn arg))))

(use-package kkp
  :ensure t
  :defer t
  :hook (tty-setup . global-kkp-mode))

;; Provider: ~/GitHub/agzam/mxp - an Emacs Piper shell script driving
;; emacsclient (mxp:551 runs this hook when boundp); no elisp defines the
;; hook, so don't re-flag this as rot.
(defun on-mxp-buffer-update-h (buffer-name beg end)
  (with-current-buffer buffer-name
    (ansi-color-apply-on-region beg end)))

(add-hook 'mxp-buffer-update-hook #'on-mxp-buffer-update-h)
