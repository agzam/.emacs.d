;;; modules/dired/config.el --- dired + treemacs stack -*- lexical-binding: t; -*-
;;; Commentary:
;; Port of ~/.doom.d/modules/custom/dired.  Deviations:
;; - treemacs-projectile dropped (projectile is dropped config-wide);
;;   `treemacs-project-toggle' rebuilt on treemacs's own project machinery
;;   and `dired-jump-find-in-project' on project.el (autoload/project.el).
;; - lsp-treemacs parked until the lsp module ports.
;; - winum unignore glue dropped as rotted: upstream renamed
;;   treemacs--buffer-name-prefix -> treemacs-buffer-name-prefix, so doom.d's
;;   treemacs-mode-hook fn errored (void-variable) and its ".*Treemacs.*"
;;   removal targeted a pattern winum no longer carries.  Verified live
;;   behavior (treemacs windows unnumbered, treemacs's own winum
;;   compatibility glue intact) is what this config preserves.  Only the s-N
;;   treemacs-mode-map loop was alive - ported below.
;; - insert-directory-program gls setq dropped: Emacs 31's standard value
;;   resolves gls on darwin natively; only the effective listing switches
;;   remain (still assumes GNU ls semantics, as doom.d did).
;; - dired-kill-when-opening-new-dired-buffer nil dropped (set-to-default).
;; - treemacs persist files redirected in doom-compat.el's quarantine.
;; - dired-split-action macro replaced by `dired-open-item-in-split' + cmd!.
;; - dired-preview loaded eagerly in doom.d; deferred here (M-x only).
;;; Code:

(use-package treemacs
  :defer t
  :init
  (setopt treemacs-follow-after-init t
          treemacs-sorting 'alphabetic-case-insensitive-asc)
  :config
  (after! dired (treemacs-resize-icons 16))
  (treemacs-follow-mode 1)
  (add-hook 'treemacs-mode-hook #'hl-line-mode)
  (after! winum
    ;; global s-N bindings don't reach the treemacs evil state
    (dolist (wn (seq-map #'number-to-string (number-sequence 0 9)))
      (let ((f (intern (concat "winum-select-window-" wn)))
            (k (concat "s-" wn)))
        (map! :map treemacs-mode-map k f)))))

(use-package treemacs-evil
  ;; :after gives demand-after-parent here (always-defer nil), replacing
  ;; doom.d's manual (after! treemacs (require 'treemacs-evil))
  :after treemacs
  :init
  (add-to-list 'doom-evil-state-alist '(?T . treemacs))
  :config
  (map! :map evil-treemacs-state-map
        [return] #'treemacs-RET-action
        [tab]    #'treemacs-TAB-action
        "TAB"    #'treemacs-TAB-action
        ;; deliberate v/s swap for C-w {v,s} muscle-memory (Doom #1875)
        "o v" #'treemacs-visit-node-horizontal-split
        "o s" #'treemacs-visit-node-vertical-split
        "o l" #'treemacs-visit-node-horizontal-split
        "L"   (cmd! (treemacs-toggle-node :recursive))))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-mode)
  :config
  ;; icons in subtrees
  (advice-add 'dired-subtree-insert :after #'treemacs-icons-after-subtree-insert-a))

(use-package dired-imenu
  :after dired)

(use-package dired-subtree
  :after dired
  :init
  (setopt dired-subtree-cycle-depth 5))

(after! dired
  (setopt dired-use-ls-dired t
          dired-listing-switches "-aBhl --group-directories-first"
          dired-dwim-target t
          dired-do-revert-buffer t
          remote-file-name-inhibit-delete-by-moving-to-trash t
          dired-vc-rename-file t)

  (put 'dired-find-alternate-file 'disabled nil)

  (add-to-list 'dired-guess-shell-alist-user '("\\.pdf\\'" "open -a Preview"))

  (when (modulep! :custom search)
    (add-hook 'dired-after-readin-hook #'add-to-zoxide-cache))

  (defadvice! dired-do-shell-command-full-paths-a (orig-fn command &optional arg _)
    "Hand `dired-do-shell-command' full paths instead of local file names."
    :around #'dired-do-shell-command
    (let ((files (dired-get-marked-files nil current-prefix-arg nil nil t)))
      (funcall orig-fn command arg files)))

  (advice-add 'dired-do-rename :around #'dired-do-rename-wrapper-a)

  (add-hook! 'dired-mode-hook
             #'dired-hide-details-mode
             (defun evil-matchit-off-h ()
               ;; fboundp: keeps dired loadable without :custom tree-sitter
               (when (fboundp 'turn-off-evil-matchit-mode)
                 (turn-off-evil-matchit-mode)))
             (defun dired-set-keys-h ()
               ;; re-applied per buffer: evil-collection-dired's lazy setup
               ;; binds "o" after any config-time registration would run
               (map! :map dired-mode-map
                     :n "M-l" #'dired-subtree-cycle
                     :n "M-h" #'dired-remove-subtree
                     :n "M-k" #'dired-remove-subtree
                     :n "M-j" #'dired-subtree-down-n-open
                     :n "M-n" #'dired-subtree-next-sibling
                     :n "M-p" #'dired-subtree-previous-sibling

                     :n "o" nil
                     (:prefix ("o" . "open")
                      :desc "below" :n "j" (cmd! (dired-open-item-in-split #'window-split-and-follow))
                      :desc "right" :n "l" (cmd! (dired-open-item-in-split #'window-vsplit-and-follow))
                      :desc "left"  :n "h" (cmd! (dired-open-item-in-split #'split-window-horizontally))
                      :desc "above" :n "k" (cmd! (dired-open-item-in-split #'split-window-vertically))
                      :desc "ace-action" :n "a" #'dired-ace-action)
                     (:localleader
                      "l" #'dired-subtree-cycle
                      "h" #'dired-remove-subtree)))))

(use-package dired-sidebar
  :defer t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setopt dired-sidebar-should-follow-file t
          dired-sidebar-window-fixed nil))

(use-package dired-narrow
  :after dired
  :config
  (add-hook! 'dired-mode-hook
    (defun dired-narrow-keys-h ()
      (map! :map dired-mode-map
            (:localleader
             "n" #'dired-narrow-fuzzy
             "s" #'dired-sort-toggle-or-edit)))))

(use-package dired-preview
  :defer t
  :config
  (setopt dired-preview-delay 0.01
          dired-preview-max-size (expt 2 20)))

;;; config.el ends here
