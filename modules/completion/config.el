;;; modules/completion/config.el --- vertico/consult/completion-preview stack -*- lexical-binding: t; -*-
;;; Commentary:
;; Port of ~/.doom.d/modules/custom/completion (+icons +minibuffer +childframe,
;; flags inlined).  Deltas from the Doom original:
;; - package! recipes became :ensure recipes; vertico ships its extensions
;;   via :files, replacing the straight build-path load-path hack
;; - vertico extensions and completion-preview use :ensure nil
;; - corfu and satellites (popon, corfu-terminal, kind-icon) dropped: built-in
;;   completion-preview-mode plus the echo-area pager is the in-buffer UI
;; - autoload/*.el are verbatim copies, loaded eagerly before this file
;;; Code:

;;; * in-buffer completion defaults

(setopt completion-cycle-threshold 1
        tab-always-indent 'complete
        dabbrev-ignored-buffer-modes '(pdf-view-mode dired-mode ghostel-mode))

(add-hook! 'doom-init-modules-hook
  (defun reset-lsp-completion-provider-h ()
    (after! lsp-mode
      (setq lsp-completion-provider :none))))

(add-hook! 'lsp-completion-mode-hook
  (defun init-orderless-lsp-completions-h ()
    (setf (alist-get 'lsp-capf completion-category-defaults)
          '((styles . (orderless flex)))))
  (defun lsp-completion-off-in-text-modes-h ()
    (when (and lsp-completion-mode
               (member major-mode '(text-mode org-mode markdown-mode
                                    message-mode git-commit-mode)))
      (lsp-completion-mode -1))))

;;; * completion-preview (built-in)

(use-package completion-preview
  :ensure nil
  :hook (doom-first-buffer . global-completion-preview-mode)
  :config
  (setopt completion-preview-minimum-symbol-length 3)
  ;; Suppress the built-in "i out of n" cycling echo; the echo-area candidate
  ;; list below (`completion-preview-echo-candidates') replaces it.
  (setopt completion-preview-message-format nil)
  ;; Org rebinds letters to `org-self-insert-command' (and DEL to
  ;; `org-delete-backward-char'); teach completion-preview to refresh on them.
  (dolist (cmd '(org-self-insert-command org-delete-backward-char))
    (add-to-list 'completion-preview-commands cmd))
  (map! :map completion-preview-active-mode-map
        ;; Accept the preview even where a major mode grabs <tab> (ECA, Org).
        "<tab>" #'completion-preview-insert
        "TAB"   #'completion-preview-insert
        ;; Cycle candidates while a preview shows; the built-ins keep the
        ;; preview alive across presses (a custom wrapper would not).
        "M-/"   #'completion-preview-next-candidate
        "M-?"   #'completion-preview-prev-candidate))

(after! org
  (defadvice! completion-preview-accept-or-metaright-a (orig-fn &rest args)
    "Accept the completion preview if shown, else run ORIG-FN (`org-metaright')."
    ;; evil-org's high-precedence insert map shadows our global M-l; advise
    ;; the command itself instead of fighting keymap precedence.
    :around #'org-metaright
    (if (bound-and-true-p completion-preview-active-mode)
        (completion-preview-insert)
      (apply orig-fn args))))

(after! completion-preview
  ;; Popup-less candidate list: echo the current page of candidates.
  (when (fboundp 'completion-preview--update)
    (advice-add 'completion-preview--update :after #'completion-preview-echo-candidates))
  (advice-add 'completion-preview-next-candidate :after #'completion-preview-echo-candidates)
  (advice-add 'completion-preview-active-mode :after #'completion-preview-echo-clear)
  (advice-add 'completion-preview-next-candidate :around
              #'completion-preview-next-candidate-guard-a)
  ;; M+number completes with the Nth candidate of the visible page.
  (map! :map completion-preview-active-mode-map
        "M-1" (cmd! () (completion-preview-insert-indexed 1))
        "M-2" (cmd! () (completion-preview-insert-indexed 2))
        "M-3" (cmd! () (completion-preview-insert-indexed 3))
        "M-4" (cmd! () (completion-preview-insert-indexed 4))
        "M-5" (cmd! () (completion-preview-insert-indexed 5))))

;;; * orderless

(use-package orderless
  :after-call doom-first-input-hook
  :config
  (setopt orderless-affix-dispatch-alist
          '((?! . orderless-not)
            (?& . orderless-annotation)
            (?% . char-fold-to-regexp)
            (?` . orderless-initialism)
            (?= . orderless-literal)
            (?^ . orderless-literal-prefix)
            (?~ . orderless-flex))
          orderless-style-dispatchers
          '(vertico-orderless-dispatch
            vertico-orderless-disambiguation-dispatch))

  (setopt completion-styles '(orderless partial-completion basic)
          completion-category-defaults nil
          completion-category-overrides '((file (styles . (partial-completion)))
                                          (symbol (styles . (partial-completion))))))

;;; * cape - extra capfs feeding completion-at-point/completion-preview

(use-package cape
  :defer t
  :init
  (map! [remap dabbrev-expand] 'cape-dabbrev)
  (map! (:prefix ("C-c p" . "cape")
                 "p"  #'completion-at-point
                 "t"  #'complete-tag
                 "d"  #'cape-dabbrev
                 "h"  #'cape-history
                 "f"  #'cape-file
                 "k"  #'cape-keyword
                 "s"  #'cape-elisp-symbol
                 "a"  #'cape-abbrev
                 "l"  #'cape-line
                 "w"  #'cape-dict
                 "_"  #'cape-tex
                 "&"  #'cape-sgml
                 "r"  #'cape-rfc1345))
  (add-hook! latex-mode
    (defun cape-latex-capf-h ()
      (add-to-list 'completion-at-point-functions #'cape-tex)))

  (add-hook! (text-mode prog-mode)
    (defun cape-completion-at-point-functions-h ()
      (dolist (cfn '(cape-file
                     yasnippet-capf
                     cape-dabbrev
                     cape-dict
                     cape-keyword))
        (add-to-list 'completion-at-point-functions cfn :append)
        (setq-local completion-at-point-functions
                    (remove 'ispell-completion-at-point
                            completion-at-point-functions)))))

  (add-hook! emacs-lisp-mode
    (defun cape-completion-at-point-elisp-h ()
      (add-to-list 'completion-at-point-functions #'cape-elisp-symbol :append)))

  (add-hook! (org-mode markdown-mode)
    (defun cape-completion-at-point-org-md-h ()
      (add-to-list 'completion-at-point-functions #'cape-elisp-block :append))))

;; corfu-history used to switch this on; minibuffer histories still need it.
(use-package savehist
  :ensure nil
  :hook (doom-first-input . savehist-mode))

;;; * vertico

(use-package vertico
  :ensure (vertico :files (:defaults "extensions/*"))
  :hook (doom-first-input . vertico-mode)
  :init
  (defadvice! vertico-crm-indicator-a (args)
    :filter-args #'completing-read-multiple
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  :config
  (setopt vertico-resize nil
          vertico-count 17
          vertico-cycle t)
  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args)))

  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (map! :map vertico-map "DEL" #'vertico-directory-delete-char)

  ;; These commands are problematic and automatically show the *Completions* buffer
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)
  (defadvice! vertico--suppress-completion-help-a (fn &rest args)
    :around #'ffap-menu-ask
    (letf! ((#'minibuffer-completion-help #'ignore))
      (apply fn args)))
  (setopt completion-ignore-case t
          read-buffer-completion-ignore-case t)

  (defadvice! vertico-current-with-arrow-a
    ;; Prefix current candidate with arrow
    (orig cand prefix suffix index _start)
    :around #'vertico--format-candidate
    (setq cand (funcall orig cand prefix suffix index _start))
    (concat
     (if (= vertico--index index)
         (propertize "» " 'face 'vertico-current)
       "  ")
     cand))

  (map! :map vertico-map
        (:prefix ";"
         "." #'evil-insert-state
         ";" #'vertico-posframe-briefly-tall
         "b" #'vertico-multiform-buffer
         "c" #'embark-collect
         "d" #'consult-dir
         "e" #'embark-export
         "f" #'vertico-multiform-flat
         "g" #'vertico-multiform-grid
         "i" #'vertico-quick-insert
         "j" #'vertico-quick-jump
         "o" #'vertico-detour
         "p" #'vertico-multiform-posframe
         "r" #'vertico-multiform-reverse
         "s" #'vertico-suspend
         "t" #'vertico-posframe-briefly-tall
         "u" #'vertico-multiform-unobtrusive
         "C-;" #'embark-act
         :desc "insert ;" "SPC" (cmd! (insert ";")))
        "DEL" #'delete-backward-char
        "C-h" #'vertico-directory-delete-word
        "M-h" #'vertico-grid-left
        "M-l" #'vertico-grid-right
        "M-j" #'vertico-next
        "M-k" #'vertico-previous
        "C-e" #'vertico-scroll-up
        "C-y" #'vertico-scroll-down
        "]" #'vertico-next-group
        "[" #'vertico-previous-group
        "~" #'vertico-jump-to-home-dir-on~
        "C-/" #'vertico-jump-root
        "C-?" #'vertico-jump-sudo
        "M-m" #'embark-select
        "C-SPC" #'embark-preview)

  ;; Global so the same key round-trips: out of the minibuffer and back in.
  (map! "M-o" #'vertico-detour))

(use-package vertico-posframe
  :ensure (vertico-posframe :host github :repo "tumashu/vertico-posframe")
  :after vertico
  :config
  (setopt vertico-posframe-poshandler 'posframe-poshandler-frame-bottom-center)
  (setq
   vertico-posframe-global t
   vertico-posframe-height nil
   vertico-posframe-width 150
   marginalia-margin-threshold 500
   vertico-posframe-parameters `((alpha . 1.0))
   ;; Ignore buffer-local text-scale and use frame's default font size
   posframe-text-scale-factor-function (lambda (_) 0))
  (vertico-posframe-mode +1)

  ;; disable and restore posframe when emacsclient connects in terminal
  (add-hook! 'after-make-frame-functions
    (defun disable-vertico-posframe-in-term-h (frame)
      (when (and (not (display-graphic-p frame))
                 (bound-and-true-p vertico-posframe-mode))
        (vertico-posframe-mode -1)
        (setq vertico-posframe-restore-after-term-p t))))

  (add-hook! 'delete-frame-functions
    (defun restore-vertico-posframe-after-term-h (_frame)
      (when (bound-and-true-p vertico-posframe-restore-after-term-p)
        (vertico-posframe-mode +1))))

  ;; fixing "Doesn't properly respond to C-n"
  ;; https://github.com/tumashu/vertico-posframe/issues/11
  (defadvice! vertico-posframe--display-no-evil (fn lines)
    :around #'vertico-posframe--display
    (funcall-interactively fn lines)
    (evil-local-mode -1)))

(use-package vertico-repeat
  :ensure nil
  :after vertico
  :config
  (add-hook! 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package vertico-quick
  :ensure nil
  :after vertico)

(use-package vertico-directory
  :ensure nil
  :after vertico)

(use-package vertico-grid
  :ensure nil
  :after vertico
  :config
  (add-hook! 'minibuffer-exit-hook
    (defun vertico-grid-mode-off ()
      (vertico-grid-mode -1))))

(use-package vertico-buffer
  :ensure nil
  :after vertico
  :config
  (add-hook! 'vertico-buffer-mode-hook
    (defun vertico-buffer-h ()
      (vertico-posframe-mode (if vertico-buffer-mode -1 +1)))))

;;; * consult

(use-package consult
  :defer t
  :preface
  (define-key!
    [remap bookmark-jump]                 #'consult-bookmark
    [remap evil-show-marks]               #'consult-mark
    [remap evil-show-registers]           #'consult-register
    [remap goto-line]                     #'consult-goto-line
    [remap imenu]                         #'consult-imenu
    [remap Info-search]                   #'consult-info
    [remap locate]                        #'consult-locate
    [remap load-theme]                    #'consult-theme
    [remap recentf-open-files]            #'consult-recent-file
    [remap switch-to-buffer]              #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window
    [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
    [remap yank-pop]                      #'consult-yank-pop
    [remap persp-switch-to-buffer]        #'vertico-switch-workspace-buffer)
  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   search-project search-other-project
   search-project-for-symbol-at-point
   search-cwd search-other-cwd
   search-notes-for-symbol-at-point
   search-emacsd
   :preview-key 'any)

  (setopt consult-preview-key "C-SPC"
          consult-narrow-key "<")
  (consult-customize
   search-buffer
   :preview-key (list "C-SPC" :debounce 0.5 'any))

  (define-key!
    :keymaps (append default-minibuffer-maps)
    "C-/" #'consult-history)

  (map! :after consult :map isearch-mode-map "M-s l" #'consult-line)
  (map! :after consult :map minibuffer-local-map "C-r" #'consult-history)

  (remove-hook! 'consult-after-jump-hook 'consult--maybe-recenter)
  (add-hook! 'consult-after-jump-hook 'recenter))

(use-package consult-dir
  :defer t
  :init
  (map! [remap list-directory] #'consult-dir
        (:after vertico
         :map vertico-map
         "C-x C-d" #'consult-dir
         "C-x C-j" #'consult-dir-jump-file))
  :config
  (setopt consult-dir-project-list-function #'consult-dir-project-dirs
          consult-dir-shadow-filenames nil
          ;; Jump straight into the picked dir instead of re-prompting via find-file.
          consult-dir-default-command #'consult-dir-dired))

(use-package marginalia
  :hook (doom-first-input . marginalia-mode)
  :init
  (map! :map minibuffer-local-map
        :desc "Cycle marginalia views" "M-A" #'marginalia-cycle)
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  (advice-add #'marginalia--project-root :override #'doom-project-root)
  (dolist (c '((find-file-under-here . file)
               (find-in-config-dir . project-file)
               (persp-switch-to-buffer . buffer)))
    (add-to-list 'marginalia-command-categories c)))

(use-package nerd-icons-completion
  :defer t)

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setopt wgrep-auto-save-buffer t))

;;; * yasnippet

(use-package yasnippet
  :defer-incrementally eldoc easymenu help-mode
  :commands (yas-minor-mode-on
             yas-expand
             yas-expand-snippet
             yas-lookup-snippet
             yas-insert-snippet
             yas-new-snippet
             yas-visit-snippet-file
             yas-activate-extra-mode
             yas-deactivate-extra-mode
             yas-maybe-expand-abbrev-key-filter)
  :config
  (map! :map yas-minor-mode-map
        "M-j" #'yas-next-field
        "M-k" #'yas-prev-field)
  ;; Personal snippet library, in-tree since the Doom cord was cut.
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets/" user-emacs-directory))
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  (yas-reload-all)
  (yas-global-mode +1)

  (add-hook! 'yas-before-expand-snippet-hook
             #'temporarily-disable-smart-parens
             #'evil-insert-state)
  (advice-add 'yas-completing-prompt :around #'yas-completing-prompt-a))

(use-package yasnippet-capf
  :ensure (yasnippet-capf :host github :repo "elken/yasnippet-capf")
  :after cape
  :config
  (add-hook! 'yas-minor-mode-hook :append
    (defun remove-t-capf-h ()
      (remove-hook! 'completion-at-point-functions :local 't))))

(use-package consult-yasnippet
  :after (consult yasnippet)
  :config
  (setopt consult-yasnippet-use-thing-at-point t))

;;; * dash docs

(use-package dash-docs
  :defer t
  :config
  (setopt dash-docs-browser-func #'browse-dash-doc
          dash-docs-enable-debugging nil)

  ;; a check, before activation of a docset to install it if needed
  (advice-add 'dash-docs-activate-docset :around #'dash-docs-activate-docset-a)

  ;; overriding internal implementation fns for the time being
  ;; https://github.com/dash-docs-el/dash-docs/issues/23
  (advice-add 'dash-docs-install-user-docset :override #'dash-docs-ensure-user-docset)
  (advice-add 'dash-docs-install-docset :override #'dash-docs-ensure-docset)
  (advice-add 'dash-docs-unofficial-docsets :override #'dash-docs-unofficial-docsets-versioned))

(use-package consult-dash
  :ensure (consult-dash :host github :repo "emacsmirror/consult-dash")
  :commands (consult-dash)
  :config
  (map! :map consult-dash-embark-keymap
        :n "b" #'browse-url))

;; The set-lookup-handlers! 'lsp-mode call that doom.d kept here (inside
;; consult-dash's :config) moved to modules/lsp/config.el - here it only
;; registered after the first consult-dash invocation, so earlier lsp
;; buffers got no lookup handlers (2026-07 lsp port).

;;; completion.el ends here
