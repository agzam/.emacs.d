;;; modules/tree-sitter/config.el -*- lexical-binding: t; -*-

;; Emacs 31: built-in *-ts-modes register their own grammar source and
;; install it on first visit.  Explicit list (not t) so we don't clobber
;; yaml-mode (yaml-pro, highlight-indent-guides) or lua-mode, which hook
;; the non-ts major modes.  The whole block is 31-only API - CI's 30.1
;; skips it (grammar coverage there is thin anyway).
(use-package treesit
  :ensure nil
  :when (and (>= emacs-major-version 31)
             (fboundp 'treesit-available-p)
             (treesit-available-p))
  :config
  (setopt treesit-auto-install-grammar 'always
          treesit-enabled-modes '(python-ts-mode css-ts-mode
                                  typescript-ts-mode js-ts-mode json-ts-mode
                                  bash-ts-mode dockerfile-ts-mode
                                  java-ts-mode go-ts-mode))
  ;; .json opens in third-party json-mode, not in the core registry.
  (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
  (add-to-list 'major-mode-remap-alist '(mermaid-mode . mermaid-ts-mode))
  ;; yaml-pro-ts-mode and mermaid-ts-mode consume a grammar but, unlike
  ;; built-in *-ts-modes, don't register a source or self-install.  Ensure
  ;; lazily on their load - at boot the 'ask policy would prompt inside
  ;; every pty probe/smoke run.
  (dolist (src '((yaml    "https://github.com/ikatyang/tree-sitter-yaml" "master")
                 (mermaid "https://github.com/monaqa/tree-sitter-mermaid")))
    (add-to-list 'treesit-language-source-alist src))
  (with-eval-after-load 'yaml-pro (treesit-ensure-installed 'yaml))
  (with-eval-after-load 'mermaid-ts-mode (treesit-ensure-installed 'mermaid)))

;; install-only: reached via major-mode-remap / manual enable
(use-package clojure-ts-mode
  :ensure (clojure-ts-mode :host github :repo "clojure-emacs/clojure-ts-mode")
  :defer t)

(use-package evil-matchit
  :ensure (evil-matchit :host github :repo "redguardtoo/evil-matchit")
  :hook (doom-first-file . global-evil-matchit-mode))

;; install-only: reached via major-mode-remap from mermaid-mode
(use-package mermaid-ts-mode
  :ensure (mermaid-ts-mode :host github :repo "JonathanHope/mermaid-ts-mode"
                           :files ("mermaid-ts-mode.el"))
  :defer t)

(use-package mermaid-mode
  :defer t
  :config
  ;; install https://github.com/mermaid-js/mermaid-cli
  (when-let* ((mmdc (executable-find "mmdc")))
    (setopt mermaid-mmdc-location mmdc)))

(use-package treesitter-context
  :ensure (treesitter-context :host github :repo "zbelial/treesitter-context.el")
  :defer t
  :config
  ;; the fold-supported-modes list only exists at load time - wire it up then
  (dolist (mode treesitter-context--fold-supported-mode)
    (add-hook (intern (format "%s-hook" mode)) #'treesitter-context-fold-mode))
  (advice-add 'evil-fold-action :around #'fold-all-with-treesitter-context-a))
