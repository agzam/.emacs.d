;;; modules/yaml/config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Port of ~/.doom.d/modules/custom/yaml.  Deltas from the Doom original:
;; - package! recipes folded into :ensure (yaml-pro stays pinned to GitHub).
;; - the two highlight-indent-guides hook fns extracted to autoload/ for the
;;   batch tier (tree-sitter fold precedent); +indent-guides-* -> the plus-free
;;   indent-guides-init-faces-h / indent-guides-disable-maybe-h.
;; - `doom-theme' is undefined here (doom-defaults dropped it); the initial
;;   face refresh guards on `custom-enabled-themes' instead - a theme is
;;   already enabled by the time yaml-mode first opens (the colors module's
;;   own ring anchor).
;; - yaml-pro-ts-mode consumes the yaml tree-sitter grammar, which the
;;   tree-sitter module ensures lazily on yaml-pro load (already wired there).
;;; Code:

(use-package yaml-mode
  :defer t)

(use-package jinja2-mode
  :defer t
  :mode "\\.jinja$"
  :config
  ;; The default reindents the whole buffer on save - disruptive and
  ;; imposing.  Leave reindenting to the explicit indent commands.
  (setopt jinja2-enable-indent-on-save nil))

(use-package highlight-indent-guides
  :defer t
  :hook (yaml-mode . highlight-indent-guides-mode)
  :init
  ;; :init runs before the package loads, so these can't be `setopt'ed
  ;; (their custom types aren't defined yet).
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-suppress-auto-error t)
  :config
  ;; Registered from :config (like doom.d) so the theme-refresh only fires
  ;; once the package is actually loaded - the hook fn calls into it.
  (add-hook 'doom-load-theme-hook #'indent-guides-init-faces-h)
  (when custom-enabled-themes
    (indent-guides-init-faces-h))
  (add-hook 'org-mode-local-vars-hook #'indent-guides-disable-maybe-h))

(use-package yaml-pro
  :ensure (yaml-pro :host github :repo "zkry/yaml-pro")
  :after yaml-mode
  :hook (yaml-mode . yaml-pro-ts-mode)
  :config
  (map! :map yaml-pro-ts-mode-map
        [remap imenu] #'yaml-pro-jump
        "C-c C-f" nil
        :n "]]" #'yaml-pro-ts-next-subtree
        :n "[[" #'yaml-pro-ts-prev-subtree
        :n "[{" #'yaml-pro-ts-first-sibling
        :n "]}" #'yaml-pro-ts-last-sibling
        :n "M-l" #'yaml-pro-ts-indent-subtree
        :n "M-h" #'yaml-pro-ts-unindent-subtree
        :n "zc" #'yaml-pro-fold-at-point
        :n "zo" #'yaml-pro-unfold-at-point
        :n "gk" #'yaml-pro-ts-prev-subtree
        :n "gj" #'yaml-pro-ts-next-subtree
        :n "gK" #'yaml-pro-ts-up-level
        :n "gJ" #'yaml-pro-ts-down-level
        :n "M-k" #'yaml-pro-ts-move-subtree-up
        :n "M-j" #'yaml-pro-ts-move-subtree-down))

;;; config.el ends here
