;;; modules/rust/config.el -*- lexical-binding: t; -*-

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :init
  ;; start lsp via `lsp!' below, not rustic's own auto-setup
  (setq rustic-lsp-setup-p nil)
  :config
  (add-hook 'rustic-mode-hook #'lsp!))
