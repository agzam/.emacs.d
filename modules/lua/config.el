;;; modules/lua/config.el -*- lexical-binding: t; -*-

;; Ported from ~/.doom.d/modules/custom/lua.  Deviations from doom.d:
;; - lua-mode is built-in as of Emacs 31: immerrr's mode was upstreamed into
;;   progmodes/lua-mode.el (FSF 2025-2026), carrying `lua-indent-level' and
;;   `lua-search-documentation' - so :ensure nil, no MELPA package.
;; - friar dropped: it drove an AwesomeWM (Linux/X11) fennel REPL off a
;;   /home/ag/.luarocks path.  This machine is macOS, where the Lua/Fennel
;;   work is Hammerspoon - handled by spacehammer (writing) and the clojure
;;   module's monroe glue.  Nothing bound awesomewm-repl or friar.
;; - fennel starts lsp via `lsp!' (deferred, guarded) instead of doom.d's
;;   bare `lsp', matching the lsp module's blessed entry point.

(use-package fennel-mode
  :mode "\\.fnl\\'"
  :defer t
  :hook (fennel-mode . lsp!)
  :config
  (after! lsp-mode
    (add-to-list 'lsp-language-id-configuration '(fennel-mode . "fennel"))

    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection "fennel-ls")
      :activation-fn (lsp-activate-on "fennel")
      :server-id 'fennel-ls))

    (add-hook! lsp-mode
      (defun lsp-mode-bindings-override-h ()
        ;; fennel-ls doesn't implement textDocument/documentSymbol yet, so
        ;; imenu in fennel buffers falls back to consult-imenu.
        (map! :map lsp-mode-map
              [remap imenu]
              (cmd! (if (eq major-mode 'fennel-mode)
                        (call-interactively #'consult-imenu)
                      (call-interactively #'consult-lsp-file-symbols)))))))

  (set-lookup-handlers! 'fennel-mode
    :documentation #'fennel-show-documentation
    :definition #'fennel-find-definition)

  (when (eq system-type 'darwin)
    (add-hook! fennel-mode
      (defun fennel-mode-h ()
        (dash-docs-activate-docset "Hammerspoon")))))

(use-package lua-mode
  :ensure nil                           ; built-in since Emacs 31
  :defer t
  :init
  ;; Built-in `lua-indent-level' defaults to 4 otherwise.
  (setq lua-indent-level 2)
  :config
  ;; K -> the Lua docset via consult-dash (renders in eww), not
  ;; lua-search-documentation's external system browser.
  (set-lookup-handlers! 'lua-mode :documentation #'consult-dash-doc)
  (add-hook! lua-mode
    (defun lua-mode-activate-docset-h ()
      (dash-docs-activate-docset "Lua"))))
