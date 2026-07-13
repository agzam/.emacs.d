;;; modules/python/config.el -*- lexical-binding: t; -*-

;; Ported from ~/.doom.d/modules/custom/python.  Toolchain (basedpyright +
;; ruff + uv, all driven through lsp-mode) confirmed current for 2026;
;; lsp-pyright's basedpyright API is unchanged.  Deviations from doom.d:
;; - the localleader map! and the python-fix-imports ruff advice live in the
;;   `python' block, so they arm on the first .py buffer instead of waiting on
;;   lsp-pyright's deferred load.
;; - lsp-ruff-python-path is set via `after! lsp-ruff' rather than doom.d's
;;   combined setopt over an unloaded defcustom.
;; - the two duplicate ("f" . "format") localleader prefixes are merged.

(use-package python
  :ensure nil                           ; built-in
  :defer t
  :mode ("[./]flake8\\'" . conf-mode)
  :mode ("/Pipfile\\'" . conf-mode)
  :config
  (add-hook! (python-ts-mode python-mode)
             #'lsp!
             #'python-lookup-handlers-h
             (defun activate-python-dash-docset-h ()
               (dash-docs-activate-docset "Python 3"))
             (defun set-python-trace-definition-h ()
               (setq-local magit-log-trace-definition-function
                           #'magit-python-which-function)))

  ;; Fix unused imports with Ruff (F401) instead of the built-in pyflakes path.
  (defadvice! python--fix-imports-with-ruff-a (&optional _beg _end)
    "Use Ruff instead of pyflakes."
    :override #'python-fix-imports
    (save-buffer)
    (shell-command (concat "ruff check --select F401 --fix " (buffer-file-name)))
    (revert-buffer t t t))

  (map!
   :map (python-mode-map python-ts-mode-map)
   "C-c C-n" #'python-edit-imports
   (:localleader
    "," #'python-fully-qualified-symbol-at-point
    (:prefix ("i" . "insert")
     "p" #'python-insert-ipdb)
    (:prefix ("f" . "format")
     "r" #'python-format
     "i" #'python-fix-imports)
    (:prefix ("c" . "convert")
     "j" #'python-to-json
     "e" #'python-to-edn)
    (:prefix ("e" . "errors")
     "f" #'python-fix-all))))

(use-package lsp-pyright
  :ensure (lsp-pyright :host github :repo "emacs-lsp/lsp-pyright")
  :defer t
  :init
  ;; Important: this needs to be set before the package loads.
  (setopt lsp-pyright-multi-root nil)
  :config
  (setopt
   ;; don't forget to:
   ;; uv tool install basedpyright
   ;; uv tool install ruff
   lsp-pyright-langserver-command "basedpyright"
   lsp-pyright-venv-path "."
   lsp-pyright-venv-directory ".venv"
   python-shell-interpreter "python")

  (setopt lsp-pyright-type-checking-mode "off")

  (lsp-register-custom-settings
   `(("basedpyright.analysis.typeCheckingMode" lsp-pyright-type-checking-mode)))

  (lsp-dependency
   'pyright
   `(:system ,(executable-find "basedpyright-langserver"))))

;; The ruff LSP client (`ruff server') attaches to python buffers alongside
;; pyright; point it at the plain `python' executable.
(after! lsp-ruff
  (setopt lsp-ruff-python-path "python"))
