;;; modules/lsp/config.el -*- lexical-binding: t; -*-

;; doom.d custom/lsp + the useful runtime slice of Doom's :tools lsp (+peek).
;; LSP_USE_PLISTS is set in early-init.el - it must be in the environment
;; both when elpaca byte-compiles lsp-mode and when it loads (lsp-use-plists
;; reads it at load time; compiled lsp-get accessors bake it in).
;; Path quarantine (lsp-session-file, lsp-server-install-dir, dap files)
;; lives in doom-compat.el.
;;
;; Dropped from doom.d, all git-resurrectable:
;; - lsp-ui-flycheck-list+ and its "ge"/q bindings: lsp-ui-mode never
;;   activates in live doom.d (the remove-hook! below its :hook killed it),
;;   so lsp-ui-flycheck-list--update was void at runtime there - bound-but-
;;   broken rot; flycheck itself is parked on :checkers here anyway.
;; - (remove-hook! lsp-mode #'lsp-ui-mode #'+lookup--init-lsp-mode-handlers-h):
;;   both hooks were Doom-registered; neither registration exists here.  The
;;   lsp-ui-mode suppression survives as lsp-ui-manual-only-a, and the lookup
;;   handlers are OURS now (set-lookup-handlers! below).
;; - lsp-modeline--enable-code-actions nil: private helper var, not the
;;   defcustom (that one is set right below it) - inert in doom.d.
;; - Doom's +lsp-optimization-mode: the root config.el already pins
;;   read-process-output-max at 10MB and a 200MB gc-cons-threshold globally,
;;   which supersedes the mode's save/restore dance.
;; - Doom's +lsp--respect-user-defined-checkers-a: flycheck-only, parked with
;;   :checkers.
;;
;; Deviation from doom.d: set-lookup-handlers! for lsp-mode moved HERE from
;; consult-dash's :config (completion module) - there it only registered
;; after the first consult-dash invocation, so lsp buffers opened earlier got
;; no lookup handlers at all (latent doom.d bug, load-order dependent).

(use-package lsp-mode
  ;; The last three carry no upstream cookies (SPC c a/r/o bind them
  ;; globally; cold presses should load lsp-mode, not void-function).
  :commands (lsp lsp-deferred lsp-install-server
             lsp-execute-code-action lsp-rename lsp-organize-imports)
  :init
  ;; Doom :tools lsp defaults worth keeping: don't auto-kill servers (the
  ;; defer-shutdown advice below owns that), disable features with slow
  ;; potential, no surprise reformatting, no s-l prefix map.
  (setq lsp-keep-workspace-alive nil
        lsp-enable-folding nil
        lsp-enable-text-document-color nil
        lsp-enable-on-type-formatting nil
        lsp-keymap-prefix nil)
  :config
  (setq
   lsp-eldoc-enable-hover t
   lsp-eldoc-render-all nil
   lsp-modeline-diagnostics-enable nil
   lsp-modeline-code-actions-enable nil
   lsp-headerline-breadcrumb-enable t
   lsp-completion-enable t
   lsp-enable-symbol-highlighting t
   lsp-enable-imenu nil
   lsp-treemacs-errors-position-params '((side . right))
   lsp-treemacs-sync-mode nil

   lsp-ui-sideline-enable nil
   lsp-ui-doc-enable nil
   lsp-ui-doc-position 'top

   lsp-semantic-tokens-enable nil
   lsp-lens-enable t
   lsp-enable-indentation t)

  (add-hook! 'lsp-mode-hook
    (defun set-lsp-mode-keys-h ()
      (map! :map lsp-mode-map
            [remap imenu] #'consult-lsp-file-symbols
            [remap xref-find-apropos] #'consult-lsp-symbols
            (:localleader
             (:prefix ("a" . "code actions")
                      "a" #'lsp-execute-code-action)
             (:prefix ("g" . "goto")
                      "n" #'lsp-find-declaration
                      "d" #'lsp-find-definition
                      "D" (cmd! (lsp-find-definition :display-action 'window))
                      "r" #'lsp-find-references
                      "R" (cmd! (lsp-find-references t :display-action 'window))
                      "s" #'consult-lsp-symbols)
             (:prefix ("f" . "format")
                      "b" #'lsp-format-buffer
                      "r" #'lsp-format-region
                      "i" #'lsp-organize-imports)
             (:prefix ("h" . "help")
                      "h" #'lsp-describe-thing-at-point)
             (:prefix ("t". "toggle")
                      "h" #'lsp--document-highlight
                      "L" #'lsp-lens-mode
                      "f" #'file-notify-rm-all-watches)
             (:prefix ("x" . "text/code")
                      "l" #'lsp-lens-show
                      "L" #'lsp-lens-hide)))))

  ;; Registers on lsp-mode-hook immediately (not behind consult-dash's load).
  (set-lookup-handlers! 'lsp-mode
    :definition #'lsp-lookup-definition-handler
    :references #'lsp-lookup-references-handler
    ;; K -> analyzer hover in *lsp-help* first (rust-analyzer, pyright, ...),
    ;; falling back to the offline docset search; both render in Emacs.
    :documentation #'lsp-lookup-documentation
    :implementations '(lsp-find-implementation :async t)
    :type-definition #'lsp-find-type-definition)

  ;; Close the signature popup on doom/escape (Doom :tools lsp behavior).
  (add-hook! 'doom-escape-hook
    (defun lsp-signature-stop-maybe-h ()
      "Close the displayed `lsp-signature'."
      (when lsp-signature-mode
        (lsp-signature-stop)
        t)))

  ;; Doom's deferred server shutdown: killing the last workspace buffer
  ;; doesn't nuke the (expensive) server for a few seconds, so quick file
  ;; hops and buffer reverts don't thrash it.
  (defvar lsp-defer-shutdown 3
    "If non-nil, defer workspace shutdown for this many seconds.")
  (defvar lsp--deferred-shutdown-timer nil)
  (defadvice! lsp-defer-server-shutdown-a (fn &optional restart)
    "Defer server shutdown for a few seconds."
    :around #'lsp--shutdown-workspace
    (if (or lsp-keep-workspace-alive
            restart
            (null lsp-defer-shutdown)
            (= lsp-defer-shutdown 0))
        (funcall fn restart)
      (when (timerp lsp--deferred-shutdown-timer)
        (cancel-timer lsp--deferred-shutdown-timer))
      (setq lsp--deferred-shutdown-timer
            (run-at-time
             (if (numberp lsp-defer-shutdown) lsp-defer-shutdown 3)
             nil (lambda (workspaces)
                   (dolist (ws workspaces)
                     (or (cl-some #'lsp-buffer-live-p
                                  (lsp--workspace-buffers ws))
                         (with-lsp-workspace ws
                           (let ((lsp-restart 'ignore))
                             (funcall fn))))))
             lsp--buffer-workspaces))))

  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

;; lsp-ui rides along installed (registry says +peek; the peek commands stay
;; reachable via M-x) but lsp-ui-mode only ever activates manually - doom.d
;; ground truth: its remove-hook! kept the mode off everywhere.
(use-package lsp-ui
  :defer t
  :init
  (defadvice! lsp-ui-manual-only-a (fn &rest args)
    "Keep `lsp--auto-configure' from force-enabling `lsp-ui-mode'."
    :around #'lsp--auto-configure
    (letf! ((#'lsp-ui-mode #'ignore))
      (apply fn args))))

(use-package consult-lsp
  :defer t)

;; Deviation from doom.d's shape (:defer t + :init after!-require): a bare
;; :after gives the same demand-after-lsp-mode semantics under this
;; use-package (always-defer premise was Doom-side rot, see MIGRATION).
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1))

(use-package dap-ui
  :ensure nil                          ; ships inside dap-mode
  :defer t
  :hook (dap-mode . dap-ui-mode)
  :hook (dap-ui-mode . dap-ui-controls-mode))
