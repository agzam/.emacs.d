;;; modules/evil.el --- lean evil core -*- lexical-binding: t; -*-
;;; Commentary:
;; Distilled from Doom's :editor evil +everywhere (MIT), minus the extras
;; disabled in ~/.doom.d/packages.el (snipe, easymotion, embrace, etc.).
;; Personal settings folded in from ~/.doom.d/config.el.
;;; Code:

;; Set before evil loads; `defvar' so they remain overridable.
(defvar evil-want-keybinding nil)  ; evil-collection owns mode keybinds
(defvar evil-want-C-g-bindings t)
(defvar evil-want-C-i-jump nil)    ; [C-i] bound to evil-jump-forward below
(defvar evil-want-C-u-scroll t)
(defvar evil-want-C-u-delete t)
(defvar evil-want-C-w-delete t)
(defvar evil-want-Y-yank-to-eol t)
(defvar evil-want-abbrev-expand-on-insert-exit nil)
(defvar evil-respect-visual-line-mode nil)

(setq evil-ex-search-vim-style-regexp t
      evil-ex-visual-char-range t
      evil-mode-line-format nil
      evil-symbol-word-search t
      evil-normal-state-cursor 'box
      evil-insert-state-cursor 'bar
      evil-visual-state-cursor 'hollow
      evil-ex-interactive-search-highlight 'selected-window
      evil-kbd-macro-suppress-motion-error t
      ;; TODO: switch to undo-fu(+session) when :emacs undo gets ported.
      evil-undo-system 'undo-redo
      ;; PERF: don't spam the clipboard process on visual-mode movement.
      evil-visual-update-x-selection-p nil)

(elpaca (evil :wait t))
(evil-mode 1)
(evil-select-search-module 'evil-search-module 'evil-search)

(defadvice! evil--persist-state-a (fn &rest args)
  "When changing major modes, Evil's state is lost. Preserve it."
  :around #'set-auto-mode
  (if evil-state
      (evil-save-state (apply fn args))
    (apply fn args)))

(defadvice! evil--clean-isearch-overlays-a (&rest _)
  "`evil-ex-search' leaves isearch fold overlays open (emacs-evil/evil#1630)."
  :after #'evil-ex-search
  (isearch-clean-overlays))

(defadvice! evil--doom-escape-a (&rest _)
  "Run `doom/escape' (and thus `doom-escape-hook') on interactive ESC."
  :after #'evil-force-normal-state
  (when (called-interactively-p 'any)
    (call-interactively #'doom/escape)))

(defadvice! drop-insert-repeat-count-a (&rest _)
  "Never replay an insertion on insert-state exit.
A stray count typed before `i'/`a'/`o' silently arms
`evil-insert-count', and ESC then repeats the whole insertion.
Leaves `evil-insert-vcount' alone, so visual-block I/A still works."
  :before #'evil-cleanup-insert-state
  (setq evil-insert-count nil))

(defun remember-visual-selection (beg end)
  "Store BEG..END as the region `evil-visual-restore' brings back.
It reads these three variables and nothing else, so writing them is what
makes a region reachable by `gv'."
  (setq evil-visual-mark (copy-marker beg)
        evil-visual-point (copy-marker end)
        evil-visual-selection 'char))

(defadvice! yank-sets-visual-selection-a (fn &rest args)
  "Let `gv' select text any yank command inserted.
Every paste path funnels through `insert-for-yank': `yank' behind the
system paste key, `yank-pop', `yank-from-kill-ring', consult's yank
commands, the mouse yanks, and Evil's own paste."
  :around #'insert-for-yank
  (let ((beg (point)))
    (prog1 (apply fn args)
      (when (< beg (point))
        (remember-visual-selection beg (1- (point)))))))

(defadvice! paste-sets-visual-selection-a (&rest _)
  "Let `gv' select the text an Evil paste inserted.
Evil keeps the bounds in the `[' and `]' markers, which
`evil-visual-restore' never reads: it restores regions selected by hand,
so right after a paste it has nothing to give back.  The markers span
every copy of a counted paste, which the `insert-for-yank' advice, firing
once per copy, cannot see."
  :after '(evil-paste-before evil-paste-after)
  (when-let* ((beg (evil-get-marker ?\[))
              (end (evil-get-marker ?\])))
    (remember-visual-selection beg end)))

;; vim's jump-forward; reachable in GUI frames only, where doom-defaults'
;; key-translation hack synthesizes [C-i] distinct from TAB.
(map! :m [C-i] #'evil-jump-forward)

(use-package evil-collection
  :unless noninteractive
  :hook (elpaca-after-init . evil-collection-init)
  :preface
  (defvar evil-collection-disabled-list
    '(anaconda-mode
      company
      eglot
      elisp-mode
      ert
      lispy)
    "Modules to ignore in `evil-collection-mode-list'.")
  (defvar evil-collection-company-use-tng nil)
  (defvar evil-collection-setup-minibuffer nil)
  (defvar evil-collection-want-unimpaired-p nil)
  (defvar evil-collection-want-find-usages-bindings-p nil)
  (defvar evil-collection-outline-enable-in-minor-mode-p nil)
  :config
  (dolist (sym evil-collection-disabled-list)
    (if-let* ((elt (assq sym evil-collection-mode-list)))
        (cl-callf2 delete elt evil-collection-mode-list)
      (cl-callf2 delq sym evil-collection-mode-list)))

  ;; Flags resolved statically: :tools lookup and :tools eval are "enabled".
  (setopt evil-collection-binding-overrides
          '((repl-submit :enabled nil)
            (repl-newline :enabled nil)
            (pop-definition :enabled nil)
            (find-file :enabled nil)
            (find-definition :enabled nil)
            (find-usages :enabled nil)
            (lookup-doc :enabled nil)
            (goto-repl :enabled nil))
          evil-collection-key-blacklist
          (append (list doom-leader-key
                        doom-localleader-key
                        doom-leader-alt-key)
                  evil-collection-key-blacklist
                  '("gz" "<escape>"))))

(use-package evil-escape
  :ensure (:host github :repo "hlissner/evil-escape")
  :hook (doom-first-input . evil-escape-mode)
  :init
  ;; kj sequence and delay come from the user config.el layer.
  (setq evil-escape-excluded-states '(normal multiedit emacs motion)))

(use-package evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :hook (doom-first-input . global-evil-surround-mode))

(use-package evil-traces
  :after evil-ex
  :config
  (evil-traces-mode))

;;; evil.el ends here
