;;; modules/general/config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Port of ~/.doom.d/modules/custom/general.  Deltas from the Doom original:
;; - package! recipes folded into :ensure
;; - undo-fu, undo-fu-session, visual-fill-column recipes deferred until the
;;   :emacs undo / writing ports (TODO)
;;; Code:

;; Change the cursor color in emacs state. We do it this roundabout way
;; to ensure changes in theme doesn't break these colors.
(add-hook! '(doom-load-theme-hook doom-init-modules-hook)
  (defun evil-update-cursor-color-h ()
    (put 'cursor 'evil-emacs-color "SkyBlue2")
    (put 'cursor 'evil-normal-color "DarkGoldenrod2")
    (posframe-delete-all)))

(use-package transient
  :ensure (transient :host github :repo "magit/transient")
  :demand t)

(use-package winum
  :after-call doom-switch-window-hook
  :config
  (setopt winum-scope 'frame-local)
  (winum-mode +1)
  (dolist (wn (seq-map 'number-to-string (number-sequence 0 9)))
    (let ((f (intern (concat "winum-select-window-" wn)))
          (k (concat "s-" wn)))
      (map! :n k f)
      (map! :leader :n wn f
            :n (concat "w" wn) f)
      (global-set-key (kbd k) f))))

(use-package info+
  ;; EmacsWiki package: purged from MELPA in 2018; straight resolved it via
  ;; its emacsmirror recipe source, Elpaca needs the mirror spelled out.
  :ensure (info+ :host github :repo "emacsmirror/info-plus")
  :after-call Info-mode-hook
  :commands (info info-display-manual)
  :config
  (setopt Info-fontify-angle-bracketed-flag nil))

;; Core vendored from Doom modules/doom/compat/+smartparens.el. Left out on
;; purpose: the language rule blocks (cc/ruby/swift/haskell/python/ml/
;; markdown) - parked until those language modules port; Doom's quote/brace/
;; lisp-( pair rules - the after! smartparens block below re-specifies them
;; with this config's own :unless lists; the sly-mrepl-mode line (no sly).
(defun disable-show-paren-mode-h ()
  "Turn off `show-paren-mode' buffer-locally."
  (setq-local show-paren-mode nil))

(use-package smartparens
  :hook (doom-first-buffer . smartparens-global-mode)
  :commands (sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string)
  :config
  ;; default pair rules for various languages
  (require 'smartparens-config)
  ;; show-parens covers this faster and without overlay distraction
  (setopt sp-highlight-pair-overlay nil
          sp-highlight-wrap-overlay nil
          sp-highlight-wrap-tag-overlay nil)
  (after! evil
    ;; under evil, point sits ON the closing char in insert mode - sp must
    ;; treat that as inside the pair
          (setopt sp-show-pair-from-inside t
                  sp-cancel-autoskip-on-backward-movement nil)
    ;; sp binds C-g while pair overlays are active (even invisible ones),
    ;; forcing a double ESC out of insert mode
    (setq sp-pair-overlay-keymap (make-sparse-keymap)))

  ;; sp scans are expensive; tighter than the 100/10 defaults
  (setq sp-max-prefix-length 25
        sp-max-pair-length 4)

  ;; silence echo-area spam
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (alist-get key sp-message-alist) nil))

  (add-hook! 'eval-expression-minibuffer-setup-hook
    (defun smartparens-in-eval-expression-h ()
      "Enable sp in `read--expression' minibuffers (eval-expression, edebug)."
      (when smartparens-global-mode (smartparens-mode +1))))
  (add-hook! 'minibuffer-setup-hook
    (defun smartparens-in-evil-ex-h ()
      (when (and smartparens-global-mode (memq this-command '(evil-ex)))
        (smartparens-mode +1))))

  ;; minibuffer input is usually lisp; these aren't string quotes there
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "'" nil :actions nil)
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "`" nil :actions nil)

  ;; sp breaks evil's replace state
  (defvar buffer-smartparens-mode nil)
  (add-hook! 'evil-replace-state-exit-hook
    (defun enable-smartparens-maybe-h ()
      (when buffer-smartparens-mode
        (turn-on-smartparens-mode)
        (kill-local-variable 'buffer-smartparens-mode))))
  (add-hook! 'evil-replace-state-entry-hook
    (defun disable-smartparens-maybe-h ()
      (when smartparens-mode
        (setq-local buffer-smartparens-mode t)
        (smartparens-mode -1))))

  ;; sp's navigation is expensive and less useful under evil
  (add-hook! 'after-change-major-mode-hook
    (defun disable-smartparens-navigate-skip-match-h ()
      (setq sp-navigate-skip-match nil
            sp-navigate-consider-sgml-tags nil)))

  ;; no square-bracket space-expansion where it makes no sense
  (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode gfm-mode)
                 "[" nil :post-handlers '(:rem ("| " "SPC"))))

(after! smartparens
  (eval `(add-hook! , sp-lisp-modes
                      #'disable-show-paren-mode-h
                      #'show-smartparens-mode))

  ;; fix for smartparens. Doom's default module does things like skipping pairs if
  ;; one typed at the beginning of the word.
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
             :unless '(sp-point-before-same-p)))

  (sp-pair "\"" nil :unless '(sp-point-before-word-p
                              sp-point-after-word-p))

  (sp-local-pair sp-lisp-modes "(" ")"
                 :wrap ")"
                 :unless '(:rem sp-point-before-same-p)))

(use-package expreg
  :commands (evil-visual-char evil-visual-line evil-visual-block expreg-transient)
  :init
  (defadvice! evil-select-block-a (ofn &rest args)
    :around #'evil-visual-message
    (expreg-transient)
    (apply ofn args))
  :config
  (map! :map evil-visual-state-map
        "v" #'expreg-transient)

  (setq-default expreg-functions
                '(expreg--subword
                  expreg--word
                  expreg--sentence
                  expreg--line
                  expreg--list
                  expreg--string
                  expreg--treesit
                  expreg--comment
                  expreg--paragraph-defun
                  expreg--markdown-subtree)))

;; Loading ibuffer doesn't pull in ibuf-ext, where define-ibuffer-filter and
;; the grouping vars live; require it so the filters below actually define.
(after! ibuffer
  (require 'ibuf-ext)
  (setopt
   ibuffer-old-time 8 ; buffer considered old after that many hours
   ibuffer-group-buffers-by 'projects
   ibuffer-expert t
   ibuffer-show-empty-filter-groups nil
   ibuffer-jump-offer-only-visible-buffers t)

  (define-ibuffer-filter unsaved-file-buffers
      "Toggle current view to buffers whose file is unsaved."
    (:description "file is unsaved")
    (ignore qualifier)
    (and (buffer-local-value 'buffer-file-name buf)
         (buffer-modified-p buf)))

  (define-ibuffer-filter file-buffers
      "Only show buffers backed by a file."
    (:description "file buffers")
    (ignore qualifier)
    (buffer-local-value 'buffer-file-name buf))

  (define-ibuffer-filter non-special-buffers
      "Only show non-special buffers (without earmuffs)."
    (:description "non-special buffers")
    (ignore qualifier)
    (string-match "^[^*].*" (buffer-name buf))))

(after! avy
  ;; jumping straight to a lone candidate skips the read loop, and with it
  ;; every dispatch action - one extra keypress buys yank/teleport/embark
  (setopt avy-all-windows t
          avy-single-candidate-jump nil)
  (setf (alist-get ?. avy-dispatch-alist) #'avy-action-embark)
  (advice-add #'avy--process-1 :around #'avy-dispatch-guide-a))

;; ensure that browsing in Helpful and Info modes doesn't create
;; additional window splits
(add-to-list
 'display-buffer-alist
 `(,(rx bos (or "*helpful" "*info"))
   (display-buffer-reuse-window
    display-buffer-reuse-mode-window
    display-buffer-in-quadrant)
   (direction . right)
   (window . root)))

;; (Doom's yank-indent knobs and the undefadvice! that disarmed them are
;; gone: that advice never exists here - the config/default module that
;; installs it isn't vendored.)

(after! edit-indirect
  ;; I want indirect buffers to always appear on the right side of current window
  (add-to-list
   'display-buffer-alist
   `("\\*edit-indirect .*\\*"
     (display-buffer-reuse-window
      display-buffer-reuse-mode-window
      display-buffer-in-quadrant)
     (direction . right)
     (window . root))))

(after! evil
  (advice-add #'evil-ex-start-word-search :around #'evil-ex-visual-star-search-a)

  (defadvice! turn-off-writeroom-before-split-a (&rest args)
    "writeroom hangs Emacs on splits"
    :before #'evil-window-vsplit
    :before #'evil-window-split
    (when (bound-and-true-p writeroom-mode)
      (writeroom-mode -1)))

  ;; no evil in transients
  ;; otherwise, evil prioritizes buffer's major mode keymap
  ;; for some reason tapping into transient-setup|buffer-hook
  ;; didn't work for me
  (add-hook! 'transient-exit-hook
    (defun transient-exit-evil-normal-h ()
      (save-mark-and-excursion
        (when (evil-emacs-state-p)
          (evil-normal-state)))))

  (defadvice! osc52-clipboard-in-ssh-session-a (&rest _)
    "Make Emacs propagate yanked shit to system clipboard.

    OSC 52 is an escape sequence for clipboard operations in terminals!
    OSC - OS command (part of the terminal control sequences)
    52 = the specific command number for clipboard operations

    It's a standardized way to say:
    `hey terminal, put this crap in the system clipboard`"
    :after #'kill-new
    :after #'kill-append
    (when (and (not (display-graphic-p))
               (getenv "SSH_CONNECTION"))
      (let* ((text (current-kill 0 t))
             (base64 (base64-encode-string
                      (encode-coding-string text 'utf-8) t)))
        (send-string-to-terminal
         (format "\033]52;c;%s\007" base64))))))

;; VC-root filter groups, used by the sidebar hook and the ",g v" binding.
(use-package ibuffer-vc
  :after ibuffer)

(use-package ibuffer-sidebar
  :defer t
  :commands (ibuffer-sidebar-toggle-sidebar ibuffer-sidebar-jump)
  :config
  (add-hook! ibuffer-sidebar-mode
    (defun ibuffer-sidebar-h ()
      (ibuffer-vc-set-filter-groups-by-vc-root)
      (ibuffer-do-sort-by-recency)
      (call-interactively #'ibuffer-filter-by-non-special-buffers)))

  (setopt ibuffer-sidebar-use-custom-font t
          ibuffer-sidebar-width 30
          ibuffer-sidebar-pop-to-sidebar-on-toggle-open nil)
  (set-face-attribute 'ibuffer-sidebar-face nil :height 0.9))

;;; scratch: the persistent scratch replaces the built-in *scratch*

;; It takes the *scratch* name at startup (startup-scratch-buffer buries
;; the stillborn built-in) and get-scratch-buffer-create - the choke point
;; every built-in path funnels through (scratch-buffer, emacsclient,
;; fallbacks) - hands out the persistent buffer, so a killed scratch
;; resurrects with its persisted state instead of as dead weight.
(unless noninteractive
  (setopt initial-buffer-choice #'startup-scratch-buffer)
  (defadvice! get-scratch-buffer-create-a ()
    :override #'get-scratch-buffer-create
    (scratch-buffer-create nil (scratch--initial-mode) default-directory nil)))

;;; zen (writeroom): SPC t z - helpers and knobs in autoload/zen.el

(use-package writeroom-mode
  :defer t
  :init
  (defalias 'zen-toggle #'writeroom-mode)
  :config
  ;; per-buffer zen: no frame-wide effects, frame geometry has its own
  ;; transient (SPC z f)
  (setopt writeroom-global-effects nil
          writeroom-maximize-window t)
  (setopt visual-fill-column-adjust-for-text-scale nil)
  (add-hook 'writeroom-local-effects #'zen-text-scale-h t)
  ;; manual zoom inside zen must re-fit the centered column
  (advice-add #'text-scale-adjust :after #'visual-fill-column-adjust))

(use-package mixed-pitch
  :defer t
  :init
  (add-hook 'writeroom-local-effects #'zen-mixed-pitch-h)
  :config
  ;; Doom's fixed-pitch list minus doom-themes/solaire/org-ref faces the
  ;; lab doesn't define
  (dolist (face '(org-date org-footnote org-special-keyword
                  org-property-value org-tag org-todo org-done
                  font-lock-comment-face))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))

(after! evil
  (advice-add #'evil-window-split :before #'turn-off-writeroom-before-split-a)
  (advice-add #'evil-window-vsplit :before #'turn-off-writeroom-before-split-a))

(use-package which-key-posframe
  :after (which-key)
  :config
  (defun posframe-poshandler-frame-right-vertical (info)
    (cons (- (plist-get info :parent-frame-width)
             (plist-get info :posframe-width) 10)
          (max 0 (/ (- (plist-get info :parent-frame-height)
                       (plist-get info :posframe-height))
                    2))))
  (setopt which-key-posframe-poshandler 'posframe-poshandler-frame-right-vertical)

  (defadvice! which-key-posframe-dynamic-height-a (fn act-popup-dim)
    :around #'which-key-posframe--show-buffer
    (let ((max-h (- (frame-height) 2)))
      (setq which-key-min-display-lines max-h)
      (funcall fn (cons (min (car act-popup-dim) max-h)
                        (cdr act-popup-dim)))))

  (add-hook! 'which-key-posframe-mode-hook
    (defun which-key-posframe-mode-h ()
      (if which-key-posframe-mode
          (setq which-key-max-display-columns 2
                which-key-max-description-length 100)
        (setq which-key-popup-type 'side-window
              which-key-max-display-columns nil
              which-key-min-display-lines 6
              which-key-max-description-length 35))))

  (when (display-graphic-p)
    (which-key-posframe-mode +1)))

(use-package ultra-scroll
  :ensure (ultra-scroll :host github :repo "jdtsmith/ultra-scroll")
  :after-call (doom-first-file-hook)
  :defer t
  :init
  (setopt scroll-conservatively 101 ; important
          scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(after! image-mode
  (map! :map image-mode-map
        (:prefix ("z" . "zoom")
         :n "k" #'image-increase-size
         :n "j" #'image-decrease-size)))

(after! flycheck
  (map! :map flycheck-mode-map
        (:localleader
         (:prefix ("ee" . "errors")
          "n" #'flycheck-next-error
          "p" #'flycheck-previous-error
          "y" #'flycheck-copy-errors-as-kill
          "l" #'flycheck-list-errors
          "t" #'lsp-treemacs-errors-list
          "s" #'flycheck-select-checker))))

(after! tramp
  ;; Typically I have RemoteCommand in my ssh/config that
  ;; automatically starts tmux, that can interfere with TRAMP and hang
  ;; the session after the handshake
  (add-to-list 'tramp-connection-properties
               (list "^/\\(ssh\\|scp\\):.*:"
                     "remote-shell" "/bin/bash")))

;;; config.el ends here
