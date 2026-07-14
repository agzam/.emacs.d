;;; modules/chat/config.el -*- lexical-binding: t; -*-

;; Ported from doom.d modules/custom/chat.  telega needs tdlib + a compiled
;; telega-server (M-x telega-server-build, first run); the elpaca recipe just
;; ships the elisp plus etc/ (glyphs) and server/ (C source) so that build can
;; happen - elpaca's :defaults omit both.

(use-package telega
  :ensure (telega :host github :repo "zevlg/telega.el"
                  :files (:defaults "contrib" "etc" "server" "Makefile"))
  :defer t
  :config
  (setopt telega-server-libs-prefix
          (cond ((eq system-type 'darwin) "/opt/homebrew/opt/tdlib")
                ((eq system-type 'gnu/linux) "/usr"))
          telega-completing-read-function #'completing-read-default)

  (map! :map telega-root-mode-map [remap imenu] #'telega-chat-with)
  (map! :map telega-chat-mode-map
        "C-l" #'recenter
        :i "s-<return>" #'telega-chatbuf-input-send)
  (map! :map telega-msg-button-map "SPC" nil)

  (add-hook! 'telega-chat-update-hook
    (defun telega-chat-update-h (_)
      (with-telega-root-buffer
       (hl-line-highlight))))

  (add-hook! 'telega-chat-mode-hook
    (defun telega-chat-mode-h ()
      (jinx-mode)
      (emojify-mode)))

  (add-hook! 'telega-root-mode-hook
    (defun telega-root-mode-h ()
      (hl-line-mode 1)
      (setq line-spacing 9))))

;; emojify renders emoji in telega chat buffers (telega-chat-mode-h) and backs
;; the global "SPC i e" insert binding.  doom.d relied on it arriving
;; transitively (via code-review); own it explicitly here.  Its download dir
;; is quarantined in doom-compat.el.
(use-package emojify
  :defer t)
