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

  ;; A message button's keymap sits at point, ahead of evil's states, so the
  ;; two leader keys have to be freed there or they never reach the chatbuf.
  ;; "," was a duplicate of "<" (rewind a voice note), SPC of scroll-up.
  (map! :map telega-msg-button-map
        "SPC" nil
        "," nil)

  ;; Rootbuf: the chat under the cursor is the object; prefixes are telega's
  ;; own keymaps, so which-key lists them instead of them being restated here.
  (map! :map telega-root-mode-map
        (:localleader
         :desc "chat with"       "c" #'telega-chat-with
         :desc "info"            "i" #'telega-describe-chat
         :desc "operate on chat" "o" #'telega-transient-chat-operate
         :desc "toggle read"     "r" #'telega-chat-toggle-read
         :desc "read filtered"   "R" #'telega-chats-filtered-toggle-read
         :desc "toggle pin"      "p" #'telega-chat-toggle-pin
         :desc "delete chat"     "d" #'telega-transient-chat-delete
         :desc "new chat"        "n" #'telega-chat-create
         :desc "search messages" "s" #'telega-view-search
         :desc "saved messages"  "S" #'telega-saved-messages
         :desc "chat buffers"    "b" #'telega-switch-buffer
         :desc "switch account"  "a" #'telega-account-switch
         :desc "quit telega"     "Q" #'telega-kill
         :desc "filter"          "/" telega-filter-map
         :desc "sort"            "\\" telega-sort-map
         :desc "views"           "v" telega-root-view-map
         :desc "folders"         "f" telega-folder-map
         :desc "goto"            "g" telega-root-fastnav-map
         :desc "describe"        "?" telega-describe-map))

  ;; Chatbuf: the message under the cursor is the object, so it keeps the
  ;; top level; chat-wide commands move to their uppercase twin.
  (map! :map telega-chat-mode-map
        (:localleader
         :desc "reply"             "r" #'telega-msg-reply
         :desc "edit"              "e" #'telega-msg-edit
         :desc "forward"           "f" #'telega-transient-msg-forward
         :desc "delete"            "d" #'telega-transient-msg-delete
         :desc "operate on msg"    "o" #'telega-transient-msg-operate
         :desc "react"             "!" #'telega-msg-add-reaction
         :desc "translate"         "t" #'telega-transient-msg-translate
         :desc "mark"              "m" #'telega-msg-mark-toggle
         :desc "favorite"          "*" #'telega-msg-favorite-toggle
         :desc "pin"               "^" #'telega-transient-msg-pin-toggle
         :desc "thread/topic"      "T" #'telega-msg-open-thread-or-topic
         :desc "save media"        "S" #'telega-transient-msg-save
         :desc "info"              "i" #'telega-describe-chat
         :desc "operate on chat"   "C" #'telega-transient-chat-operate
         :desc "chat with"         "c" #'telega-chat-with
         :desc "cancel reply/edit" "k" #'telega-chatbuf-cancel-dwim
         :desc "goto"              "g" telega-chatbuf-fastnav-map
         (:prefix ("y" . "copy")
          :desc "text" "y" #'telega-msg-copy-dwim
          :desc "link" "l" #'telega-msg-copy-link)
         (:prefix ("a" . "attach")
          :desc "attach..."      "a" #'telega-chatbuf-attach
          :desc "clipboard"      "c" #'telega-chatbuf-attach-clipboard
          :desc "file"           "f" #'telega-chatbuf-attach-file
          :desc "media"          "m" #'telega-chatbuf-attach-media
          :desc "photo"          "p" #'telega-chatbuf-attach-photo
          :desc "video"          "v" #'telega-chatbuf-attach-video
          :desc "video from URL" "u" #'telega-extract-and-attach-video
          :desc "gif"            "g" #'telega-chatbuf-attach-animation
          :desc "sticker"        "s" #'telega-chatbuf-attach-sticker
          :desc "screenshot"     "S" #'telega-chatbuf-attach-screenshot
          :desc "voice note"     "n" #'telega-chatbuf-attach-voice-note
          :desc "video note"     "N" #'telega-chatbuf-attach-video-note
          :desc "poll"           "P" #'telega-chatbuf-attach-poll
          :desc "scheduled"      "t" #'telega-chatbuf-attach-scheduled
          :desc "formatting"     "F" #'telega-chatbuf-input-formatting-set
          :desc "input options"  "o" #'telega-transient-chatbuf-input-options)
         (:prefix ("s" . "search")
          :desc "search chat"     "s" #'telega-chatbuf-filter-search
          :desc "search backward" "i" #'telega-chatbuf-inplace-search
          :desc "by sender"       "u" #'telega-chatbuf-filter-by-sender
          :desc "by topic"        "t" #'telega-chatbuf-filter-by-topic
          :desc "favorites"       "*" #'telega-chatbuf-filter-favorite
          :desc "filter"          "f" #'telega-chatbuf-filter
          :desc "cancel filter"   "c" #'telega-chatbuf-filter-cancel)))

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
