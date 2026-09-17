;;; modules/ai/autoload/eca-handoff.el -*- lexical-binding: t; -*-

(defvar eca-chat-finished-hook)
(defvar eca-chat-transient-area-segments)
(defvar eca-chat--chat-loading)

(defvar-local eca-chat-handoff-successor nil
  "Name of the chat buffer this chat handed its work to.")

;;;###autoload
(define-minor-mode eca-chat-handoff-lock-mode
  "Refuse input in a chat whose work moved on to another chat.
Typing in the chat left behind is the accident worth preventing: the
prompt lands where nobody is working and the answer costs a read of a
context the successor already owns.  eca writes the chat through
`inhibit-read-only', so the lock stops the reader without stopping the
stream, and the chat stays readable, foldable and copyable.  Any key
bound to `read-only-mode' takes the chat back."
  :group 'eca
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [remap read-only-mode] #'eca-chat-handoff-reclaim)
            map)
  (setq buffer-read-only (and eca-chat-handoff-lock-mode t))
  (eca-chat-handoff--refresh))

(defun eca-chat-handoff--segment ()
  "Line the transient area shows while the chat is locked."
  (when eca-chat-handoff-lock-mode
    (concat (propertize (format "⛔ handed off%s - read-only, C-x C-q to reclaim"
                                (if eca-chat-handoff-successor
                                    (format " to %s" eca-chat-handoff-successor)
                                  ""))
                        'font-lock-face 'warning)
            "\n")))

(defun eca-chat-handoff--refresh ()
  "Redraw the transient area of the chat.
eca renders that area without lifting `buffer-read-only', so the redraw
that shows or drops the banner has to lift it here."
  (when (fboundp 'eca-chat--refresh-transient-area)
    (let ((inhibit-read-only t))
      (eca-chat--refresh-transient-area))))

(defun eca-chat-handoff--lock-h ()
  "Lock the chat now that the turn which armed the lock has ended."
  (remove-hook 'eca-chat-finished-hook #'eca-chat-handoff--lock-h t)
  (eca-chat-handoff-lock-mode 1))

;;;###autoload
(defun eca-chat-handoff-lock (&optional successor)
  "Lock this chat, naming SUCCESSOR as the chat that carries the work on.
A handoff happens inside a turn, and locking mid-turn would take away
the steering the reader still has over it, so a running turn only arms
the lock and the end of the turn closes it."
  (interactive (list (read-string "Handed off to: ")))
  (setq eca-chat-handoff-successor
        (unless (or (null successor) (string-empty-p successor))
          successor))
  (if (and (boundp 'eca-chat--chat-loading) eca-chat--chat-loading)
      (progn
        (add-hook 'eca-chat-finished-hook #'eca-chat-handoff--lock-h nil t)
        (list :armed t :successor eca-chat-handoff-successor))
    (eca-chat-handoff-lock-mode 1)
    (list :locked t :successor eca-chat-handoff-successor)))

;;;###autoload
(defun eca-chat-handoff-reclaim ()
  "Take the chat back, whether the lock is armed or already closed."
  (interactive)
  (remove-hook 'eca-chat-finished-hook #'eca-chat-handoff--lock-h t)
  (eca-chat-handoff-lock-mode -1))

(with-eval-after-load 'eca-chat
  (add-to-list 'eca-chat-transient-area-segments #'eca-chat-handoff--segment t))
