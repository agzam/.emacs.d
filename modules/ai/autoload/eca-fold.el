;;; modules/ai/autoload/eca-fold.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'occult)

;;;###autoload
(defcustom eca-chat-fold-injected-prompt-regexp "\\`Background job job-[0-9]+ "
  "First line of a user message eca was handed, not one the reader typed.
A background job reports itself through the chat as a user message and
arrives with the same face and overlay properties a typed prompt gets,
so only its text tells the two apart.  Nil treats every user message as
the reader's."
  :type '(choice (const :tag "Every user message is the reader's" nil)
                 regexp)
  :group 'eca)

(defun eca-chat--fold-blocks ()
  "Chat block overlays in buffer order."
  (sort (seq-filter (lambda (ov) (overlay-get ov 'eca-chat--expandable-content-id))
                    (overlays-in (point-min) (point-max)))
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun eca-chat--fold-block-region (ov)
  "Return (BEG . END) over the whole lines block OV occupies.
The label overlay is empty and sits on the label's first character.
The content overlay ends at the start of the line after the block,
whether it holds expanded content or nothing, so the block's last line
is the one before that."
  (let* ((content (overlay-get ov 'eca-chat--expandable-content-ov-content))
         (tail (if (and content (overlay-buffer content)
                        (< (overlay-start ov) (overlay-end content)))
                   (1- (overlay-end content))
                 (overlay-start ov))))
    (cons (save-excursion (goto-char (overlay-start ov)) (line-beginning-position))
          (save-excursion (goto-char tail) (line-end-position)))))

(defun eca-chat--fold-prompt-p (ov)
  "Non-nil when block OV holds a message the reader typed."
  (and (overlay-get ov 'eca-chat--user-message-id)
       (not (and eca-chat-fold-injected-prompt-regexp
                 (string-match-p eca-chat-fold-injected-prompt-regexp
                                 (save-excursion
                                   (goto-char (overlay-start ov))
                                   (buffer-substring-no-properties
                                    (point) (line-end-position))))))))

(defun eca-chat--fold-runs ()
  "Regions covering each stretch of blocks that are not the reader's prompts.
Blank text between two blocks keeps a stretch going.  A prompt or any
other text ends it, since that text is the reply itself."
  (let (runs run)
    (dolist (ov (eca-chat--fold-blocks))
      (let ((region (eca-chat--fold-block-region ov)))
        (cond
         ((eca-chat--fold-prompt-p ov)
          (when run
            (push run runs)
            (setq run nil)))
         ((and run
               (or (<= (car region) (cdr run))
                   (string-blank-p (buffer-substring-no-properties (cdr run) (car region)))))
          (setcdr run (max (cdr run) (cdr region))))
         (t
          (when run (push run runs))
          (setq run (cons (car region) (cdr region)))))))
    (when run (push run runs))
    (nreverse runs)))

(defun eca-chat--fold-setup ()
  "Tell occult what the noise of a chat is and how a fold summary reads.
The block stretches are the noise.  The status symbol, the time beside
it and the diff button stay in the buffer text and out of the line the
fold shows.  The block marker is a `line-prefix', with no text to
match, so the prefix is overridden instead."
  (setq-local occult-noise-regions-function #'eca-chat--fold-runs
              occult-summary-line-prefix ""
              occult-summary-replace-alist
              `((,(concat " " (regexp-quote eca-chat-mcp-tool-call-success-symbol)
                          " [0-9]+[ms]\\(?: [0-9]+s\\)?")
                 . "")
                (" view diff" . ""))))

;;;###autoload
(defun eca-chat-fold ()
  "Fold every stretch of tool calls, thoughts and other blocks into one occult fold.
The reader's prompts and the replies between the stretches stay
visible.  Each fold is a plain occult fold: its first line stays
visible, point can rest on it, and occult's keymap and
`occult-edit-region' work on it.  Safe to run again: a stretch a fold
already hides is left as it is, so only what the reader opened folds
back.  Also arms `eca-chat-auto-fold-mode', so each turn folds as it
ends from here on; `eca-chat-reveal' opens everything and disarms it.

A fold summary shows the label of the block it starts with and none of
eca's decorations around it: no status symbol, no elapsed time, no diff
button, no block marker.  The folds outlive eca's re-protect of the
history after every streamed chunk: that is a property change, and
occult keeps a fold through those."
  (interactive)
  (eca-chat--fold-setup)
  (let ((folded (occult-fold-noise)))
    (eca-chat-auto-fold-mode 1)
    (when (called-interactively-p 'interactive)
      (message "Folded %d stretch%s, auto-fold on" folded (if (= folded 1) "" "es")))
    folded))

;;;###autoload
(defun eca-chat-reveal ()
  "Open every fold in the chat and stop folding turns as they end.
The counterpart of `eca-chat-fold'."
  (interactive)
  (eca-chat-auto-fold-mode -1)
  (occult-reveal-all))

(defvar eca-chat--last-user-message-pos)

(defun eca-chat-auto-fold-h ()
  "Fold the block stretches of the newest answer.
`eca-chat--last-user-message-pos' is where that answer starts, right
after the prompt that asked for it, so folds the reader opened in
earlier turns stay open."
  (eca-chat--fold-setup)
  (occult-fold-noise (or eca-chat--last-user-message-pos (point-min))
                     (point-max)))

;;;###autoload
(define-minor-mode eca-chat-auto-fold-mode
  "Fold the blocks of a turn as soon as eca reports it finished.
Every block is complete by then; a fold over a block still running
dies when eca appends its status to the label.  Only the newest answer
folds, so folds the reader opened in earlier turns stay open.  A chat
starts with this off: `eca-chat-fold' arms it and `eca-chat-reveal'
disarms it."
  :group 'eca
  (if eca-chat-auto-fold-mode
      (add-hook 'eca-chat-finished-hook #'eca-chat-auto-fold-h nil t)
    (remove-hook 'eca-chat-finished-hook #'eca-chat-auto-fold-h t)))
