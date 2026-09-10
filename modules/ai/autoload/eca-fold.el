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

(defun eca-chat--fold-region (beg end)
  "Fold BEG to END without disturbing the mark.
`occult-hide-region' deactivates the mark for its interactive callers;
here it also runs while the chat streams, when the reader may be
selecting text."
  (let ((mark-active nil))
    (occult-hide-region beg end)))

(defun eca-chat--fold-snapshot ()
  "Every occult fold in the buffer with its bounds."
  (mapcar (lambda (ov)
            (list ov (overlay-start ov) (overlay-end ov)))
          (seq-filter (lambda (ov) (overlay-get ov 'occult))
                      (overlays-in (point-min) (point-max)))))

;;;###autoload
(defun eca-chat-fold ()
  "Fold every stretch of tool calls, thoughts and other blocks into one occult fold.
The reader's prompts and the replies between the stretches stay
visible.  Each fold is a plain occult fold: its first line stays
visible, point can rest on it, and occult's keymap and
`occult-edit-region' work on it.  Safe to run again: occult absorbs the
folds already there.  `occult-reveal-all' opens them all, and evil's
\\<evil-normal-state-map>\\[evil-open-folds] is advised to do so.

A fold summary stops before the status symbol of the tool call it
starts with, so the checkmark and the time after it stay on the line
but out of the fold.  The setting is buffer-local because the folds
`eca-chat-refold-after-protect-a' rebuilds are made outside this
command."
  (interactive)
  (setq-local occult-summary-end-regexp
              (concat " " (regexp-quote eca-chat-mcp-tool-call-success-symbol)))
  (let ((folded 0))
    (dolist (run (eca-chat--fold-runs))
      (when (occult-hide-region (car run) (cdr run))
        (cl-incf folded)))
    (when (called-interactively-p 'interactive)
      (message "Folded %d stretch%s" folded (if (= folded 1) "" "es")))
    folded))

(defadvice! eca-chat-refold-after-protect-a (fn &rest args)
  "Rebuild the occult folds a history re-protect wiped out.
`put-text-property' fires the modification hook of every overlay in
the range it is handed as soon as one character in that range changes,
and `occult--modification-hook' deletes its fold.  eca re-protects the
history after every streamed chunk, at the end of a turn, and after a
block toggle, a history page or a resume, so without this a fold
survives only until the next chunk arrives.  Only the folds the
re-protect deleted come back, at the bounds they had, so a fold the
reader opened stays open."
  :around #'eca-chat--protect-non-prompt
  (let ((folds (eca-chat--fold-snapshot)))
    (prog1 (apply fn args)
      (pcase-dolist (`(,ov ,beg ,end) folds)
        (unless (overlay-buffer ov)
          (eca-chat--fold-region beg end))))))
