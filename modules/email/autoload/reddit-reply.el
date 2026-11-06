;;; modules/email/autoload/reddit-reply.el -*- lexical-binding: t; -*-
;;; Commentary:
;; A reply to a Reddit post or comment, drafted in the thread view below
;; the message it answers, the way go-jira.el drafts a comment.  C-c C-c
;; posts it through reddit-api.el, and the thread's fetch brings it back.
;;; Code:

(require 'gnus-sum)
(require 'subr-x)

(defvar mail-thread-draft)
(defvar mail-thread-group)
(defvar mail-thread-buffer-name)
(defvar mail-thread-reload-waiting)
(defvar reddit-thread-fetched-at)

(defface reddit-reply-draft '((t :inherit fixed-pitch))
  "Text of a reply drafted in the thread view, markdown as Reddit takes it."
  :group 'gnus-summary)

(defvar-local reddit-reply-posting nil
  "Non-nil while the reply drafted in the view is on its way to Reddit.")

(defvar-keymap reddit-reply-mode-map
  :doc "Keys of a thread view holding a reply draft."
  "C-c C-c" #'reddit-reply-submit
  "C-c C-k" #'reddit-reply-abort
  ;; special-mode makes typing undefined, and DEL scrolls
  "<remap> <self-insert-command>" #'self-insert-command
  "DEL" #'delete-backward-char)

;; and makes commands of some printable keys, which type again
(map-keymap (lambda (event binding)
              (when (and binding (characterp event) (<= ?\s event ?~))
                (define-key reddit-reply-mode-map (vector event) #'self-insert-command)))
            special-mode-map)

(define-minor-mode reddit-reply-mode
  "Minor mode of a thread view while it holds a reply draft."
  :lighter " Reply"
  :keymap reddit-reply-mode-map)

;;;###autoload
(defun reddit-on-screen-p ()
  "Non-nil when the summary, thread view or article at hand reads a Reddit group."
  (when-let* ((group (cond ((derived-mode-p 'mail-thread-mode) mail-thread-group)
                           ((derived-mode-p 'gnus-article-mode) (car gnus-article-current))
                           ((derived-mode-p 'gnus-summary-mode) gnus-newsgroup-name))))
    (reddit-group-p group)))

;;; The draft

(defun reddit-reply-parent ()
  "Message the reply drafted in the view answers."
  (overlay-get mail-thread-draft 'reddit-reply-parent))

(defun reddit-reply-text ()
  "Text of the reply drafted in the view, trimmed."
  (let ((text (overlay-get mail-thread-draft 'reddit-reply-text)))
    (string-trim (buffer-substring-no-properties (overlay-start text) (overlay-end text)))))

(defun reddit-reply-insert-draft (message)
  "Insert an empty reply to MESSAGE after its body; return the draft's overlay.
The rest of the view turns read-only, and point lands in the reply."
  (let ((prefix (mail-thread-body-prefix (1+ (mail-thread-message-depth message))))
        (start (mail-thread-body-end message)))
    (with-silent-modifications
      (put-text-property (point-min) (point-max) 'read-only t)
      (goto-char start)
      ;; the next message's marker goes after it, and a filter's overlay
      ;; hiding that message leaves it out
      (insert-before-markers
       (propertize (concat "▼ Reply to " (mail-thread-sender (mail-thread-message-header message)))
                   'face 'shadow 'line-prefix prefix 'read-only t)
       ;; text typed right after it inherits nothing
       (propertize "\n" 'line-prefix prefix 'read-only t 'rear-nonsticky t)
       "\n"
       (propertize "\n" 'line-prefix prefix 'read-only t)))
    (let* ((body (- (point) 2))
           (text (make-overlay body (1+ body) nil nil t))
           (draft (make-overlay start (point))))
      (overlay-put text 'face 'reddit-reply-draft)
      (overlay-put text 'line-prefix prefix)
      (overlay-put text 'wrap-prefix prefix)
      (overlay-put draft 'reddit-reply-text text)
      (overlay-put draft 'reddit-reply-parent message)
      (goto-char body)
      draft)))

(defun reddit-reply-keep-line-h (&rest _)
  "Keep a newline ending the draft's text, so the next message keeps its line."
  (when-let* ((draft mail-thread-draft)
              (text (overlay-get draft 'reddit-reply-text))
              (end (overlay-end text))
              ((or (= end (overlay-start text)) (not (eql (char-before end) ?\n)))))
    (save-excursion
      (goto-char end)
      (insert "\n"))))

(defun reddit-reply-update-header-line ()
  "Name in the header line whom the draft answers, and the keys that end it."
  (let ((to (string-replace "%" "%%" (mail-thread-sender
                                      (mail-thread-message-header (reddit-reply-parent))))))
    (setq-local header-line-format
                (cons (propertize (concat " Reply to " to " ")
                                  'face '(:weight bold :inherit font-lock-function-name-face))
                      (if reddit-reply-posting
                          (list (propertize " │ posting…" 'face 'shadow))
                        (list (propertize " │ " 'face 'shadow)
                              (propertize "C-c C-c" 'face '(:weight bold :inherit success))
                              (propertize " submit " 'face 'shadow)
                              (propertize "│ " 'face 'shadow)
                              (propertize "C-c C-k" 'face '(:weight bold :inherit error))
                              (propertize " abort" 'face 'shadow)))))))

(defun reddit-reply-start (message)
  "Draft a reply to MESSAGE in the view, which edits as text while it lasts."
  (setq mail-thread-draft (reddit-reply-insert-draft message)
        buffer-read-only nil)
  (buffer-enable-undo)
  (add-hook 'after-change-functions #'reddit-reply-keep-line-h nil t)
  (reddit-reply-mode)
  (reddit-reply-update-header-line)
  (when (bound-and-true-p evil-local-mode)
    (evil-insert-state)))

(defun reddit-reply-end ()
  "Take the draft out of the view, which reads as it did before.
A reload that waited for the draft runs now."
  (let ((draft mail-thread-draft)
        (parent (reddit-reply-parent)))
    (remove-hook 'after-change-functions #'reddit-reply-keep-line-h t)
    (with-silent-modifications
      (delete-region (overlay-start draft) (overlay-end draft))
      (remove-list-of-text-properties (point-min) (point-max) '(read-only)))
    (delete-overlay (overlay-get draft 'reddit-reply-text))
    (delete-overlay draft)
    (setq mail-thread-draft nil
          buffer-read-only t)
    (buffer-disable-undo)
    (set-buffer-modified-p nil)
    (reddit-reply-mode -1)
    (mail-thread-update-header-line)
    (goto-char (mail-thread-message-marker parent))
    (when (bound-and-true-p evil-local-mode)
      (evil-normal-state))
    (when mail-thread-reload-waiting
      (setq mail-thread-reload-waiting nil)
      (mail-thread-reload))))

;;; Commands

;;;###autoload
(defun reply-on-reddit ()
  "Draft a reply to the Reddit message at point below it in the thread view.
\\<reddit-reply-mode-map>\\[reddit-reply-submit] posts it, \\[reddit-reply-abort] drops it."
  (interactive nil gnus-summary-mode mail-thread-mode gnus-article-mode)
  (unless (derived-mode-p 'mail-thread-mode)
    (run-in-mail-summary #'open-mail-thread)
    (set-buffer mail-thread-buffer-name))
  (when mail-thread-draft
    (goto-char (overlay-start (overlay-get mail-thread-draft 'reddit-reply-text)))
    (user-error "A reply is drafted here already"))
  (let ((message (mail-thread-message-at-point)))
    (unless (and message (reddit-group-p mail-thread-group)
                 (reddit-thing-id (mail-header-id (mail-thread-message-header message))))
      (user-error "Not a Reddit post or comment"))
    (unless (mail-thread-message-open-p message)
      (mail-thread-expand message))
    (reddit-reply-start message)
    (message "Write the reply, then C-c C-c to post it")))

(defun reddit-reply-submit ()
  "Post the reply drafted in the view to Reddit.
The draft goes once Reddit took it, and a fetch of the thread brings it in."
  (interactive nil mail-thread-mode)
  (unless mail-thread-draft
    (user-error "No reply drafted here"))
  (when reddit-reply-posting
    (user-error "The reply is on its way"))
  (let ((text (reddit-reply-text))
        (id (mail-header-id (mail-thread-message-header (reddit-reply-parent))))
        (view (current-buffer))
        (group mail-thread-group))
    (when (string-empty-p text)
      (user-error "The reply is empty"))
    (setq reddit-reply-posting t)
    (reddit-reply-update-header-line)
    (message "Posting the reply...")
    (reddit-api-post "/api/comment" `(("thing_id" . ,(reddit-thing-id id)) ("text" . ,text))
                     (lambda (_answer error)
                       (reddit-reply-posted view group (reddit-post-id id) error)))))

(defun reddit-reply-posted (view group post error)
  "End the draft in VIEW and fetch thread POST of GROUP, unless Reddit said ERROR.
A reply from the inbox or mine lands in mine, so every folder is fetched."
  (when (buffer-live-p view)
    (with-current-buffer view
      (setq reddit-reply-posting nil)
      (when mail-thread-draft
        (if error
            (reddit-reply-update-header-line)
          (reddit-reply-end)))))
  (cond (error
         (message "Reddit refused the reply: %s" error))
        ((subreddit-group-p group)
         ;; the view's opening fetched the thread within the interval
         (setq reddit-thread-fetched-at (assoc-delete-all post reddit-thread-fetched-at))
         (fetch-reddit-thread group post)
         (message "Replied; fetching the thread"))
        (t
         (fetch-reddit)
         (message "Replied; fetching Reddit"))))

(defun reddit-reply-abort ()
  "Drop the reply drafted in the view, asking first when it holds text."
  (interactive nil mail-thread-mode)
  (unless mail-thread-draft
    (user-error "No reply drafted here"))
  (when reddit-reply-posting
    (user-error "The reply is on its way"))
  (when (or (string-empty-p (reddit-reply-text))
            (yes-or-no-p "Drop the reply? "))
    (reddit-reply-end)
    (message "Reply dropped")))

(provide 'reddit-reply)
;;; reddit-reply.el ends here
