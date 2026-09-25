;;; modules/email/autoload/thread.el -*- lexical-binding: t; -*-
;;; Commentary:
;; A whole thread in one buffer, the way notmuch-show reads it.  Gnus has
;; no such view: its tree pane still shows one article at a time.  Each
;; message gets a line of its own and its body is rendered through the
;; normal article pipeline, so shr, the MIME dissection and the charset
;; decoding are the ones the article buffer uses.  Only the message the
;; summary was on renders before the buffer shows.  The unread ones fill
;; in while Emacs is idle, because every gmane article is an NNTP round
;; trip.  A read one renders when it is unfolded - one sent message in
;; this store is a 33 MB article, and a thread must not pay for it to
;; show a line.
;;; Code:

(require 'cl-lib)
(require 'gnus)
(require 'gnus-art)
(require 'gnus-sum)
(require 'gnus-util)
(require 'mm-decode)
(require 'seq)

(defvar mail-thread-buffer-name "*mail thread*"
  "Buffer every thread is rendered into.")

(cl-defstruct (mail-thread-message (:constructor mail-thread-message-create))
  "One message of the thread on screen."
  article header marker)

(defvar-local mail-thread-messages nil
  "Messages of the thread in this buffer, oldest first.")

(defvar-local mail-thread-summary-buffer nil
  "Summary buffer the thread was opened from.")

(defvar-local mail-thread-group nil
  "Group the articles are fetched from.")

(defvar-local mail-thread-waiting nil
  "Unread messages whose bodies the fill has yet to render, in fill order.")

(defvar-local mail-thread-fill-timer nil
  "Idle timer rendering `mail-thread-waiting'.")

(defvar mail-thread-fill-delay 0.1
  "Seconds of idleness before the fill renders the next waiting body.")

;;; Reading the thread out of the summary

(defun mail-thread-entries ()
  "Thread at point as (ARTICLE . HEADER) pairs, oldest first.
Called in a summary buffer.  The summary sorts threads newest first;
inside one, reading order is chronological."
  (let ((entries
         (delq nil
               (mapcar (lambda (article)
                         (unless (memq article gnus-newsgroup-sparse)
                           (when-let* ((header (gnus-summary-article-header article)))
                             (cons article header))))
                       (save-excursion
                         (gnus-summary-top-thread)
                         (gnus-summary-articles-in-thread))))))
    (sort entries
          (lambda (a b)
            (time-less-p (gnus-date-get-time (mail-header-date (cdr a)))
                         (gnus-date-get-time (mail-header-date (cdr b))))))))

;;; Rendering one message

(defun mail-thread-render (group article)
  "Body of ARTICLE in GROUP as the article buffer would render it.
Nil when the store no longer holds the article: nnmaildir can lose one
from its in-memory map and then raises on the request."
  (with-temp-buffer
    (when (condition-case nil
              (gnus-request-article article group (current-buffer))
            (error nil))
      (let ((raw (current-buffer)))
        (with-temp-buffer
          (insert-buffer-substring raw)
          ;; treatments read the undecoded article back out of the
          ;; original-article buffer, and Gnus's own one holds whatever
          ;; the article buffer showed last
          (let ((gnus-newsgroup-name group)
                (gnus-original-article-buffer raw))
            (gnus-article-mode)
            (run-hooks 'gnus-article-decode-hook)
            (gnus-article-prepare-display))
          (widen)
          (article-goto-body)
          (prog1 (string-trim (buffer-substring (point) (point-max)))
            (mm-destroy-parts gnus-article-mime-handles)))))))

(defun mail-thread-sender (header)
  "Display name of HEADER's sender, or their address."
  (let* ((from (mail-decode-encoded-word-string (or (mail-header-from header) "")))
         (components (gnus-extract-address-components from)))
    (or (car components) (cadr components) from)))

(defun mail-thread-subject (header)
  "Decoded subject of HEADER."
  (mail-decode-encoded-word-string (or (mail-header-subject header) "")))

(defun mail-thread-message-line (message subject open)
  "Line describing MESSAGE, folded unless OPEN.
SUBJECT is the thread's, shown again only where a message changed it."
  (let* ((header (mail-thread-message-header message))
         (own (mail-thread-subject header)))
    (concat (propertize (if open "▼" "▶") 'face 'shadow)
            " "
            (propertize (mail-thread-sender header) 'face 'gnus-header-from)
            "  "
            (propertize (gnus-user-date (mail-header-date header)) 'face 'shadow)
            (unless (string= (gnus-simplify-subject-re own)
                             (gnus-simplify-subject-re subject))
              (concat "  " (propertize own 'face 'gnus-header-subject))))))

;;; Folding

(defun mail-thread-body-start (message)
  "Where MESSAGE's body begins, rendered or not."
  (save-excursion
    (goto-char (mail-thread-message-marker message))
    (forward-line 1)
    (point)))

(defun mail-thread-body-end (message)
  "Where MESSAGE's body ends."
  (if-let* ((next (cadr (memq message mail-thread-messages))))
      (marker-position (mail-thread-message-marker next))
    (point-max)))

(defun mail-thread-fold-overlay (message)
  "Overlay hiding MESSAGE's body, if it is folded."
  (let ((start (mail-thread-body-start message))
        (end (mail-thread-body-end message)))
    (when (< start end)
      (seq-find (lambda (overlay) (overlay-get overlay 'mail-thread-fold))
                (overlays-in start end)))))

(defun mail-thread-rendered-p (message)
  "Non-nil when MESSAGE's body is in the buffer, folded or not."
  (< (mail-thread-body-start message) (mail-thread-body-end message)))

(defun mail-thread-message-open-p (message)
  "Non-nil when MESSAGE's body is rendered and visible."
  (and (mail-thread-rendered-p message)
       (not (mail-thread-fold-overlay message))))

(defun mail-thread-set-indicator (message open)
  "Draw MESSAGE's fold indicator as OPEN.
These glyphs are in the buffer font; the smaller U+25B8 pair is not, and
a fallback font draws it at the wrong size."
  (let ((start (mail-thread-message-marker message)))
    (put-text-property start (1+ start) 'display (if open "▼" "▶"))))

(defun mail-thread-insert-body (message)
  "Render MESSAGE's body and insert it, leaving point after it.
Markers where the body goes end up after it, so a point or window start
on the next message stays there while a body lands above it."
  (goto-char (mail-thread-body-start message))
  (insert-before-markers
   (or (mail-thread-render mail-thread-group
                           (mail-thread-message-article message))
       "[the store no longer holds this message]")
   "\n\n"))

(defun mail-thread-expand (message)
  "Show MESSAGE's body and mark its article read."
  (let ((inhibit-read-only t))
    (if-let* ((overlay (mail-thread-fold-overlay message)))
        (delete-overlay overlay)
      (save-excursion (mail-thread-insert-body message)))
    (mail-thread-set-indicator message t)
    (set-buffer-modified-p nil))
  (mail-thread-mark-read (mail-thread-message-article message)))

(defun mail-thread-collapse (message)
  "Hide MESSAGE's body, keeping it rendered."
  (let ((inhibit-read-only t)
        (start (mail-thread-body-start message))
        (end (mail-thread-body-end message)))
    (when (< start end)
      (let ((overlay (make-overlay start end)))
        (overlay-put overlay 'invisible t)
        (overlay-put overlay 'mail-thread-fold t)))
    (mail-thread-set-indicator message nil)
    (set-buffer-modified-p nil)))

(defun mail-thread-mark-read (article)
  "Mark ARTICLE read in the summary the thread came from."
  (when (buffer-live-p mail-thread-summary-buffer)
    (with-current-buffer mail-thread-summary-buffer
      (save-excursion
        (gnus-summary-mark-article article gnus-read-mark)))))

;;; Filling in unread bodies

(defun mail-thread-fill-order (entry)
  "Every message but ENTRY, in the order the fill renders them.
Reading goes on down from ENTRY, so the messages after it come first,
then the ones before it, nearest first.  Without ENTRY, top to bottom."
  (if-let* ((after (memq entry mail-thread-messages)))
      (append (cdr after)
              (reverse (seq-take-while (lambda (message) (not (eq message entry)))
                                       mail-thread-messages)))
    mail-thread-messages))

(defun mail-thread-next-waiting ()
  "Waiting message to render next: the one at point, else the first in line.
Messages an unfold rendered meanwhile leave the queue."
  (setq mail-thread-waiting (seq-remove #'mail-thread-rendered-p mail-thread-waiting))
  (let ((here (mail-thread-message-at-point)))
    (if (memq here mail-thread-waiting)
        here
      (car mail-thread-waiting))))

(defun mail-thread-fill (buffer timer)
  "Render BUFFER's waiting messages until input arrives.
TIMER is the idle timer running this; it cancels itself once BUFFER is
gone or holds another."
  (if (not (and (buffer-live-p buffer)
                (eq timer (buffer-local-value 'mail-thread-fill-timer buffer))))
      (cancel-timer timer)
    (with-current-buffer buffer
      (if (not (buffer-live-p mail-thread-summary-buffer))
          (mail-thread-stop-fill)
        ;; timers run with quitting inhibited, and an NNTP fetch can hang
        (let ((inhibit-quit nil))
          (condition-case nil
              (while-let ((message (and (not (input-pending-p))
                                        (mail-thread-next-waiting))))
                ;; off the queue first, so a render that signals is not
                ;; retried every idle period
                (setq mail-thread-waiting (delq message mail-thread-waiting))
                (mail-thread-expand message)
                (redisplay))
            (quit (mail-thread-stop-fill)
                  (signal 'quit nil))))
        (unless mail-thread-waiting
          (mail-thread-stop-fill))))))

(defun mail-thread-start-fill ()
  "Render the waiting messages whenever Emacs is idle."
  (when mail-thread-waiting
    (let ((timer (run-with-idle-timer mail-thread-fill-delay t #'ignore)))
      (timer-set-function timer #'mail-thread-fill (list (current-buffer) timer))
      (setq mail-thread-fill-timer timer))))

(defun mail-thread-stop-fill ()
  "Stop filling in bodies; whatever still waits stays a folded line."
  (when mail-thread-fill-timer
    (cancel-timer mail-thread-fill-timer))
  (setq mail-thread-fill-timer nil
        mail-thread-waiting nil))

;;; Commands

(defun mail-thread-message-at-point ()
  "Message point is inside."
  (let (found)
    (dolist (message mail-thread-messages found)
      (when (<= (mail-thread-message-marker message) (point))
        (setq found message)))))

(defun mail-thread-goto-message (message)
  "Put point on MESSAGE's line, at the top of the window."
  (goto-char (mail-thread-message-marker message))
  (when (get-buffer-window) (recenter 0)))

(defun mail-thread-move (count)
  "Move COUNT messages forward, or backward when COUNT is negative."
  (let* ((current (mail-thread-message-at-point))
         (index (+ (seq-position mail-thread-messages current) count)))
    (cond ((< index 0) (user-error "First message"))
          ((<= (length mail-thread-messages) index) (user-error "Last message"))
          (t (mail-thread-goto-message (nth index mail-thread-messages))))))

(defun mail-thread-next-message (&optional count)
  "Move to the COUNTth next message."
  (interactive "p" mail-thread-mode)
  (mail-thread-move (or count 1)))

(defun mail-thread-previous-message (&optional count)
  "Move to the COUNTth previous message."
  (interactive "p" mail-thread-mode)
  (mail-thread-move (- (or count 1))))

(defun mail-thread-toggle-message ()
  "Fold or unfold the message at point."
  (interactive nil mail-thread-mode)
  (let ((message (mail-thread-message-at-point)))
    (if (mail-thread-message-open-p message)
        (mail-thread-collapse message)
      (mail-thread-expand message))
    (goto-char (mail-thread-message-marker message))))

(defun mail-thread-quit ()
  "Leave the thread, giving its window back to the summary."
  (interactive nil mail-thread-mode)
  (mail-thread-stop-fill)
  (let ((summary mail-thread-summary-buffer))
    (bury-buffer (current-buffer))
    (when (buffer-live-p summary)
      (let ((gnus-summary-buffer summary))
        (gnus-configure-windows 'summary 'force)))))

(defun mail-thread-open-article ()
  "Read the message at point in the article buffer.
The copied text keeps its faces and links but not the MIME handles, so
attachments are opened here."
  (interactive nil mail-thread-mode)
  (let ((article (mail-thread-message-article (mail-thread-message-at-point)))
        (summary mail-thread-summary-buffer))
    (mail-thread-quit)
    (when (buffer-live-p summary)
      (pop-to-buffer summary)
      (gnus-summary-goto-subject article)
      (read-mail-article))))

(define-derived-mode mail-thread-mode special-mode "Mail thread"
  "Major mode for reading every message of a thread in one buffer."
  (buffer-disable-undo)
  ;; HTML paragraphs come unfilled, and the window decides where they wrap
  (visual-line-mode)
  (visual-wrap-prefix-mode)
  ;; a fill must not outlive the thread it was started for
  (add-hook 'kill-buffer-hook #'mail-thread-stop-fill nil t)
  (add-hook 'change-major-mode-hook #'mail-thread-stop-fill nil t))

(defun mail-thread-build (entries entry unreads)
  "Insert every message of ENTRIES, rendering the one the summary was on.
ENTRY is that article and UNREADS the articles the summary holds unread;
those wait in `mail-thread-waiting' for the fill, the rest stay a line."
  (let ((subject (mail-thread-subject (cdar entries)))
        (inhibit-read-only t)
        messages shown)
    (setq-local header-line-format
                (format "%s   %d messages" subject (length entries)))
    (pcase-dolist (`(,article . ,header) entries)
      (let ((message (mail-thread-message-create
                      :article article :header header
                      :marker (point-marker))))
        (push message messages)
        (insert (mail-thread-message-line message subject (eql article entry)) "\n")
        ;; a body unfolded later is inserted right where the next
        ;; message's marker sits, and that marker has to end up after it
        (set-marker-insertion-type (mail-thread-message-marker message) t)
        (when (eql article entry)
          (setq shown message)
          (mail-thread-insert-body message)
          (mail-thread-mark-read article))))
    (setq mail-thread-messages (nreverse messages)
          mail-thread-waiting (seq-filter
                               (lambda (message)
                                 (memq (mail-thread-message-article message) unreads))
                               (mail-thread-fill-order shown)))
    (set-buffer-modified-p nil)))

;;;###autoload
(defun open-mail-thread ()
  "Read every message of the thread at point in one buffer."
  (interactive nil gnus-summary-mode)
  (let* ((entry (or (gnus-summary-article-number)
                    (user-error "No article on this line")))
         (unreads gnus-newsgroup-unreads)
         (group gnus-newsgroup-name)
         (summary (current-buffer))
         (entries (or (mail-thread-entries)
                      (user-error "No article Gnus can fetch in this thread")))
         (buffer (get-buffer-create mail-thread-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (remove-overlays))
      (mail-thread-mode)
      ;; the next Gnus layout replaces a Gnus buffer's window
      (gnus-add-buffer)
      (setq mail-thread-summary-buffer summary
            mail-thread-group group)
      (mail-thread-build entries entry unreads)
      (mail-thread-start-fill))
    (let ((gnus-summary-buffer summary))
      (gnus-configure-windows 'mail-thread))
    (with-current-buffer buffer
      (when-let* ((message (seq-find (lambda (message)
                                       (eql (mail-thread-message-article message) entry))
                                     mail-thread-messages)))
        (mail-thread-goto-message message)))))

(provide 'mail-thread)
;;; thread.el ends here
