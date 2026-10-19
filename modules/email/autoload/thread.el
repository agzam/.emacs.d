;;; modules/email/autoload/thread.el -*- lexical-binding: t; -*-
;;; Commentary:
;; A whole thread in one buffer, the way notmuch-show reads it, each body
;; rendered through Gnus's own article pipeline.  The fill renders unread
;; bodies while Emacs is idle, and read ones only under the group parameter
;; mail-thread-unfold-read: one sent message of the store is 33 MB.
;;; Code:

(require 'cl-lib)
(require 'color)
(require 'gnus)
(require 'gnus-art)
(require 'gnus-search)
(require 'gnus-sum)
(require 'gnus-util)
(require 'mm-decode)
(require 'seq)
(require 'visual-fill-column)

(defvar mail-thread-buffer-name "*mail thread*"
  "Buffer every thread is rendered into.")

(cl-defstruct (mail-thread-message (:constructor mail-thread-message-create))
  "One message of the thread on screen, DEPTH replies below a root."
  article header marker (depth 0))

(defvar-local mail-thread-messages nil
  "Messages of the thread in this buffer, in reply order.")

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

(defvar-local mail-thread-filter-query nil
  "Query the thread is filtered by, nil while every message shows.")

(defvar-local mail-thread-filter-process nil
  "The notmuch process answering the latest filter query.")

(defvar mail-thread-max-indent 6
  "Levels the thread view indents at most.")

(defvar mail-thread-width 80
  "Text width of the thread view's column, besides the deepest guide bars.")

(defvar mail-thread-open-hook nil
  "Hook run in the thread view once it shows a thread opened from a summary.")

(defface mail-thread-guide '((t :inherit shadow))
  "Guide bars left of a reply in the thread view."
  :group 'gnus-summary)

(defface mail-thread-from '((t :inherit shadow))
  "Sender on a message's line in the thread view."
  :group 'gnus-summary)

(defvar mail-thread-guide-shade 0.15
  "Share of the text color in `mail-thread-guide', the rest the background's.")

(defvar mail-thread-from-shade 0.6
  "Share of the text color in `mail-thread-from', the rest the background's.")

;;; Reading the thread out of the summary

(defun mail-thread-entries ()
  "Thread at point as (ARTICLE . HEADER) pairs, in summary order.
Called in a summary buffer.  The placeholders Gnus makes up for missing
parents stay out."
  (delq nil
        (mapcar (lambda (article)
                  (unless (memq article gnus-newsgroup-sparse)
                    (when-let* ((header (gnus-summary-article-header article)))
                      (cons article header))))
                (save-excursion
                  (gnus-summary-top-thread)
                  (gnus-summary-articles-in-thread)))))

(defun mail-thread-oldest-first (entries)
  "ENTRIES, (ARTICLE . HEADER) pairs, sorted by date."
  (sort entries :key (lambda (entry) (gnus-date-get-time (mail-header-date (cdr entry))))
        :lessp #'time-less-p))

(defun mail-thread-arrange (entries)
  "ENTRIES in reply order, each pair as (DEPTH ARTICLE . HEADER).
A message follows the one it answers, siblings oldest first.  It answers
the nearest of its References that ENTRIES hold; one answering none of
them is a root at depth 0."
  (let ((by-id (make-hash-table :test #'equal))
        (replies (make-hash-table :test #'eq))
        (seen (make-hash-table :test #'eq))
        roots arranged)
    (dolist (entry entries)
      (puthash (mail-header-id (cdr entry)) entry by-id))
    (dolist (entry entries)
      (if-let* ((parent (seq-some (lambda (id) (gethash id by-id))
                                  (reverse (gnus-split-references
                                            (mail-header-references (cdr entry)))))))
          (push entry (gethash parent replies))
        (push entry roots)))
    (cl-flet ((walk (root)
                (let ((stack (list (cons 0 root))))
                  (while stack
                    (pcase-let ((`(,depth . ,entry) (pop stack)))
                      (unless (gethash entry seen)
                        (puthash entry t seen)
                        (push (cons depth entry) arranged)
                        (setq stack (nconc (mapcar (lambda (reply) (cons (1+ depth) reply))
                                                   (mail-thread-oldest-first
                                                    (gethash entry replies)))
                                           stack))))))))
      (mapc #'walk (mail-thread-oldest-first roots))
      ;; messages whose References loop have no root above them
      (while-let ((strays (seq-remove (lambda (entry) (gethash entry seen)) entries)))
        (walk (car (mail-thread-oldest-first strays)))))
    (nreverse arranged)))

;;; Rendering one message

;;;###autoload
(defun mail-render-message (raw group)
  "Body of the message in buffer RAW, rendered as GROUP's article buffer would."
  (with-temp-buffer
    (insert-buffer-substring raw)
    ;; treatments read the undecoded article back out of the
    ;; original-article buffer, and Gnus's own one holds whatever the
    ;; article buffer showed last
    (let ((gnus-newsgroup-name group)
          (gnus-original-article-buffer raw))
      (gnus-article-mode)
      (run-hooks 'gnus-article-decode-hook)
      (gnus-article-prepare-display))
    (widen)
    (article-goto-body)
    (prog1 (string-trim (buffer-substring (point) (point-max)))
      (mm-destroy-parts gnus-article-mime-handles))))

(defun mail-thread-render (group article)
  "Body of ARTICLE in GROUP as the article buffer would render it.
Nil when the store no longer holds the article: nnmaildir can lose one
from its in-memory map and then raises on the request."
  (with-temp-buffer
    (when (condition-case nil
              (gnus-request-article article group (current-buffer))
            (error nil))
      (mail-render-message (current-buffer) group))))

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
            (propertize (mail-thread-sender header) 'face 'mail-thread-from)
            "  "
            (propertize (gnus-user-date (mail-header-date header)) 'face 'shadow)
            ;; `mail-thread-draw-labels' shows the message's labels here
            (propertize " " 'mail-labels (mail-header-id header))
            (unless (string= (gnus-simplify-subject-re own)
                             (gnus-simplify-subject-re subject))
              (concat "  " (propertize own 'face 'gnus-header-subject))))))

;;; Indenting by depth

(defun mail-thread-guides (depth)
  "Guide bars left of each line of a message DEPTH replies deep, or nil.
They stop at `mail-thread-max-indent' levels."
  (when (< 0 depth)
    (propertize (apply #'concat (make-list (min depth mail-thread-max-indent) "│ "))
                'face 'mail-thread-guide)))

(defun mail-thread-shade (share)
  "Color SHARE of the way from the theme's background to its text, or nil."
  (when-let* ((background (color-name-to-rgb (or (face-background 'default nil t) "")))
              (text (color-name-to-rgb (or (face-foreground 'default nil t) ""))))
    (apply #'color-rgb-to-hex
           (append (cl-mapcar (lambda (from to) (+ from (* share (- to from))))
                              background text)
                   '(2)))))

(defun mail-thread-shade-faces (&rest _)
  "Color the guide bars and the senders as shades of the current theme.
A terminal whose colors Emacs does not know keeps their `shadow'."
  (when-let* ((guide (mail-thread-shade mail-thread-guide-shade))
              (from (mail-thread-shade mail-thread-from-shade)))
    (set-face-foreground 'mail-thread-guide guide)
    (set-face-foreground 'mail-thread-from from)))

(defun mail-thread-indent (message start end)
  "Draw MESSAGE's guide bars left of each line from START to END."
  (put-text-property start end 'line-prefix
                     (mail-thread-guides (mail-thread-message-depth message))))

(defun mail-thread-wrap-prefix (guides prefix)
  "GUIDES followed by PREFIX, the `wrap-prefix' visual-wrap gave a line.
Its stretch aligns to the window edge, and the line starts after GUIDES."
  (pcase prefix
    ((pred stringp) (concat guides prefix))
    (`(space :align-to ,width)
     (concat guides (propertize " " 'display `(space :width ,width))))
    (_ guides)))

(defun mail-thread-wrap-guides (start end)
  "Draw the guide bars on the continuation lines from START to END too.
Runs after `visual-wrap-prefix-function', which resets each line's
`wrap-prefix' there first."
  (goto-char end)
  (unless (bolp) (forward-line 1))
  (setq end (point))
  (goto-char start)
  (forward-line 0)
  (setq start (point))
  (while (< (point) end)
    (when-let* ((guides (get-text-property (point) 'line-prefix)))
      (put-text-property (point) (pos-eol) 'wrap-prefix
                         (mail-thread-wrap-prefix guides (get-text-property (point) 'wrap-prefix))))
    (forward-line))
  `(jit-lock-bounds ,start . ,end))

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
  (let ((start (point)))
    (insert-before-markers
     (or (mail-thread-render mail-thread-group
                             (mail-thread-message-article message))
         "[the store no longer holds this message]")
     "\n\n")
    (mail-thread-indent message start (point))))

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
  "Mark ARTICLE in the summary the way Gnus marks an article it displays."
  (when (buffer-live-p mail-thread-summary-buffer)
    (with-current-buffer mail-thread-summary-buffer
      (save-excursion
        ;; the hook reads the mark on the line at point
        (when (gnus-summary-goto-subject article nil t)
          (let ((gnus-current-article article))
            (run-hooks 'gnus-mark-article-hook)))))))

;;; Filtering by a notmuch query

(defun mail-thread-hidden-p (message)
  "Non-nil when the filter hides MESSAGE."
  (seq-some (lambda (overlay) (overlay-get overlay 'mail-thread-hidden))
            (overlays-at (mail-thread-message-marker message))))

(defun mail-thread-shown-messages ()
  "Messages the filter leaves on show, in reply order."
  (seq-remove #'mail-thread-hidden-p mail-thread-messages))

(defun mail-thread-message-id (message)
  "MESSAGE's Message-ID without its angle brackets."
  (string-trim (mail-header-id (mail-thread-message-header message)) "<" ">"))

(defun mail-thread-filter-args (query)
  "Arguments for notmuch to list which of the thread's messages match QUERY."
  ;; an unquoted id holding + or = matches other messages
  (list (format "--config=%s" gnus-search-notmuch-config-file)
        "search" "--output=messages"
        (format "(%s)" (mapconcat (lambda (message)
                                    (format "id:\"%s\"" (string-replace
                                                         "\"" "\"\""
                                                         (mail-thread-message-id message))))
                                  mail-thread-messages " or "))
        "and" (format "(%s)" query)))

;;;###autoload
(defun mail-thread-draw-labels ()
  "Show each message's labels on its line."
  (let ((tables (mail-label-tables))
        (pos (point-min)))
    (with-silent-modifications
      (while (setq pos (text-property-not-all pos (point-max) 'mail-labels nil))
        (let ((chips (mail-label-chips-of (list (get-text-property pos 'mail-labels)) tables)))
          (put-text-property pos (1+ pos) 'display (if chips (concat "  " chips) "")))
        (setq pos (1+ pos))))))

;;;###autoload
(defun mail-thread-update-header-line ()
  "Show the thread's subject, its labels, and how many messages are on show."
  (let* ((subject (mail-thread-subject
                   (mail-thread-message-header (car mail-thread-messages))))
         (chips (mail-label-chips-of
                 (mapcar (lambda (message) (mail-header-id (mail-thread-message-header message)))
                         mail-thread-messages)))
         (title (if chips (concat subject "  " chips) subject))
         (total (length mail-thread-messages)))
    (setq-local header-line-format
                (string-replace
                 "%" "%%"
                 (if mail-thread-filter-query
                     (format "%s   %d of %d shown: %s" title
                             (length (mail-thread-shown-messages)) total
                             mail-thread-filter-query)
                   (format "%s   %d messages" title total))))))

(defun mail-thread-show-all ()
  "Show every message of the thread again."
  (remove-overlays (point-min) (point-max) 'mail-thread-hidden t)
  (setq mail-thread-filter-query nil)
  (mail-thread-update-header-line))

(defun mail-thread-hide-unmatched (query ids)
  "Hide the messages whose Message-ID is not among IDS, notmuch's answer to QUERY.
When none of them matches, every message stays.  Point on a message
that goes moves to the next one shown, or the last."
  (if (not (seq-some (lambda (message) (member (mail-thread-message-id message) ids))
                     mail-thread-messages))
      (message "Nothing in this thread matches %s" query)
    (mail-thread-show-all)
    (let ((here (seq-find (lambda (message) (<= (mail-thread-message-marker message) (point)))
                          (reverse mail-thread-messages))))
      (dolist (message mail-thread-messages)
        (unless (member (mail-thread-message-id message) ids)
          (let ((overlay (make-overlay (mail-thread-message-marker message)
                                       (mail-thread-body-end message))))
            (overlay-put overlay 'invisible t)
            (overlay-put overlay 'mail-thread-hidden t))))
      (setq mail-thread-filter-query query)
      (mail-thread-update-header-line)
      (when (and here (mail-thread-hidden-p here))
        (mail-thread-goto-message
         (or (seq-find (lambda (message) (not (mail-thread-hidden-p message)))
                       (cdr (memq here mail-thread-messages)))
             (car (last (mail-thread-shown-messages)))))))))

(defun mail-thread-filter-answered (thread process query)
  "Apply PROCESS's answer to QUERY in THREAD, unless a newer filter replaced it."
  (when (and (buffer-live-p thread)
             (eq process (buffer-local-value 'mail-thread-filter-process thread)))
    (let ((ids (with-current-buffer (process-buffer process)
                 (goto-char (point-min))
                 (let (ids)
                   (while (re-search-forward "^id:\\(.+\\)$" nil t)
                     (push (match-string 1) ids))
                   ids))))
      ;; the thread's own window, so point and the recentering land there
      (with-selected-window (or (get-buffer-window thread) (selected-window))
        (with-current-buffer thread
          (setq mail-thread-filter-process nil)
          (if (and (eq (process-status process) 'exit)
                   (eql 0 (process-exit-status process)))
              (mail-thread-hide-unmatched query ids)
            (message "notmuch could not filter the thread")))))))

(defun mail-thread-start-filter (query)
  "Ask notmuch which of the thread's messages QUERY matches."
  (let ((thread (current-buffer)))
    (setq mail-thread-filter-process
          (make-process
           :name "mail-thread-filter"
           :buffer (generate-new-buffer " *mail thread filter*")
           :noquery t
           :command (cons gnus-search-notmuch-program (mail-thread-filter-args query))
           :sentinel (lambda (process _event)
                       (unless (process-live-p process)
                         (unwind-protect
                             (mail-thread-filter-answered thread process query)
                           (kill-buffer (process-buffer process)))))))))

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
Messages an unfold rendered meanwhile leave the queue, and the ones the
filter hides wait until it goes, since a render marks them read."
  (setq mail-thread-waiting (seq-remove #'mail-thread-rendered-p mail-thread-waiting))
  (let ((here (mail-thread-message-at-point)))
    (if (memq here mail-thread-waiting)
        here
      (seq-find (lambda (message) (not (mail-thread-hidden-p message)))
                mail-thread-waiting))))

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
  "Message point is inside; in a hidden one, the shown message above it.
The cursor of a point in hidden text shows at the end of that message."
  (let ((shown (mail-thread-shown-messages))
        found)
    (dolist (message shown (or found (car shown)))
      (when (<= (mail-thread-message-marker message) (point))
        (setq found message)))))

(defun mail-thread-goto-message (message)
  "Put point on MESSAGE's line, at the top of the window."
  (goto-char (mail-thread-message-marker message))
  (when (get-buffer-window) (recenter 0)))

(defun mail-thread-move (count)
  "Move COUNT shown messages forward, or backward when COUNT is negative."
  (let* ((shown (mail-thread-shown-messages))
         (index (+ (seq-position shown (mail-thread-message-at-point)) count)))
    (cond ((< index 0) (user-error "First message"))
          ((<= (length shown) index) (user-error "Last message"))
          (t (mail-thread-goto-message (nth index shown))))))

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

(defun mail-thread-unfold-all ()
  "Unfold every message on show; the fill renders the ones never rendered."
  (interactive nil mail-thread-mode)
  (let* ((here (mail-thread-message-at-point))
         (shown (mail-thread-shown-messages)))
    (dolist (message shown)
      (when (mail-thread-fold-overlay message)
        (mail-thread-expand message)))
    (setq mail-thread-waiting
          (seq-filter (lambda (message)
                        (and (memq message shown) (not (mail-thread-rendered-p message))))
                      (cons here (mail-thread-fill-order here))))
    (unless mail-thread-fill-timer
      (mail-thread-start-fill))))

(defun mail-thread-fold-all ()
  "Fold every message and stop the fill.
Point moves to the line of the message it was in."
  (interactive nil mail-thread-mode)
  (let ((here (mail-thread-message-at-point)))
    (mail-thread-stop-fill)
    (dolist (message mail-thread-messages)
      (when (mail-thread-message-open-p message)
        (mail-thread-collapse message)))
    (goto-char (mail-thread-message-marker here))))

(defun mail-thread-filter (query)
  "Show only the messages of this thread that notmuch QUERY matches.
An empty QUERY shows every message again.  notmuch answers in the
background, and the filter lands when it does."
  (interactive (list (read-string "Filter thread (empty for all): ")) mail-thread-mode)
  (when (process-live-p mail-thread-filter-process)
    (delete-process mail-thread-filter-process))
  (setq mail-thread-filter-process nil)
  (if (string-blank-p query)
      (mail-thread-show-all)
    (mail-thread-start-filter query)))

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
  ;; HTML paragraphs come unfilled and wrap at the column, whose right
  ;; margin takes the rest of the window
  (visual-line-mode)
  (visual-wrap-prefix-mode)
  (setq-local visual-fill-column-width
              (+ mail-thread-width
                 (string-width (or (mail-thread-guides mail-thread-max-indent) ""))))
  (visual-fill-column-mode)
  ;; after visual-wrap's own function, at depth 90
  (add-hook 'jit-lock-functions #'mail-thread-wrap-guides 95 t)
  (jit-lock-register #'mail-thread-wrap-guides)
  ;; a fill must not outlive the thread it was started for
  (add-hook 'kill-buffer-hook #'mail-thread-stop-fill nil t)
  (add-hook 'change-major-mode-hook #'mail-thread-stop-fill nil t))

(defun mail-thread-build (entries entry unfold)
  "Insert every message of ENTRIES in reply order, rendering ENTRY's.
ENTRY is the article the summary was on.  The articles of UNFOLD wait in
`mail-thread-waiting' for the fill, and the rest stay a line."
  (let* ((arranged (mail-thread-arrange entries))
         (subject (mail-thread-subject (cddar arranged)))
         (inhibit-read-only t)
         messages shown)
    (pcase-dolist (`(,depth ,article . ,header) arranged)
      (let ((message (mail-thread-message-create
                      :article article :header header :depth depth
                      :marker (point-marker))))
        (push message messages)
        (insert (mail-thread-message-line message subject (eql article entry)) "\n")
        (mail-thread-indent message (mail-thread-message-marker message) (point))
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
                                 (memq (mail-thread-message-article message) unfold))
                               (mail-thread-fill-order shown)))
    (mail-thread-draw-labels)
    (mail-thread-update-header-line)
    (set-buffer-modified-p nil)))

(defun mail-thread-unfolded (entries)
  "Articles of ENTRIES the view renders, asked in the summary they come from.
Every one under the group parameter `mail-thread-unfold-read', else the
unread ones."
  (if (gnus-group-find-parameter gnus-newsgroup-name 'mail-thread-unfold-read)
      (mapcar #'car entries)
    gnus-newsgroup-unreads))

;;;###autoload
(defun open-mail-thread ()
  "Read every message of the thread at point in one buffer.
The read ones stay folded unless the group parameter
`mail-thread-unfold-read' is set."
  (interactive nil gnus-summary-mode)
  (let* ((entry (or (gnus-summary-article-number)
                    (user-error "No article on this line")))
         (group gnus-newsgroup-name)
         (summary (current-buffer))
         (entries (or (mail-thread-entries)
                      (user-error "No article Gnus can fetch in this thread")))
         (unfold (mail-thread-unfolded entries))
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
      (mail-thread-build entries entry unfold)
      (mail-thread-start-fill))
    (let ((gnus-summary-buffer summary))
      (gnus-configure-windows 'mail-thread))
    (with-current-buffer buffer
      (when-let* ((message (seq-find (lambda (message)
                                       (eql (mail-thread-message-article message) entry))
                                     mail-thread-messages)))
        (mail-thread-goto-message message))
      (run-hooks 'mail-thread-open-hook))))

;;;###autoload
(defun mail-thread-message-ids ()
  "Message-IDs of the thread in this view, in reply order."
  (mapcar (lambda (message) (mail-header-id (mail-thread-message-header message)))
          mail-thread-messages))

(defun mail-thread-summary-thread (article)
  "Thread of ARTICLE in the view's summary as (ENTRIES . UNFOLD), or nil."
  (when (buffer-live-p mail-thread-summary-buffer)
    (with-current-buffer mail-thread-summary-buffer
      (save-excursion
        (when (gnus-summary-goto-subject article nil t)
          (when-let* ((entries (mail-thread-entries)))
            (cons entries (mail-thread-unfolded entries))))))))

;;;###autoload
(defun mail-thread-reload ()
  "Lay the thread out again from its summary, which has new messages of it.
Point stays in its message, on the same row of the window showing the
view, and a filter is asked again; folds the reader made are gone."
  (let ((window (get-buffer-window (current-buffer) t)))
    (if (and window (not (eq window (selected-window))))
        (with-selected-window window
          (mail-thread-reload))
      (when-let* ((here (mail-thread-message-at-point))
                  (article (mail-thread-message-article here))
                  (thread (mail-thread-summary-thread article)))
        (let ((offset (- (point) (mail-thread-message-marker here)))
              (row (when-let* ((window) (posn (posn-at-point)))
                     (cdr (posn-col-row posn))))
              (query mail-thread-filter-query))
          (mail-thread-stop-fill)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (remove-overlays))
          (setq mail-thread-filter-query nil)
          (mail-thread-build (car thread) article (cdr thread))
          (mail-thread-start-fill)
          (when-let* ((message (seq-find (lambda (message)
                                           (eql (mail-thread-message-article message) article))
                                         mail-thread-messages)))
            (goto-char (min (+ (mail-thread-message-marker message) offset)
                            (mail-thread-body-end message)))
            (when row
              (recenter row)))
          (when query
            (mail-thread-start-filter query)))))))

;;; Acting on the message on screen
;;
;; Gnus's reply and marking commands work in the summary only, so the
;; thread view and the article buffer run them there, with the summary's
;; point on the message they show.

;;;###autoload
(defun mail-on-screen ()
  "Summary and article of the message the current buffer shows.
That is the message at point in the summary and in the thread view, and
the article on display in the article buffer."
  (cond ((derived-mode-p 'mail-thread-mode)
         (cons mail-thread-summary-buffer
               (mail-thread-message-article (mail-thread-message-at-point))))
        ((derived-mode-p 'gnus-article-mode)
         (cons gnus-article-current-summary (cdr gnus-article-current)))
        (t (cons (current-buffer) (gnus-summary-article-number)))))

(defun mail-summary-call (article command)
  "Call COMMAND in the current summary on ARTICLE, ignoring its marks."
  (unless (gnus-summary-goto-subject article nil t)
    (user-error "The summary no longer shows this message"))
  (let ((gnus-newsgroup-processable nil))
    (call-interactively command)))

;;;###autoload
(defun run-in-mail-summary (command &optional stay)
  "Call COMMAND in the summary, on the message the current buffer shows.
With STAY the selected window stays selected; otherwise the summary's
window is selected first, so a reply leaves point in its message buffer."
  (pcase-let ((`(,summary . ,article) (mail-on-screen)))
    (cond ((eq summary (current-buffer))
           (call-interactively command))
          ((not (and summary (buffer-live-p (get-buffer summary))))
           (user-error "The summary of this message is gone"))
          ((not stay)
           (pop-to-buffer summary)
           (mail-summary-call article command))
          ((get-buffer-window summary)
           (with-selected-window (get-buffer-window summary)
             (mail-summary-call article command)))
          (t
           (with-current-buffer summary
             (mail-summary-call article command))))))

;;;###autoload
(defun reply-to-sender ()
  "Reply to the sender of the message at point, quoting it."
  (interactive nil gnus-summary-mode mail-thread-mode gnus-article-mode)
  (run-in-mail-summary #'gnus-summary-reply-with-original))

;;;###autoload
(defun reply-to-everyone ()
  "Reply to the sender and every recipient of the message at point, quoting it."
  (interactive nil gnus-summary-mode mail-thread-mode gnus-article-mode)
  (run-in-mail-summary #'gnus-summary-wide-reply-with-original))

;;;###autoload
(defun reply-to-list ()
  "Reply to the mailing list of the message at point only, quoting it."
  (interactive nil gnus-summary-mode mail-thread-mode gnus-article-mode)
  (run-in-mail-summary #'gnus-summary-reply-to-list-with-original))

;;;###autoload
(defun follow-up-on-newsgroup ()
  "Post a followup to the newsgroup of the message at point, quoting it."
  (interactive nil gnus-summary-mode mail-thread-mode gnus-article-mode)
  (run-in-mail-summary #'gnus-summary-followup-with-original))

;;;###autoload
(defun forward-mail ()
  "Forward the message at point."
  (interactive nil gnus-summary-mode mail-thread-mode gnus-article-mode)
  (run-in-mail-summary #'gnus-summary-mail-forward))

;;;###autoload
(defun compose-new-mail ()
  "Start a new message with the posting style of the group at hand.
Outside a summary, a thread or an article, that is the group buffer's."
  (interactive nil gnus-group-mode gnus-summary-mode mail-thread-mode gnus-article-mode
               mail-live-collect-mode)
  (if (derived-mode-p 'gnus-summary-mode 'mail-thread-mode 'gnus-article-mode)
      (run-in-mail-summary #'gnus-summary-mail-other-window)
    (unless (gnus-alive-p)
      (gnus))
    (with-current-buffer gnus-group-buffer
      (call-interactively #'gnus-group-mail))))

(provide 'mail-thread)
;;; thread.el ends here
