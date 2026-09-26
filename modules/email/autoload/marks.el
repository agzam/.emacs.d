;;; modules/email/autoload/marks.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Marking in the summary, the way Dired flags files: every mark command
;; acts on the message at point, or on each one the active region
;; touches, and moves point to the message below them.
;;
;; Delete and archive are queued, then run by one key; a Gnus mark would
;; count as read and reach Gmail as seen.  Delete sends the message to
;; the trash, archive takes it out of the inbox and keeps its labels, and
;; either way its inbox copy goes at once, whatever this summary shows.
;;; Code:

(require 'dired)
(require 'gnus)
(require 'gnus-group)
(require 'gnus-sum)
(require 'nnselect)
(require 'seq)

(defvar mail-inbox-group)
(defvar mail-trash-group)

(defvar-local mail-marks nil
  "Queue of (ARTICLE . VERB) for this summary; VERB is `delete' or `archive'.")

;;; The summary column

(defun mail-mark-glyph (verb)
  "One-column glyph for a queued VERB, in Dired's mark colours."
  ;; line highlighting swaps the second face for the line's own where
  ;; `gnus-face' is set, and overwrites the property everywhere else
  (pcase verb
    ('delete (propertize "D" 'face (list 'dired-flagged 'default) 'gnus-face t))
    ('archive (propertize "A" 'face (list 'dired-marked 'default) 'gnus-face t))
    (_ " ")))

;;;###autoload
(defun gnus-user-format-function-D (header)
  "The `%uD' summary column: the verb queued on HEADER's article, if any."
  (mail-mark-glyph (alist-get (mail-header-number header) mail-marks)))

;;; What a command marks, and where point goes next

(defun mail-real-articles (articles)
  "ARTICLES without the sparse placeholders Gnus invents for missing parents."
  (seq-remove (lambda (article) (memq article gnus-newsgroup-sparse)) articles))

(defun mail-articles-at-point-or-region ()
  "Articles on every line the active region touches, else the one at point.
Reading the region deactivates it, which also ends evil's visual state."
  (if (not (use-region-p))
      (list (gnus-summary-article-number))
    (let ((end (region-end))
          articles)
      (save-excursion
        (goto-char (region-beginning))
        (while (progn (push (gnus-summary-article-number) articles)
                      (and (gnus-summary-find-next)
                           (< (line-beginning-position) end)))))
      (deactivate-mark)
      (nreverse articles))))

(defun thread-articles-at-point ()
  "Articles of the thread at point, the sparse placeholders left out."
  (mail-real-articles (save-excursion
                        (gnus-summary-top-thread)
                        (gnus-summary-articles-in-thread))))

(defun mail-whole-threads (articles)
  "Articles of every thread holding one of ARTICLES, in summary order."
  (seq-uniq (mapcan (lambda (article)
                      (save-excursion
                        (gnus-summary-goto-subject article)
                        (thread-articles-at-point)))
                    articles)))

(defun mail-move-below (articles)
  "Move point to the message below the last of ARTICLES."
  (gnus-summary-goto-subject (car (last articles)) nil t)
  (gnus-summary-next-subject 1))

;;; The copy in the inbox
;;
;; A message is one file per label it carries, and a search result is
;; whichever copy the search picked.

(defun mail-article-group (article)
  "Group whose file this summary's ARTICLE is."
  (if (gnus-nnselect-group-p gnus-newsgroup-name)
      (nnselect-article-group article)
    gnus-newsgroup-name))

(defun mail-article-number (article)
  "Number of this summary's ARTICLE in the group whose file it is."
  (if (gnus-nnselect-group-p gnus-newsgroup-name)
      (nnselect-article-number article)
    article))

(defun inbox-articles-by-id (message-ids)
  "Inbox articles of the messages with MESSAGE-IDS, as (MESSAGE-ID . ARTICLE)."
  (when message-ids
    ;; mail that arrived since the inbox was last read has no number yet
    (gnus-activate-group mail-inbox-group 'scan)
    (when (eq 'nov (gnus-retrieve-headers message-ids mail-inbox-group))
      (with-current-buffer nntp-server-buffer
        (goto-char (point-min))
        (let (found)
          (while (not (eobp))
            (let ((header (nnheader-parse-nov)))
              (push (cons (mail-header-id header) (mail-header-number header)) found))
            (forward-line 1))
          found)))))

(defun mail-inbox-copies (articles)
  "Inbox copies of this summary's ARTICLES, as (ARTICLE . INBOX-ARTICLE).
An article whose message is not in the inbox is left out."
  (if (equal gnus-newsgroup-name mail-inbox-group)
      (mapcar (lambda (article) (cons article article)) articles)
    (let* ((ids (mapcar (lambda (article)
                          (cons (mail-header-id (gnus-summary-article-header article))
                                article))
                        articles))
           (found (inbox-articles-by-id (mapcar #'car ids))))
      (seq-keep (lambda (id)
                  (when-let* ((inbox (cdr (assoc (car id) found))))
                    (cons (cdr id) inbox)))
                ids))))

(defun drop-gone-articles (group articles)
  "Take ARTICLES out of GROUP's open summary, when that is another buffer.
Their files went from this summary, and GROUP's lines for them would
point at nothing."
  (when-let* ((buffer (get-buffer (gnus-summary-buffer-name group)))
              ((not (eq buffer (current-buffer)))))
    (with-current-buffer buffer
      (save-excursion
        (dolist (article articles)
          (when (gnus-data-find article)
            (gnus-summary-mark-article article gnus-canceled-mark))))
      (gnus-summary-limit-to-marks (list gnus-canceled-mark) 'reverse))))

;;; Queueing

(defun mail-mark-redraw (article)
  "Redraw ARTICLE's summary line from `mail-marks'."
  (when (gnus-summary-goto-subject article nil t)
    (gnus-summary-show-thread)
    (gnus-summary-update-article-line article (gnus-summary-article-header article))))

(defun mail-mark-articles (articles verb)
  "Queue ARTICLES under VERB, or take them out of the queue when VERB is nil.
Only a message in the inbox can be archived."
  (when (eq verb 'archive)
    (let ((inbox (mapcar #'car (mail-inbox-copies articles))))
      (unless inbox
        (user-error "Not in the inbox"))
      (when (< (length inbox) (length articles))
        (message "%d not in the inbox, left alone" (- (length articles) (length inbox))))
      (setq articles inbox)))
  (save-excursion
    (dolist (article articles)
      (if verb
          (setf (alist-get article mail-marks) verb)
        (setf (alist-get article mail-marks nil t) nil))
      (mail-mark-redraw article))))

(defun mail-queue (verb &optional whole-threads)
  "Queue the message at point, or the region's, under VERB.
A nil VERB takes them out of the queue and marks them unread, the way
Gnus's own mark clearing does; a star stays.  WHOLE-THREADS extends
that to every message of their threads.  Point moves to the message
below."
  (let* ((covered (mail-articles-at-point-or-region))
         (articles (if whole-threads
                       (mail-whole-threads covered)
                     (mail-real-articles covered))))
    (unless verb
      (save-excursion
        (dolist (article articles)
          (mail-mark-keeping-star article gnus-unread-mark))))
    (mail-mark-articles articles verb)
    (mail-move-below (if whole-threads articles covered))))

;;;###autoload
(defun mail-mark-for-deletion ()
  "Queue the message at point, or the region's, for the trash."
  (interactive nil gnus-summary-mode)
  (mail-queue 'delete))

;;;###autoload
(defun mail-mark-for-archive ()
  "Queue the message at point, or the region's, to leave this label."
  (interactive nil gnus-summary-mode)
  (mail-queue 'archive))

;;;###autoload
(defun mail-unmark ()
  "Mark the message at point, or the region's, unread and out of the queue.
A star stays."
  (interactive nil gnus-summary-mode)
  (mail-queue nil))

;;;###autoload
(defun mail-mark-thread-for-deletion ()
  "Queue the thread at point, or each one in the region, for the trash."
  (interactive nil gnus-summary-mode)
  (mail-queue 'delete t))

;;;###autoload
(defun mail-mark-thread-for-archive ()
  "Queue the thread at point, or each one in the region, to leave this label."
  (interactive nil gnus-summary-mode)
  (mail-queue 'archive t))

;;;###autoload
(defun mail-unmark-thread ()
  "Mark the thread at point, or each in the region, unread and out of the queue.
Stars stay."
  (interactive nil gnus-summary-mode)
  (mail-queue nil t))

;;; Read and star
;;
;; An article in both the unread and the tick list is starred and
;; unread, and nnmaildir saves it that way.  Gnus's own commands never
;; make one: its tick counts as read.

(defun mail-star-glyph ()
  "One-column star in the colour of Gnus's starred lines."
  ;; the buffer font has U+2217; a star from a fallback font breaks the
  ;; columns
  (propertize "\u2217" 'face (list 'gnus-summary-normal-ticked 'default) 'gnus-face t))

;;;###autoload
(defun gnus-user-format-function-S (header)
  "The `%uS' summary column: a star if HEADER's article is starred.
Gnus's own `%U' column shows the tick on a read message only."
  (if (memq (mail-header-number header) gnus-newsgroup-marked)
      (mail-star-glyph)
    " "))

(defun mail-mark-keeping-star (article mark)
  "Give ARTICLE the read or unread MARK without dropping its star.
Marked read, a starred message becomes Gnus's tick; marked unread, it
goes back into the tick list too."
  (let ((starred (memq article gnus-newsgroup-marked))
        (unread (= mark gnus-unread-mark)))
    (gnus-summary-mark-article article
                               (if (and starred (not unread)) gnus-ticked-mark mark)
                               gnus-inhibit-user-auto-expire)
    (when (and starred unread)
      (setq gnus-newsgroup-marked
            (gnus-add-to-sorted-list gnus-newsgroup-marked article)))))

(defun mail-set-star (article star)
  "Star ARTICLE when STAR is non-nil, else unstar it; it stays read or unread.
Gnus's tick would make an unread message read, so an unread one only
joins or leaves the tick list."
  (if (memq article gnus-newsgroup-unreads)
      (setq gnus-newsgroup-marked
            (if star
                (gnus-add-to-sorted-list gnus-newsgroup-marked article)
              (delq article gnus-newsgroup-marked)))
    (gnus-summary-mark-article article (if star gnus-ticked-mark gnus-del-mark)
                               gnus-inhibit-user-auto-expire))
  (mail-mark-redraw article))

;;;###autoload
(defun mail-keep-star-on-read-h ()
  "Mark a starred article read as it is displayed, keeping its star.
Runs ahead of `gnus-summary-mark-read-and-unread-as-read', which sees
the unread mark on a starred unread article and drops the star."
  (when (memq gnus-current-article gnus-newsgroup-marked)
    (mail-mark-keeping-star gnus-current-article gnus-read-mark)))

;;;###autoload
(defun mail-toggle-read ()
  "Mark the message at point, or the region's, read; unread if all are read.
A star stays either way."
  (interactive nil gnus-summary-mode)
  (let* ((covered (mail-articles-at-point-or-region))
         (articles (mail-real-articles covered))
         (mark (if (seq-intersection articles gnus-newsgroup-unreads)
                   gnus-del-mark
                 gnus-unread-mark)))
    (save-excursion
      (dolist (article articles)
        (mail-mark-keeping-star article mark)))
    (mail-move-below covered)))

;;;###autoload
(defun mail-mark-thread-read ()
  "Mark the thread at point, or each one in the region, read.
Stars stay, which Gnus's own `gnus-summary-kill-thread' drops.  Point
moves below the threads."
  (interactive nil gnus-summary-mode)
  (let ((articles (mail-whole-threads (mail-articles-at-point-or-region))))
    (save-excursion
      (dolist (article articles)
        (mail-mark-keeping-star article gnus-del-mark)))
    (mail-move-below articles)))

;;;###autoload
(defun mail-toggle-star ()
  "Star the message at point, or the region's; unstar if all are starred.
Read and unread stay as they were."
  (interactive nil gnus-summary-mode)
  (let* ((covered (mail-articles-at-point-or-region))
         (articles (mail-real-articles covered))
         (star (seq-difference articles gnus-newsgroup-marked)))
    (save-excursion
      (dolist (article articles)
        (mail-set-star article star)))
    (mail-move-below covered)))

;;; Executing

(defun mail-marked-articles (verb)
  "Articles queued for VERB, lowest number first."
  (sort (mapcar #'car (seq-filter (lambda (mark) (eq (cdr mark) verb)) mail-marks))
        #'<))

;;;###autoload
(defun mail-execute-marks ()
  "Run the queue: deletions go to the trash, archives leave the inbox.
A message's inbox copy goes either way, whichever copy this summary
shows, and an archive keeps every label.  Gnus's commands act on the
process mark, so the queue is lent to it; cancelled lines leave the view."
  (interactive nil gnus-summary-mode)
  (unless mail-marks
    (user-error "Nothing is marked"))
  (let* ((deletes (mail-marked-articles 'delete))
         (archives (mail-marked-articles 'archive))
         (inbox-p (lambda (article)
                    (equal (mail-article-group article) mail-inbox-group)))
         ;; archives whose file here is the inbox's own
         (here (seq-filter inbox-p archives))
         ;; inbox copies this summary does not show
         (elsewhere (sort (seq-keep (lambda (copy)
                                      (unless (funcall inbox-p (car copy))
                                        (cdr copy)))
                                    (mail-inbox-copies (append deletes archives)))
                          #'<))
         (gone (mapcar (lambda (article)
                         (cons (mail-article-group article) (mail-article-number article)))
                       (append deletes here)))
         ;; an active region would win over the process mark
         (mark-active nil)
         (gnus-newsgroup-process-stack nil))
    (when deletes
      (let ((gnus-newsgroup-processable deletes))
        (gnus-summary-move-article nil mail-trash-group)))
    (when here
      (let ((gnus-newsgroup-processable here))
        (gnus-summary-delete-article)))
    (when elsewhere
      (gnus-request-expire-articles elsewhere mail-inbox-group t)
      (refresh-mail-group mail-inbox-group)
      (gnus-group-update-group mail-inbox-group t))
    (pcase-dolist (`(,group . ,copies)
                   (seq-group-by #'car (append gone
                                               (mapcar (lambda (article)
                                                         (cons mail-inbox-group article))
                                                       elsewhere))))
      (drop-gone-articles group (mapcar #'cdr copies)))
    (setq mail-marks nil)
    ;; an archive whose file stays here loses only its A
    (save-excursion
      (mapc #'mail-mark-redraw (seq-difference archives here)))
    (gnus-summary-limit-to-marks (list gnus-canceled-mark) 'reverse)
    (message "Trashed %d, archived %d" (length deletes) (length archives))))

(defun mail-queue-description ()
  "The queue in words, such as \"2 deletions and 1 archive\"."
  (string-join
   (seq-keep (pcase-lambda (`(,verb . ,noun))
               (let ((count (length (mail-marked-articles verb))))
                 (when (< 0 count)
                   (format "%d %s%s" count noun (if (= count 1) "" "s")))))
             '((delete . "deletion") (archive . "archive")))
   " and "))

;;;###autoload
(defun quit-mail-summary ()
  "Leave the summary; with messages queued, ask whether to run them first.
No drops the queue, and \\[keyboard-quit] stays in the summary."
  (interactive nil gnus-summary-mode)
  (when (and mail-marks
             (y-or-n-p (format "Run %s first? " (mail-queue-description))))
    (mail-execute-marks))
  (gnus-summary-exit))

;;; A search and the summaries open under it

(defvar-local mail-entry-marks nil
  "Unread and starred articles of a search summary as it opened, a cons.")

;;;###autoload
(defun note-search-entry-marks-h ()
  "Note which articles of a search summary are unread and starred as it opens."
  (when (gnus-nnselect-group-p gnus-newsgroup-name)
    (setq mail-entry-marks (cons (copy-sequence gnus-newsgroup-unreads)
                                 (copy-sequence gnus-newsgroup-marked)))))

(defun mail-set-read-and-star (article unread starred)
  "Make this summary's ARTICLE read unless UNREAD, and starred when STARRED."
  (when (gnus-data-find article)
    (unless (eq unread (and (memq article gnus-newsgroup-unreads) t))
      (mail-mark-keeping-star article (if unread gnus-unread-mark gnus-del-mark)))
    (unless (eq starred (and (memq article gnus-newsgroup-marked) t))
      (mail-set-star article starred))))

;;;###autoload
(defun carry-search-marks-h ()
  "Give the summaries open under this search what it changed in read and star.
The search writes its changes to the groups as it closes, and a summary
left open would write its own older state back when it exits."
  (when (and mail-entry-marks (not gnus-group-is-exiting-without-update-p))
    (let (changes)
      (dolist (article gnus-newsgroup-articles)
        (let ((unread (and (memq article gnus-newsgroup-unreads) t))
              (starred (and (memq article gnus-newsgroup-marked) t)))
          (unless (and (eq unread (and (memq article (car mail-entry-marks)) t))
                       (eq starred (and (memq article (cdr mail-entry-marks)) t)))
            (push (list (mail-article-group article) (mail-article-number article)
                        unread starred)
                  changes))))
      (pcase-dolist (`(,group ,article ,unread ,starred) changes)
        (when-let* ((buffer (get-buffer (gnus-summary-buffer-name group))))
          (with-current-buffer buffer
            (save-excursion
              (mail-set-read-and-star article unread starred))))))))

;;; marks.el ends here
