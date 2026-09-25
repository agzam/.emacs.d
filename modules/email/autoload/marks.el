;;; modules/email/autoload/marks.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Marking in the summary, the way Dired flags files: every mark command
;; acts on the message at point, or on each one the active region
;; touches, and moves point to the message below them.
;;
;; Deletion and archive are deferred: one key queues, one takes back,
;; one executes the whole queue.  The queue is a table of this module's
;; own, not a Gnus mark: every Gnus mark but unread, ticked and dormant
;; counts as read, so a queued message would reach Gmail as seen at
;; summary exit even when nothing was executed, and the process mark
;; cannot tell delete from archive.  Delete moves the file into the trash
;; group, which Gmail shows as Trash; archive deletes the file from the
;; label group at hand, which drops that label and keeps the All Mail
;; copy.  Read and star are Gnus's own marks, toggled independently.
;;; Code:

(require 'dired)
(require 'gnus)
(require 'gnus-sum)
(require 'seq)

(defvar mail-trash-group)
(defvar mail-archive-group)

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

;;; Queueing

(defun mail-mark-redraw (article)
  "Redraw ARTICLE's summary line from `mail-marks'."
  (when (gnus-summary-goto-subject article nil t)
    (gnus-summary-show-thread)
    (gnus-summary-update-article-line article (gnus-summary-article-header article))))

(defun mail-mark-articles (articles verb)
  "Queue ARTICLES under VERB, or take them out of the queue when VERB is nil."
  (when (eq verb 'archive)
    (cond ((equal gnus-newsgroup-name mail-trash-group)
           (user-error "Archiving out of the trash deletes for good; move the message instead"))
          ((equal gnus-newsgroup-name mail-archive-group)
           (user-error "All Mail is the archive already; delete the message or archive it from a label"))))
  (save-excursion
    (dolist (article articles)
      (if verb
          (setf (alist-get article mail-marks) verb)
        (setf (alist-get article mail-marks nil t) nil))
      (mail-mark-redraw article))))

(defun mail-queue (verb &optional whole-threads)
  "Queue the message at point, or the region's, under VERB; nil unqueues.
WHOLE-THREADS extends that to every message of their threads.  Point
moves to the message below."
  (let* ((covered (mail-articles-at-point-or-region))
         (articles (if whole-threads
                       (mail-whole-threads covered)
                     (mail-real-articles covered))))
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
  "Take the message at point, or the region's, out of the queue."
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
  "Take the thread at point, or each one in the region, out of the queue."
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
  "Run the queue: move the deletions to the trash, delete the archives' files.
Both Gnus commands act on the process mark, so the queue is lent to it
for the call; the lines they cancel are then limited out of view."
  (interactive nil gnus-summary-mode)
  (unless mail-marks
    (user-error "Nothing is marked"))
  (let ((deletes (mail-marked-articles 'delete))
        (archives (mail-marked-articles 'archive))
        ;; an active region would win over the process mark
        (mark-active nil)
        (gnus-newsgroup-process-stack nil))
    (when deletes
      (let ((gnus-newsgroup-processable deletes))
        (gnus-summary-move-article nil mail-trash-group)))
    (when archives
      (let ((gnus-newsgroup-processable archives))
        (gnus-summary-delete-article)))
    (setq mail-marks nil)
    (gnus-summary-limit-to-marks (list gnus-canceled-mark) 'reverse)
    (message "Trashed %d, archived %d" (length deletes) (length archives))))

;;; marks.el ends here
