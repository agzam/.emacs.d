;;; modules/email/autoload/marks.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Deferred deletion and archive in the summary, the way Dired flags
;; files: one key queues the message at point, one takes it back, one
;; executes the whole queue.  The queue is a table of this module's own,
;; not a Gnus mark: every Gnus mark but unread, ticked and dormant counts
;; as read, so a queued message would reach Gmail as seen at summary exit
;; even when nothing was executed, and the process mark cannot tell
;; delete from archive.  Delete moves the file into the trash group,
;; which Gmail shows as Trash; archive deletes the file from the label
;; group at hand, which drops that label and keeps the All Mail copy.
;;; Code:

(require 'dired)
(require 'gnus)
(require 'gnus-sum)
(require 'seq)

(defvar mail-trash-group)

(defvar-local mail-marks nil
  "Queue of (ARTICLE . VERB) for this summary; VERB is `delete' or `archive'.")

;;; The summary column

(defun mail-mark-glyph (verb)
  "One-column glyph for a queued VERB, in dired's mark colours."
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

;;; Queueing

(defun mail-mark-redraw (article)
  "Redraw ARTICLE's summary line from `mail-marks'."
  (when (gnus-summary-goto-subject article nil t)
    (gnus-summary-show-thread)
    (gnus-summary-update-article-line article (gnus-summary-article-header article))))

(defun mail-mark-articles (articles verb)
  "Queue ARTICLES under VERB, or take them out of the queue when VERB is nil."
  (when (and (eq verb 'archive) (equal gnus-newsgroup-name mail-trash-group))
    (user-error "Archiving out of the trash deletes for good; move the message instead"))
  (save-excursion
    (dolist (article articles)
      (if verb
          (setf (alist-get article mail-marks) verb)
        (setf (alist-get article mail-marks nil t) nil))
      (mail-mark-redraw article)))
  (gnus-summary-position-point))

(defun thread-articles-at-point ()
  "Articles of the thread at point, the sparse placeholders left out."
  (seq-remove (lambda (article) (memq article gnus-newsgroup-sparse))
              (save-excursion
                (gnus-summary-top-thread)
                (gnus-summary-articles-in-thread))))

;;;###autoload
(defun mail-mark-for-deletion ()
  "Queue the message at point for the trash."
  (interactive nil gnus-summary-mode)
  (mail-mark-articles (list (gnus-summary-article-number)) 'delete))

;;;###autoload
(defun mail-mark-for-archive ()
  "Queue the message at point to leave this label."
  (interactive nil gnus-summary-mode)
  (mail-mark-articles (list (gnus-summary-article-number)) 'archive))

;;;###autoload
(defun mail-unmark ()
  "Take the message at point out of the queue."
  (interactive nil gnus-summary-mode)
  (mail-mark-articles (list (gnus-summary-article-number)) nil))

;;;###autoload
(defun mail-mark-thread-for-deletion ()
  "Queue every message of the thread at point for the trash."
  (interactive nil gnus-summary-mode)
  (mail-mark-articles (thread-articles-at-point) 'delete))

;;;###autoload
(defun mail-mark-thread-for-archive ()
  "Queue every message of the thread at point to leave this label."
  (interactive nil gnus-summary-mode)
  (mail-mark-articles (thread-articles-at-point) 'archive))

;;;###autoload
(defun mail-unmark-thread ()
  "Take every message of the thread at point out of the queue."
  (interactive nil gnus-summary-mode)
  (mail-mark-articles (thread-articles-at-point) nil))

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
