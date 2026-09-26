;;; tests/email/marks-tests.el --- summary marking specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defvar mail-trash-group "nnmaildir+gmail:trash")
(defvar mail-archive-group "nnmaildir+gmail:archive")

(load-module-file "modules/email/autoload/marks.el")

(defun marks-tests-header (number)
  "Header of the article NUMBER."
  (make-full-mail-header number "subject" "ann@example.com"
                         "Mon, 21 Sep 2026 10:00:00 +0000"
                         (format "<%d@x>" number) "" 0 0))

(defvar marks-tests-redrawn nil
  "Articles whose line was redrawn, newest first.")

(defvar marks-tests-marked nil
  "(ARTICLE . MARK) pairs Gnus was asked to set, newest first.")

(defun marks-tests-insert-lines (lines)
  "Insert a summary line and its Gnus data for each (ARTICLE LEVEL) in LINES."
  (dolist (line lines)
    (pcase-let ((`(,article ,level) line))
      (push (gnus-data-make article gnus-read-mark (1+ (point)) nil level)
            gnus-newsgroup-data)
      (insert (propertize (format "%s%d\n" (make-string (* 2 level) ?\s) article)
                          'gnus-number article))))
  (setq gnus-newsgroup-data (nreverse gnus-newsgroup-data))
  (goto-char (point-min)))

(defun marks-tests-set-mark (article mark &rest _)
  "Log MARK on ARTICLE and change the lists the way Gnus's marking does.
The tick and the unread mark each take the article out of the other's
list; every other mark takes it out of both."
  (push (cons article mark) marks-tests-marked)
  (setq gnus-newsgroup-unreads (delq article gnus-newsgroup-unreads)
        gnus-newsgroup-marked (delq article gnus-newsgroup-marked))
  (cond ((= mark gnus-ticked-mark)
         (setq gnus-newsgroup-marked (gnus-add-to-sorted-list gnus-newsgroup-marked article)))
        ((= mark gnus-unread-mark)
         (setq gnus-newsgroup-unreads (gnus-add-to-sorted-list gnus-newsgroup-unreads article)))))

(defmacro marks-tests-in-summary (lines &rest body)
  "Run BODY in a stand-in summary of LINES, point on the first.
Each line is (ARTICLE LEVEL).  Gnus finds lines, threads and the next
message through its own data; only the redraw of a line and the mark
Gnus sets are stubbed, and logged.  The mark stub keeps the unread and
tick lists the way Gnus does."
  (declare (indent 1))
  `(with-temp-buffer
     (setq marks-tests-redrawn nil
           marks-tests-marked nil)
     (let ((gnus-newsgroup-name "nnmaildir+gmail:inbox")
           (gnus-newsgroup-data nil)
           (gnus-newsgroup-data-reverse nil)
           (gnus-newsgroup-sparse nil)
           (gnus-newsgroup-unreads nil)
           (gnus-newsgroup-marked nil)
           (transient-mark-mode t))
       (marks-tests-insert-lines ,lines)
       (cl-letf (((symbol-function 'gnus-summary-recenter) #'ignore)
                 ((symbol-function 'mail-mark-redraw)
                  (lambda (article) (push article marks-tests-redrawn)))
                 ((symbol-function 'gnus-summary-mark-article) #'marks-tests-set-mark))
         ,@body))))

(defun marks-tests-select (from to)
  "Select the lines of articles FROM to TO the way evil's linewise selection does.
Evil stretches the region from the start of the first line to the start
of the line after the last before a command runs."
  (gnus-summary-goto-subject from)
  (set-mark (line-beginning-position))
  (gnus-summary-goto-subject to)
  (forward-line 1))

(describe "gnus-user-format-function-D"
  (it "draws D for a queued deletion, A for a queued archive, a space otherwise"
    (with-temp-buffer
      (setq-local mail-marks '((1 . delete) (2 . archive)))
      (expect (mapcar (lambda (n) (substring-no-properties
                                   (gnus-user-format-function-D (marks-tests-header n))))
                      '(1 2 3))
              :to-equal '("D" "A" " "))))
  (it "colours the glyph the way line highlighting preserves"
    ;; the highlight swaps the second face of a `gnus-face' run for the
    ;; line's face; a plain face property would be overwritten
    (with-temp-buffer
      (setq-local mail-marks '((1 . delete)))
      (let ((glyph (gnus-user-format-function-D (marks-tests-header 1))))
        (expect (get-text-property 0 'gnus-face glyph) :to-be t)
        (expect (get-text-property 0 'face glyph) :to-equal '(dired-flagged default)))))
  (it "reads the queue of the buffer it draws in"
    (with-temp-buffer
      (expect (gnus-user-format-function-D (marks-tests-header 1)) :to-equal " "))))

(describe "gnus-user-format-function-S"
  (it "draws a star on a starred message, read or unread, and a space on any other"
    ;; Gnus's own column draws the tick on a read message only
    (let ((gnus-newsgroup-marked (list 1 2))
          (gnus-newsgroup-unreads (list 2 3)))
      (expect (mapcar (lambda (n) (substring-no-properties
                                   (gnus-user-format-function-S (marks-tests-header n))))
                      '(1 2 3))
              :to-equal (list (string #x2217) (string #x2217) " "))))
  (it "colours the star the way line highlighting preserves"
    (let* ((gnus-newsgroup-marked (list 1))
           (glyph (gnus-user-format-function-S (marks-tests-header 1))))
      (expect (get-text-property 0 'gnus-face glyph) :to-be t)
      (expect (get-text-property 0 'face glyph)
              :to-equal '(gnus-summary-normal-ticked default)))))

(describe "mail-mark-keeping-star"
  (it "gives an unstarred message the mark it is asked for"
    (marks-tests-in-summary '((1 0))
      (setq gnus-newsgroup-unreads (list 1))
      (mail-mark-keeping-star 1 gnus-read-mark)
      (expect marks-tests-marked :to-equal `((1 . ,gnus-read-mark)))
      (expect gnus-newsgroup-unreads :to-be nil)))
  (it "marks a starred unread message read by ticking it, so the star stays"
    (marks-tests-in-summary '((1 0))
      (setq gnus-newsgroup-unreads (list 1)
            gnus-newsgroup-marked (list 1))
      (mail-mark-keeping-star 1 gnus-read-mark)
      (expect marks-tests-marked :to-equal `((1 . ,gnus-ticked-mark)))
      (expect gnus-newsgroup-unreads :to-be nil)
      (expect gnus-newsgroup-marked :to-equal '(1))))
  (it "puts a starred message marked unread back into the tick list"
    ;; Gnus's unread mark takes it out; nnmaildir saves both as a
    ;; flagged unread file
    (marks-tests-in-summary '((1 0))
      (setq gnus-newsgroup-marked (list 1))
      (mail-mark-keeping-star 1 gnus-unread-mark)
      (expect gnus-newsgroup-unreads :to-equal '(1))
      (expect gnus-newsgroup-marked :to-equal '(1)))))

(describe "mail-keep-star-on-read-h"
  (it "ticks a starred unread article as Gnus displays it, which Gnus's own function then skips"
    (marks-tests-in-summary '((1 0))
      (setq gnus-newsgroup-unreads (list 1)
            gnus-newsgroup-marked (list 1))
      (let ((gnus-current-article 1))
        (mail-keep-star-on-read-h))
      (expect marks-tests-marked :to-equal `((1 . ,gnus-ticked-mark)))
      (expect gnus-newsgroup-marked :to-equal '(1))))
  (it "leaves an unstarred article to Gnus's own hook"
    (marks-tests-in-summary '((1 0))
      (setq gnus-newsgroup-unreads (list 1))
      (let ((gnus-current-article 1))
        (mail-keep-star-on-read-h))
      (expect marks-tests-marked :to-be nil))))

(describe "mail-articles-at-point-or-region"
  (it "answers the article at point when no region is active"
    (marks-tests-in-summary '((1 0) (2 0) (3 0))
      (gnus-summary-goto-subject 2)
      (expect (mail-articles-at-point-or-region) :to-equal '(2))))
  (it "answers every line a linewise selection covers, and not the line after it"
    (marks-tests-in-summary '((1 0) (2 0) (3 0) (4 0))
      (marks-tests-select 2 3)
      (expect (mail-articles-at-point-or-region) :to-equal '(2 3))))
  (it "counts a line the region only reaches into"
    ;; a characterwise selection starts and ends inside lines
    (marks-tests-in-summary '((1 0) (2 0) (3 0) (4 0))
      (gnus-summary-goto-subject 2)
      (forward-char 1)
      (set-mark (point))
      (gnus-summary-goto-subject 3)
      (forward-char 1)
      (expect (mail-articles-at-point-or-region) :to-equal '(2 3))))
  (it "reaches the last line of the summary"
    (marks-tests-in-summary '((1 0) (2 0))
      (marks-tests-select 1 2)
      (expect (mail-articles-at-point-or-region) :to-equal '(1 2))))
  (it "deactivates the region, which is what ends evil's visual state"
    (marks-tests-in-summary '((1 0) (2 0))
      (marks-tests-select 1 2)
      (mail-articles-at-point-or-region)
      (expect mark-active :to-be nil))))

(describe "mail-whole-threads"
  (it "answers every article of each thread the articles sit in, once, in summary order"
    (marks-tests-in-summary '((1 0) (2 1) (3 2) (4 0) (5 0) (6 1))
      (expect (mail-whole-threads '(3 2 6)) :to-equal '(1 2 3 5 6))))
  (it "leaves out the sparse placeholders Gnus invents for missing parents"
    (marks-tests-in-summary '((-1 0) (2 1) (3 1))
      (let ((gnus-newsgroup-sparse '(-1)))
        (expect (mail-whole-threads '(3)) :to-equal '(2 3))))))

(describe "mail-move-below"
  (it "moves to the message below the last of the articles"
    (marks-tests-in-summary '((1 0) (2 0) (3 0))
      (mail-move-below '(1 2))
      (expect (gnus-summary-article-number) :to-be 3)))
  (it "stays on the last message when nothing is below it"
    (marks-tests-in-summary '((1 0) (2 0))
      (mail-move-below '(2))
      (expect (gnus-summary-article-number) :to-be 2))))

(describe "mail-mark-for-deletion and mail-mark-for-archive"
  (it "queue the message at point, redraw its line and move to the next message"
    (marks-tests-in-summary '((7 0) (8 0))
      (mail-mark-for-deletion)
      (expect mail-marks :to-equal '((7 . delete)))
      (expect marks-tests-redrawn :to-equal '(7))
      (expect (gnus-summary-article-number) :to-be 8)))
  (it "replace one verb with the other instead of queueing twice"
    (marks-tests-in-summary '((7 0) (8 0))
      (mail-mark-for-deletion)
      (gnus-summary-goto-subject 7)
      (mail-mark-for-archive)
      (expect mail-marks :to-equal '((7 . archive)))))
  (it "leave the rest of the queue alone"
    (marks-tests-in-summary '((7 0))
      (setq mail-marks (list (cons 3 'archive)))
      (mail-mark-for-deletion)
      (expect mail-marks :to-have-same-items-as '((3 . archive) (7 . delete)))))
  (it "queue every message a selection covers and land below it"
    (marks-tests-in-summary '((1 0) (2 0) (3 0) (4 0))
      (marks-tests-select 2 3)
      (mail-mark-for-archive)
      (expect mail-marks :to-have-same-items-as '((2 . archive) (3 . archive)))
      (expect (gnus-summary-article-number) :to-be 4)
      (expect mark-active :to-be nil)))
  (it "skip a sparse placeholder a selection covers"
    (marks-tests-in-summary '((-1 0) (2 1) (3 0))
      (let ((gnus-newsgroup-sparse '(-1)))
        (marks-tests-select -1 2)
        (mail-mark-for-deletion))
      (expect mail-marks :to-equal '((2 . delete)))
      (expect (gnus-summary-article-number) :to-be 3)))
  (it "refuse to archive out of the trash, where dropping the file deletes for good"
    (marks-tests-in-summary '((7 0) (8 0))
      (let ((gnus-newsgroup-name mail-trash-group))
        (expect (mail-mark-for-archive) :to-throw 'user-error)
        (expect mail-marks :to-be nil)
        (mail-mark-for-deletion)
        (expect mail-marks :to-equal '((7 . delete))))))
  (it "refuse to archive out of All Mail, which holds every message already"
    ;; what Gmail does with a message expunged from All Mail is unverified
    (marks-tests-in-summary '((7 0) (8 1) (9 0))
      (let ((gnus-newsgroup-name mail-archive-group))
        (expect (mail-mark-for-archive) :to-throw 'user-error)
        (expect (mail-mark-thread-for-archive) :to-throw 'user-error)
        (expect mail-marks :to-be nil)
        (mail-mark-for-deletion)
        (expect mail-marks :to-equal '((7 . delete)))))))

(describe "mail-unmark"
  (it "takes the message at point out of the queue, marks it unread, redraws it and moves on"
    (marks-tests-in-summary '((7 0) (8 0))
      (setq mail-marks (list (cons 7 'delete) (cons 8 'archive)))
      (mail-unmark)
      (expect mail-marks :to-equal '((8 . archive)))
      (expect marks-tests-marked :to-equal `((7 . ,gnus-unread-mark)))
      (expect marks-tests-redrawn :to-equal '(7))
      (expect (gnus-summary-article-number) :to-be 8)))
  (it "marks an unqueued message unread and queues nothing"
    (marks-tests-in-summary '((7 0))
      (mail-unmark)
      (expect mail-marks :to-be nil)
      (expect gnus-newsgroup-unreads :to-equal '(7))))
  (it "keeps the star of a message it marks unread"
    (marks-tests-in-summary '((7 0))
      (setq gnus-newsgroup-marked (list 7))
      (mail-unmark)
      (expect gnus-newsgroup-unreads :to-equal '(7))
      (expect gnus-newsgroup-marked :to-equal '(7))))
  (it "takes every message a selection covers out of the queue and marks each unread"
    (marks-tests-in-summary '((1 0) (2 0) (3 0))
      (setq mail-marks (list (cons 1 'delete) (cons 2 'delete) (cons 3 'archive)))
      (marks-tests-select 1 2)
      (mail-unmark)
      (expect mail-marks :to-equal '((3 . archive)))
      (expect gnus-newsgroup-unreads :to-equal '(1 2)))))

(describe "the thread commands"
  (it "queue every article of the thread at point and move below the thread"
    (marks-tests-in-summary '((10 0) (11 1) (12 2) (13 0))
      (gnus-summary-goto-subject 11)
      (mail-mark-thread-for-deletion)
      (expect mail-marks :to-have-same-items-as '((10 . delete) (11 . delete) (12 . delete)))
      (expect marks-tests-redrawn :to-have-same-items-as '(10 11 12))
      (expect (gnus-summary-article-number) :to-be 13)))
  (it "skip the sparse placeholders Gnus invents for missing parents"
    (marks-tests-in-summary '((-1 0) (10 1) (11 1))
      (let ((gnus-newsgroup-sparse '(-1)))
        (gnus-summary-goto-subject 11)
        (mail-mark-thread-for-archive))
      (expect mail-marks :to-have-same-items-as '((10 . archive) (11 . archive)))))
  (it "take every thread a selection touches and move below the last"
    (marks-tests-in-summary '((1 0) (2 1) (3 0) (4 0) (5 1) (6 0))
      (marks-tests-select 2 4)
      (mail-mark-thread-for-archive)
      (expect (mapcar #'car mail-marks) :to-have-same-items-as '(1 2 3 4 5))
      (expect (gnus-summary-article-number) :to-be 6)))
  (it "unqueue the whole thread and mark each of its messages unread"
    (marks-tests-in-summary '((10 0) (11 1) (12 1) (20 0))
      (setq mail-marks (list (cons 10 'delete) (cons 12 'archive) (cons 20 'delete)))
      (gnus-summary-goto-subject 11)
      (mail-unmark-thread)
      (expect mail-marks :to-equal '((20 . delete)))
      (expect gnus-newsgroup-unreads :to-equal '(10 11 12))))
  (it "never mark a sparse placeholder unread, which Gnus refuses"
    (marks-tests-in-summary '((-1 0) (10 1) (11 1))
      (let ((gnus-newsgroup-sparse '(-1)))
        (gnus-summary-goto-subject 11)
        (mail-unmark-thread))
      (expect (mapcar #'car marks-tests-marked) :to-have-same-items-as '(10 11)))))

(describe "mail-toggle-read"
  (it "marks an unread message read and moves on"
    (marks-tests-in-summary '((1 0) (2 0))
      (setq gnus-newsgroup-unreads (list 1 2))
      (mail-toggle-read)
      (expect marks-tests-marked :to-equal `((1 . ,gnus-del-mark)))
      (expect (gnus-summary-article-number) :to-be 2)))
  (it "marks a read message unread"
    (marks-tests-in-summary '((1 0) (2 0))
      (mail-toggle-read)
      (expect marks-tests-marked :to-equal `((1 . ,gnus-unread-mark)))))
  (it "marks a selection read while any of it is unread, unread once all of it is read"
    ;; Gmail's toolbar offers the same choice for a selection
    (marks-tests-in-summary '((1 0) (2 0) (3 0))
      (setq gnus-newsgroup-unreads (list 2))
      (marks-tests-select 1 2)
      (mail-toggle-read)
      (expect marks-tests-marked :to-have-same-items-as
              `((1 . ,gnus-del-mark) (2 . ,gnus-del-mark)))
      (expect (gnus-summary-article-number) :to-be 3)
      (setq marks-tests-marked nil
            gnus-newsgroup-unreads nil)
      (marks-tests-select 1 2)
      (mail-toggle-read)
      (expect marks-tests-marked :to-have-same-items-as
              `((1 . ,gnus-unread-mark) (2 . ,gnus-unread-mark)))))
  (it "marks a starred read message unread and keeps its star"
    (marks-tests-in-summary '((1 0) (2 0))
      (setq gnus-newsgroup-marked (list 1))
      (mail-toggle-read)
      (expect gnus-newsgroup-unreads :to-equal '(1))
      (expect gnus-newsgroup-marked :to-equal '(1))
      (expect (gnus-summary-article-number) :to-be 2)))
  (it "marks a starred unread message read and keeps its star"
    (marks-tests-in-summary '((1 0) (2 0))
      (setq gnus-newsgroup-unreads (list 1)
            gnus-newsgroup-marked (list 1))
      (mail-toggle-read)
      (expect gnus-newsgroup-unreads :to-be nil)
      (expect gnus-newsgroup-marked :to-equal '(1))))
  (it "takes starred messages in a selection along with the rest"
    (marks-tests-in-summary '((1 0) (2 0) (3 0))
      (setq gnus-newsgroup-unreads (list 1 2)
            gnus-newsgroup-marked (list 2))
      (marks-tests-select 1 2)
      (mail-toggle-read)
      (expect gnus-newsgroup-unreads :to-be nil)
      (expect gnus-newsgroup-marked :to-equal '(2))
      (expect (gnus-summary-article-number) :to-be 3))))

(describe "mail-mark-thread-read"
  (it "marks every message of the thread at point read and moves below the thread"
    (marks-tests-in-summary '((10 0) (11 1) (12 2) (13 0))
      (setq gnus-newsgroup-unreads (list 10 11 12 13))
      (gnus-summary-goto-subject 11)
      (mail-mark-thread-read)
      (expect gnus-newsgroup-unreads :to-equal '(13))
      (expect marks-tests-marked :to-have-same-items-as
              `((10 . ,gnus-del-mark) (11 . ,gnus-del-mark) (12 . ,gnus-del-mark)))
      (expect (gnus-summary-article-number) :to-be 13)))
  (it "keeps the star of a starred unread message, which Gnus's own thread command drops"
    (marks-tests-in-summary '((10 0) (11 1) (13 0))
      (setq gnus-newsgroup-unreads (list 10 11)
            gnus-newsgroup-marked (list 11))
      (mail-mark-thread-read)
      (expect gnus-newsgroup-unreads :to-be nil)
      (expect gnus-newsgroup-marked :to-equal '(11))
      (expect (alist-get 11 marks-tests-marked) :to-equal gnus-ticked-mark)))
  (it "takes every thread a selection touches and moves below the last"
    (marks-tests-in-summary '((1 0) (2 1) (3 0) (4 0) (5 1) (6 0))
      (setq gnus-newsgroup-unreads (list 1 2 3 4 5 6))
      (marks-tests-select 2 4)
      (mail-mark-thread-read)
      (expect gnus-newsgroup-unreads :to-equal '(6))
      (expect (gnus-summary-article-number) :to-be 6)
      (expect mark-active :to-be nil)))
  (it "skips the sparse placeholders Gnus invents for missing parents"
    (marks-tests-in-summary '((-1 0) (10 1) (11 1))
      (let ((gnus-newsgroup-sparse '(-1)))
        (setq gnus-newsgroup-unreads (list 10 11))
        (gnus-summary-goto-subject 11)
        (mail-mark-thread-read))
      (expect (mapcar #'car marks-tests-marked) :to-have-same-items-as '(10 11)))))

(describe "mail-toggle-star"
  (it "stars the message at point and moves on"
    (marks-tests-in-summary '((1 0) (2 0))
      (mail-toggle-star)
      (expect marks-tests-marked :to-equal `((1 . ,gnus-ticked-mark)))
      (expect (gnus-summary-article-number) :to-be 2)))
  (it "stars a whole selection unless every message in it is starred"
    (marks-tests-in-summary '((1 0) (2 0) (3 0))
      (setq gnus-newsgroup-marked (list 1))
      (marks-tests-select 1 2)
      (mail-toggle-star)
      (expect marks-tests-marked :to-have-same-items-as
              `((1 . ,gnus-ticked-mark) (2 . ,gnus-ticked-mark)))))
  (it "unstars a selection whose messages are all starred, leaving them read"
    (marks-tests-in-summary '((1 0) (2 0) (3 0))
      (setq gnus-newsgroup-marked (list 1 2))
      (marks-tests-select 1 2)
      (mail-toggle-star)
      (expect marks-tests-marked :to-have-same-items-as
              `((1 . ,gnus-del-mark) (2 . ,gnus-del-mark)))
      (expect (gnus-summary-article-number) :to-be 3)))
  (it "stars an unread message and leaves it unread"
    ;; Gnus's tick would make it read
    (marks-tests-in-summary '((1 0) (2 0))
      (setq gnus-newsgroup-unreads (list 1))
      (mail-toggle-star)
      (expect marks-tests-marked :to-be nil)
      (expect gnus-newsgroup-marked :to-equal '(1))
      (expect gnus-newsgroup-unreads :to-equal '(1))
      (expect (gnus-summary-article-number) :to-be 2)))
  (it "unstars a starred unread message and leaves it unread"
    (marks-tests-in-summary '((1 0) (2 0))
      (setq gnus-newsgroup-unreads (list 1)
            gnus-newsgroup-marked (list 1))
      (mail-toggle-star)
      (expect marks-tests-marked :to-be nil)
      (expect gnus-newsgroup-marked :to-be nil)
      (expect gnus-newsgroup-unreads :to-equal '(1))))
  (it "redraws every line it stars, which draws the star column"
    (marks-tests-in-summary '((1 0) (2 0) (3 0))
      (setq gnus-newsgroup-unreads (list 2))
      (marks-tests-select 1 2)
      (mail-toggle-star)
      (expect marks-tests-redrawn :to-have-same-items-as '(1 2))
      (expect gnus-newsgroup-marked :to-equal '(1 2))
      (expect gnus-newsgroup-unreads :to-equal '(2)))))

(describe "mail-marked-articles"
  (it "answers one verb's articles, lowest first"
    (with-temp-buffer
      (setq-local mail-marks '((9 . delete) (2 . archive) (4 . delete)))
      (expect (mail-marked-articles 'delete) :to-equal '(4 9))
      (expect (mail-marked-articles 'archive) :to-equal '(2)))))

(defvar marks-tests-executed nil
  "What the stubbed Gnus commands were asked to do, newest first.")

(defmacro marks-tests-with-execute-stubs (&rest body)
  "Run BODY with the Gnus commands `mail-execute-marks' wraps stubbed.
Each stub logs the process mark it found, so a spec can tell which
articles each verb reached."
  (declare (indent 0))
  `(progn
     (setq marks-tests-executed nil)
     (cl-letf (((symbol-function 'gnus-summary-move-article)
                (lambda (_n to-group &rest _)
                  (push (list 'move to-group gnus-newsgroup-processable mark-active)
                        marks-tests-executed)))
               ((symbol-function 'gnus-summary-delete-article)
                (lambda (&rest _)
                  (push (list 'delete gnus-newsgroup-processable mark-active)
                        marks-tests-executed)))
               ((symbol-function 'gnus-summary-limit-to-marks)
                (lambda (marks &optional reverse)
                  (push (list 'limit marks reverse) marks-tests-executed))))
       ,@body)))

(describe "mail-execute-marks"
  (it "moves the deletions into the trash group and deletes the archives' files"
    (with-temp-buffer
      (setq-local mail-marks '((9 . delete) (2 . archive) (4 . delete)))
      (marks-tests-with-execute-stubs
        (mail-execute-marks))
      (expect (nreverse marks-tests-executed)
              :to-equal `((move ,mail-trash-group (4 9) nil)
                          (delete (2) nil)
                          (limit (,gnus-canceled-mark) reverse)))))
  (it "empties the queue"
    (with-temp-buffer
      (setq-local mail-marks '((9 . delete)))
      (marks-tests-with-execute-stubs
        (mail-execute-marks))
      (expect mail-marks :to-be nil)))
  (it "calls only the command a verb needs"
    (with-temp-buffer
      (setq-local mail-marks '((2 . archive)))
      (marks-tests-with-execute-stubs
        (mail-execute-marks))
      (expect (mapcar #'car marks-tests-executed) :to-have-same-items-as '(delete limit))))
  (it "ignores an active region, which would otherwise win over the process mark"
    (with-temp-buffer
      (setq-local mail-marks '((9 . delete)))
      (insert "one\ntwo\n")
      (set-mark (point-min))
      (goto-char (point-max))
      (expect mark-active :to-be-truthy)
      (marks-tests-with-execute-stubs
        (mail-execute-marks))
      (expect (nth 3 (assq 'move marks-tests-executed)) :to-be nil)))
  (it "refuses an empty queue"
    (with-temp-buffer
      (marks-tests-with-execute-stubs
        (expect (mail-execute-marks) :to-throw 'user-error))
      (expect marks-tests-executed :to-be nil))))

;;; marks-tests.el ends here
