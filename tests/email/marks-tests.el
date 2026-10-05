;;; tests/email/marks-tests.el --- summary marking specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defvar mail-inbox-group "nnmaildir+gmail:inbox")
(defvar mail-trash-group "nnmaildir+gmail:trash")
(defvar mail-archive-group "nnmaildir+gmail:archive")

(load-module-file "modules/email/autoload/marks.el")
(load-module-file "modules/email/autoload/news.el")

(defvar marks-tests-inbox nil
  "(MESSAGE-ID . ARTICLE) of each message the stand-in inbox holds.")

(defun marks-tests-header (number)
  "Header of the article NUMBER."
  (make-full-mail-header number "subject" "ann@example.com"
                         "Mon, 21 Sep 2026 10:00:00 +0000"
                         (format "<%d@x>" number) "" 0 0))

(defvar marks-tests-redrawn nil
  "Articles whose line was redrawn, newest first.")

(defvar marks-tests-marked nil
  "(ARTICLE . MARK) pairs Gnus was asked to set, newest first.")

(defvar marks-tests-secondary nil
  "Articles whose `%R' column was redrawn, newest first.")

(defun marks-tests-insert-lines (lines)
  "Insert a summary line and its Gnus data for each (ARTICLE LEVEL) in LINES."
  (dolist (line lines)
    (pcase-let ((`(,article ,level) line))
      (push (gnus-data-make article gnus-read-mark (1+ (point))
                            (marks-tests-header article) level)
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
Each line is (ARTICLE LEVEL), and article N's Message-ID is <N@x>.  Gnus
finds lines, threads and the next message through its own data; only
the redraws and the mark Gnus sets are stubbed, and logged.
The mark stub keeps the unread and tick lists the way Gnus does, and the
inbox holds what `marks-tests-inbox' says."
  (declare (indent 1))
  `(with-temp-buffer
     (setq marks-tests-redrawn nil
           marks-tests-marked nil
           marks-tests-secondary nil)
     (let ((gnus-newsgroup-name "nnmaildir+gmail:inbox")
           (gnus-newsgroup-data nil)
           (gnus-newsgroup-data-reverse nil)
           (gnus-newsgroup-sparse nil)
           (gnus-newsgroup-unreads nil)
           (gnus-newsgroup-marked nil)
           (gnus-newsgroup-processable nil)
           (transient-mark-mode t))
       (marks-tests-insert-lines ,lines)
       (cl-letf (((symbol-function 'gnus-summary-recenter) #'ignore)
                 ((symbol-function 'mail-mark-redraw)
                  (lambda (article) (push article marks-tests-redrawn)))
                 ((symbol-function 'gnus-summary-update-secondary-mark)
                  (lambda (article) (push article marks-tests-secondary)))
                 ((symbol-function 'gnus-summary-mark-article) #'marks-tests-set-mark)
                 ((symbol-function 'gnus-nnselect-group-p)
                  (lambda (group) (string-prefix-p "nnselect:" group)))
                 ((symbol-function 'inbox-articles-by-id)
                  (lambda (ids)
                    (seq-filter (lambda (copy) (member (car copy) ids)) marks-tests-inbox))))
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
  (it "draws a cross for a queued deletion, a down arrow for an archive, a space otherwise"
    (with-temp-buffer
      (setq-local mail-marks '((1 . delete) (2 . archive)))
      (expect (mapcar (lambda (n) (substring-no-properties
                                   (gnus-user-format-function-D (marks-tests-header n))))
                      '(1 2 3))
              :to-equal '("×" "↓" " "))))
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

(defmacro marks-tests-with-symbols (lines &rest body)
  "Run BODY in a real summary of LINES, whose symbols jit-lock draws.
Each line starts as the module's summary line does, with the queue, `%U'
and `%R' columns; line N is article N, its data mark the `%U' letter."
  (declare (indent 1))
  `(with-temp-buffer
     (gnus-summary-mode)
     (setq gnus-summary-mark-positions '((unread . 1) (replied . 2))
           gnus-newsgroup-data nil)
     (draw-mail-mark-symbols-h)
     (let ((inhibit-read-only t)
           (number 0))
       (dolist (line ,lines)
         (setq number (1+ number))
         (push (gnus-data-make number (aref line 1) (point) (marks-tests-header number) 0)
               gnus-newsgroup-data)
         (insert (propertize line 'gnus-number number) "\n")))
     (setq gnus-newsgroup-data (nreverse gnus-newsgroup-data))
     (goto-char (point-min))
     ,@body))

(defun marks-tests-shown ()
  "The `%U' and `%R' columns of every line, as redisplay draws them."
  (jit-lock-fontify-now)
  (save-excursion
    (goto-char (point-min))
    (let (shown)
      (while (not (eobp))
        (push (mapcar (lambda (offset)
                        (let ((pos (+ (point) offset)))
                          (or (get-text-property pos 'display)
                              (string (char-after pos)))))
                      '(1 2))
              shown)
        (forward-line 1))
      (nreverse shown))))

(describe "draw-mail-mark-symbols"
  (it "shows unread mail as a dot and every read state as a blank"
    ;; a starred read message too: the star has a column of its own
    (marks-tests-with-symbols
        (mapcar (lambda (mark) (format " %c  subject" mark))
                (list gnus-unread-mark gnus-read-mark gnus-del-mark gnus-ancient-mark
                      gnus-ticked-mark gnus-killed-mark gnus-catchup-mark
                      gnus-low-score-mark gnus-kill-file-mark gnus-duplicate-mark
                      gnus-sparse-mark))
      (expect (mapcar #'car (marks-tests-shown))
              :to-equal (cons "●" (make-list 10 " ")))))
  (it "keeps the letter of a rarer mark"
    (marks-tests-with-symbols
        (mapcar (lambda (mark) (format " %c  subject" mark))
                (list gnus-dormant-mark gnus-expirable-mark gnus-spam-mark
                      gnus-canceled-mark))
      (expect (mapcar #'car (marks-tests-shown)) :to-equal '("?" "E" "$" "G"))))
  (it "shows replied and forwarded as arrows, and the unseen dot as a blank"
    ;; unseen is mail that arrived since the group was last entered,
    ;; mail read elsewhere included
    (marks-tests-with-symbols
        (mapcar (lambda (mark) (format " %c%c subject" gnus-ancient-mark mark))
                (list gnus-replied-mark gnus-forwarded-mark gnus-unseen-mark
                      gnus-no-mark gnus-process-mark))
      (expect (mapcar #'cadr (marks-tests-shown))
              :to-equal (list "↩" "↪" " " " " (string gnus-process-mark)))))
  (it "follows a mark Gnus replaces in place"
    ;; Gnus copies the old letter's properties onto the new one
    (marks-tests-with-symbols (list (format " %c%c subject" gnus-ancient-mark gnus-no-mark))
      (marks-tests-shown)
      (cl-letf (((symbol-function 'gnus-summary-update-line) #'ignore))
        (gnus-summary-update-mark gnus-unread-mark 'unread)
        (gnus-summary-update-mark gnus-replied-mark 'replied))
      (expect (marks-tests-shown) :to-equal '(("●" "↩")))))
  (it "leaves alone a column the line format lacks"
    (marks-tests-with-symbols (list (format " %c%c subject" gnus-unread-mark gnus-replied-mark))
      (setq gnus-summary-mark-positions '((unread . 1)))
      (expect (marks-tests-shown)
              :to-equal (list (list "●" (string gnus-replied-mark)))))))

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
  (it "archive in any group only a message the inbox holds"
    ;; Gmail's archive takes the Inbox label and nothing else
    (marks-tests-in-summary '((7 0) (8 0) (9 0))
      (let ((gnus-newsgroup-name "nnmaildir+gmail:github")
            (marks-tests-inbox '(("<8@x>" . 108)))
            said)
        (cl-letf (((symbol-function 'message)
                   (lambda (format &rest args) (setq said (apply #'format format args)))))
          (marks-tests-select 7 9)
          (mail-mark-for-archive))
        (expect mail-marks :to-equal '((8 . archive)))
        (expect said :to-equal "2 not in the inbox, left alone"))))
  (it "refuse to archive when none of the messages is in the inbox"
    (marks-tests-in-summary '((7 0) (8 1) (9 0))
      (dolist (group (list mail-trash-group mail-archive-group "nnselect:search"))
        (let ((gnus-newsgroup-name group))
          (expect (mail-mark-for-archive) :to-throw 'user-error '("Not in the inbox"))
          (expect (mail-mark-thread-for-archive) :to-throw 'user-error)))
      (expect mail-marks :to-be nil)
      (let ((gnus-newsgroup-name mail-trash-group))
        (mail-mark-for-deletion))
      (expect mail-marks :to-equal '((7 . delete)))))
  (it "archive every message of the inbox summary without asking where it is"
    (marks-tests-in-summary '((7 0))
      (cl-letf (((symbol-function 'inbox-articles-by-id)
                 (lambda (&rest _) (error "The inbox summary shows the inbox"))))
        (mail-mark-for-archive))
      (expect mail-marks :to-equal '((7 . archive))))))

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

(describe "a mark command with messages marked"
  (it "acts on the marked messages, not the one at point, and leaves point there"
    (marks-tests-in-summary '((1 0) (2 0) (3 0) (4 0))
      (setq gnus-newsgroup-unreads (list 1 2 3 4)
            gnus-newsgroup-processable (list 3 1))
      (gnus-summary-goto-subject 2)
      (mail-toggle-read)
      (expect gnus-newsgroup-unreads :to-equal '(2 4))
      (expect (gnus-summary-article-number) :to-be 2)))
  (it "takes the mark off each message it acted on and redraws its mark column"
    (marks-tests-in-summary '((1 0) (2 0) (3 0))
      (setq gnus-newsgroup-processable (list 3 1))
      (mail-toggle-star)
      (expect gnus-newsgroup-marked :to-equal '(1 3))
      (expect gnus-newsgroup-processable :to-be nil)
      (expect marks-tests-secondary :to-have-same-items-as '(1 3))))
  (it "lets an active region win, and unmarks only the marked messages it covers"
    (marks-tests-in-summary '((1 0) (2 0) (3 0) (4 0))
      (setq gnus-newsgroup-unreads (list 1 2 3 4)
            gnus-newsgroup-processable (list 3 1))
      (marks-tests-select 2 3)
      (mail-toggle-read)
      (expect gnus-newsgroup-unreads :to-equal '(1 4))
      (expect gnus-newsgroup-processable :to-equal '(1))
      (expect (gnus-summary-article-number) :to-be 4)))
  (it "leaves alone a marked message the summary does not show"
    (marks-tests-in-summary '((1 0) (2 0) (3 0))
      (setq gnus-newsgroup-unreads (list 1 2 3)
            gnus-newsgroup-processable (list 9 3))
      (mail-toggle-read)
      (expect gnus-newsgroup-unreads :to-equal '(1 2))
      (expect gnus-newsgroup-processable :to-equal '(9))))
  (it "takes the message at point when no marked message is shown"
    (marks-tests-in-summary '((1 0) (2 0))
      (setq gnus-newsgroup-unreads (list 1 2)
            gnus-newsgroup-processable (list 9))
      (mail-toggle-read)
      (expect gnus-newsgroup-unreads :to-equal '(2))
      (expect (gnus-summary-article-number) :to-be 2)))
  (it "queues the marked messages for the trash"
    (marks-tests-in-summary '((1 0) (2 0) (3 0))
      (setq gnus-newsgroup-processable (list 3 1))
      (gnus-summary-goto-subject 2)
      (mail-mark-for-deletion)
      (expect mail-marks :to-have-same-items-as '((1 . delete) (3 . delete)))
      (expect gnus-newsgroup-processable :to-be nil)
      (expect (gnus-summary-article-number) :to-be 2)))
  (it "takes the marked messages out of the queue and marks them unread"
    (marks-tests-in-summary '((1 0) (2 0) (3 0))
      (setq mail-marks (list (cons 1 'delete) (cons 2 'delete))
            gnus-newsgroup-processable (list 1 3))
      (mail-unmark)
      (expect mail-marks :to-equal '((2 . delete)))
      (expect gnus-newsgroup-unreads :to-equal '(1 3))
      (expect gnus-newsgroup-processable :to-be nil)))
  (it "takes every thread holding a marked message, and unmarks it"
    (marks-tests-in-summary '((10 0) (11 1) (12 1) (20 0) (30 0) (31 1))
      (setq gnus-newsgroup-unreads (list 10 11 12 20 30 31)
            gnus-newsgroup-processable (list 31 11))
      (gnus-summary-goto-subject 20)
      (mail-mark-thread-read)
      (expect gnus-newsgroup-unreads :to-equal '(20))
      (expect gnus-newsgroup-processable :to-be nil)
      (expect (gnus-summary-article-number) :to-be 20)
      (mail-mark-thread-for-deletion)
      (expect mail-marks :to-equal '((20 . delete)))))
  (it "keeps the marks when the command refuses"
    (marks-tests-in-summary '((7 0) (8 0))
      (let ((gnus-newsgroup-name "nnmaildir+gmail:github"))
        (setq gnus-newsgroup-processable (list 8 7))
        (expect (mail-mark-for-archive) :to-throw 'user-error '("Not in the inbox"))
        (expect gnus-newsgroup-processable :to-equal '(8 7))))))

(describe "mail-selection-menu"
  (it "marks in bulk and runs the mark keys' commands"
    (dolist (pair '(("*" . gnus-uu-mark-buffer)
                    ("u" . gnus-summary-unmark-all-processable)
                    ("s" . gnus-uu-mark-by-regexp)
                    ("!" . mail-toggle-read)
                    ("=" . mail-toggle-star)
                    ("d" . mail-mark-for-deletion)
                    ("D" . mail-mark-thread-for-deletion)
                    ("a" . mail-mark-for-archive)
                    ("A" . mail-mark-thread-for-archive)
                    ("x" . mail-execute-marks)))
      (expect (plist-get (cdr (transient-get-suffix 'mail-selection-menu (car pair))) :command)
              :to-be (cdr pair))))
  (it "lays its keys out in columns of at most two rows"
    (let ((group (car (aref (get 'mail-selection-menu 'transient--layout) 2))))
      (expect (aref group 0) :to-be 'transient-columns)
      (dolist (column (aref group 2))
        (expect (length (aref column 2)) :to-be-less-than 3))))
  (it "names what its commands act on: the region, else the marked messages, else point"
    (marks-tests-in-summary '((1 0) (2 0) (3 0))
      (expect (mail-selection-header) :to-equal "Selected: the message at point")
      (setq gnus-newsgroup-processable (list 9 3 1))
      (expect (mail-selection-header) :to-equal "Selected: 2 marked")
      (marks-tests-select 1 2)
      (expect (mail-selection-header) :to-equal "Selected: the region's messages"))))

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
Each stub logs what it was asked, the process mark included, so a spec
can tell which articles each verb reached."
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
                  (push (list 'limit marks reverse) marks-tests-executed)))
               ((symbol-function 'gnus-request-expire-articles)
                (lambda (articles group force)
                  (push (list 'expire group articles force) marks-tests-executed)
                  nil))
               ((symbol-function 'refresh-mail-group)
                (lambda (group) (push (list 'refresh group) marks-tests-executed)))
               ;; the trash is loaded unless a spec says otherwise
               ((symbol-function 'mail-group-loaded-p) (lambda (_) t))
               ((symbol-function 'gnus-group-update-group) #'ignore)
               ((symbol-function 'drop-gone-articles)
                (lambda (group articles)
                  (push (list 'drop group articles) marks-tests-executed)))
               ((symbol-function 'mail-mark-redraw)
                (lambda (article) (push (list 'redraw article) marks-tests-executed))))
       ,@body)))

(defun marks-tests-steps ()
  "What the stubs were asked, in order, the drops sorted by group."
  (let* ((steps (reverse marks-tests-executed))
         (drops (sort (seq-filter (lambda (step) (eq (car step) 'drop)) steps)
                      (lambda (a b) (string< (cadr a) (cadr b))))))
    (mapcar (lambda (step) (if (eq (car step) 'drop) (pop drops) step)) steps)))

(describe "mail-execute-marks"
  (it "keeps the queue while the trash loads, and loads it first"
    ;; nnmaildir would refuse every move into it
    (marks-tests-in-summary '((2 0) (4 0))
      (setq-local mail-marks '((4 . delete) (2 . archive)))
      (marks-tests-with-execute-stubs
        (let (asked)
          (cl-letf (((symbol-function 'mail-group-loaded-p) #'ignore)
                    ((symbol-function 'load-mail-groups)
                     (lambda (&rest args) (push args asked))))
            (expect (mail-execute-marks) :to-throw 'user-error '("trash is still loading; the queue stays")))
          (expect asked :to-equal '((("nnmaildir+gmail:trash") t)))))
      (expect marks-tests-executed :to-be nil)
      (expect mail-marks :to-equal '((4 . delete) (2 . archive)))))
  (it "drops the messages that left from the summary's process marks"
    (marks-tests-in-summary '((2 0) (4 0) (7 0))
      (setq-local mail-marks '((4 . delete) (2 . archive)))
      (setq gnus-newsgroup-processable (list 7 4 2))
      (marks-tests-with-execute-stubs
        (mail-execute-marks))
      (expect gnus-newsgroup-processable :to-equal '(7))
      (expect (nth 2 (assq 'move marks-tests-executed)) :to-equal '(4))))
  (it "moves the deletions into the trash and deletes the archives' inbox files"
    (marks-tests-in-summary '((2 0) (4 0) (9 0))
      (setq-local mail-marks '((9 . delete) (2 . archive) (4 . delete)))
      (marks-tests-with-execute-stubs
        (mail-execute-marks))
      (expect (nreverse marks-tests-executed)
              :to-equal `((move ,mail-trash-group (4 9) nil)
                          (delete (2) nil)
                          (drop ,mail-inbox-group (4 9 2))
                          (limit (,gnus-canceled-mark) reverse)))))
  (it "in a label, archives by deleting the inbox copy and keeps the label's line"
    ;; the message keeps every label, this one included
    (marks-tests-in-summary '((2 0) (4 0) (9 0))
      (let ((gnus-newsgroup-name "nnmaildir+gmail:github")
            (marks-tests-inbox '(("<2@x>" . 102) ("<9@x>" . 109))))
        (setq-local mail-marks '((2 . archive) (4 . delete) (9 . delete)))
        (marks-tests-with-execute-stubs
          (mail-execute-marks))
        (expect (marks-tests-steps)
                :to-equal `((move ,mail-trash-group (4 9) nil)
                            (expire ,mail-inbox-group (102 109) t)
                            (refresh ,mail-inbox-group)
                            (drop "nnmaildir+gmail:github" (4 9))
                            (drop ,mail-inbox-group (102 109))
                            (redraw 2)
                            (limit (,gnus-canceled-mark) reverse))))))
  (it "in search results, acts on an inbox hit through the summary and drops it from the inbox's"
    (marks-tests-in-summary '((1 0) (2 0) (3 0))
      (let ((gnus-newsgroup-name "nnselect:search")
            (gnus-newsgroup-selection [["nnmaildir+gmail:inbox" 11 100]
                                       ["nnmaildir+gmail:inbox" 12 100]
                                       ["nnmaildir+gmail:github" 5 100]])
            (marks-tests-inbox '(("<1@x>" . 11) ("<2@x>" . 12))))
        (setq-local mail-marks '((1 . archive) (2 . delete) (3 . delete)))
        (marks-tests-with-execute-stubs
          (mail-execute-marks))
        (expect (marks-tests-steps)
                :to-equal `((move ,mail-trash-group (2 3) nil)
                            (delete (1) nil)
                            (drop "nnmaildir+gmail:github" (5))
                            (drop ,mail-inbox-group (12 11))
                            (limit (,gnus-canceled-mark) reverse))))))
  (it "in a news group, deletes the posts where they lie and leaves Gmail alone"
    ;; the trash would upload them to Gmail, and the inbox copy of a
    ;; post that also came by mail is the inbox's business
    (marks-tests-in-summary '((4 0) (9 0))
      (let ((gnus-newsgroup-name "nnmaildir+news:gmane.test")
            (marks-tests-inbox '(("<9@x>" . 109))))
        (setq-local mail-marks '((4 . delete) (9 . delete)))
        (marks-tests-with-execute-stubs
          (mail-execute-marks))
        (expect (marks-tests-steps)
                :to-equal `((delete (4 9) nil)
                            (drop "nnmaildir+news:gmane.test" (4 9))
                            (limit (,gnus-canceled-mark) reverse))))))
  (it "in search results, deletes a news hit where it lies and trashes a mail hit"
    (marks-tests-in-summary '((1 0) (2 0))
      (let ((gnus-newsgroup-name "nnselect:search")
            (gnus-newsgroup-selection [["nnmaildir+news:gmane.test" 7 100]
                                       ["nnmaildir+gmail:github" 5 100]]))
        (setq-local mail-marks '((1 . delete) (2 . delete)))
        (marks-tests-with-execute-stubs
          (mail-execute-marks))
        (expect (marks-tests-steps)
                :to-equal `((move ,mail-trash-group (2) nil)
                            (delete (1) nil)
                            (drop "nnmaildir+gmail:github" (5))
                            (drop "nnmaildir+news:gmane.test" (7))
                            (limit (,gnus-canceled-mark) reverse))))))
  (it "empties the queue"
    (marks-tests-in-summary '((9 0))
      (setq-local mail-marks '((9 . delete)))
      (marks-tests-with-execute-stubs
        (mail-execute-marks))
      (expect mail-marks :to-be nil)))
  (it "calls only the command a verb needs"
    (marks-tests-in-summary '((2 0))
      (setq-local mail-marks '((2 . archive)))
      (marks-tests-with-execute-stubs
        (mail-execute-marks))
      (expect (mapcar #'car marks-tests-executed) :to-have-same-items-as '(delete drop limit))))
  (it "ignores an active region, which would otherwise win over the process mark"
    (marks-tests-in-summary '((1 0) (9 0))
      (setq-local mail-marks '((9 . delete)))
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

(describe "inbox-articles-by-id"
  (it "reads the inbox first and answers the inbox article of each message it holds"
    (let ((nntp-server-buffer (generate-new-buffer " *marks-tests nntp*"))
          activated asked)
      (unwind-protect
          (cl-letf (((symbol-function 'gnus-activate-group)
                     (lambda (&rest args) (setq activated args)))
                    ((symbol-function 'gnus-retrieve-headers)
                     (lambda (ids group &rest _)
                       (setq asked (list ids group))
                       (with-current-buffer nntp-server-buffer
                         (erase-buffer)
                         (insert "102\tsubject\tann@x\tMon, 21 Sep 2026 10:00:00 +0000\t<2@x>\t\t0\t0\n"))
                       'nov)))
            (expect (inbox-articles-by-id '("<2@x>" "<3@x>")) :to-equal '(("<2@x>" . 102)))
            (expect activated :to-equal (list mail-inbox-group 'scan))
            (expect asked :to-equal (list '("<2@x>" "<3@x>") mail-inbox-group)))
        (kill-buffer nntp-server-buffer))))
  (it "asks nothing for no messages"
    (cl-letf (((symbol-function 'gnus-retrieve-headers) (lambda (&rest _) (error "Asked"))))
      (expect (inbox-articles-by-id nil) :to-be nil))))

(describe "mail-inbox-copies"
  (it "pairs each article with its message's inbox article, leaving out the rest"
    (marks-tests-in-summary '((1 0) (2 0) (3 0))
      (let ((gnus-newsgroup-name "nnmaildir+gmail:github")
            (marks-tests-inbox '(("<3@x>" . 13) ("<1@x>" . 11))))
        (expect (mail-inbox-copies '(1 2 3)) :to-equal '((1 . 11) (3 . 13))))))
  (it "takes the inbox summary's articles as their own copies"
    (marks-tests-in-summary '((1 0))
      (expect (mail-inbox-copies '(1)) :to-equal '((1 . 1))))))

(describe "mail-article-group and mail-article-number"
  (it "answer the group and number a search result's file has"
    (marks-tests-in-summary '((1 0) (2 0))
      (let ((gnus-newsgroup-name "nnselect:search")
            (gnus-newsgroup-selection [["nnmaildir+gmail:inbox" 11 100]
                                       ["nnmaildir+gmail:github" 5 100]]))
        (expect (mapcar #'mail-article-group '(1 2))
                :to-equal '("nnmaildir+gmail:inbox" "nnmaildir+gmail:github"))
        (expect (mapcar #'mail-article-number '(1 2)) :to-equal '(11 5)))))
  (it "answer the summary's own group and number elsewhere"
    (marks-tests-in-summary '((1 0))
      (expect (mail-article-group 1) :to-equal "nnmaildir+gmail:inbox")
      (expect (mail-article-number 1) :to-be 1))))

(describe "drop-gone-articles"
  (it "cancels the articles in the group's open summary and limits them out"
    (let ((inbox (get-buffer-create "*Summary nnmaildir+gmail:inbox*"))
          canceled limited)
      (unwind-protect
          (progn
            (with-current-buffer inbox
              (setq-local gnus-newsgroup-data
                          (list (gnus-data-make 11 gnus-read-mark 1 nil 0)
                                (gnus-data-make 12 gnus-read-mark 2 nil 0))))
            (cl-letf (((symbol-function 'gnus-summary-mark-article)
                       (lambda (article mark &rest _)
                         (push (list (buffer-name) article mark) canceled)))
                      ((symbol-function 'gnus-summary-limit-to-marks)
                       (lambda (marks &optional reverse)
                         (setq limited (list (buffer-name) marks reverse)))))
              (with-temp-buffer
                (drop-gone-articles mail-inbox-group '(12 99))))
            (expect canceled :to-equal `(("*Summary nnmaildir+gmail:inbox*" 12 ,gnus-canceled-mark)))
            (expect limited :to-equal `("*Summary nnmaildir+gmail:inbox*"
                                        (,gnus-canceled-mark) reverse)))
        (kill-buffer inbox))))
  (it "leaves alone the summary it is called from"
    (let ((inbox (get-buffer-create "*Summary nnmaildir+gmail:inbox*")))
      (unwind-protect
          (cl-letf (((symbol-function 'gnus-summary-limit-to-marks)
                     (lambda (&rest _) (error "Limited its own summary"))))
            (with-current-buffer inbox
              (expect (drop-gone-articles mail-inbox-group '(12)) :to-be nil)))
        (kill-buffer inbox)))))

(describe "quit-mail-summary"
  :var (asked executed exited)
  (before-each
    (setq asked nil executed nil exited nil)
    (spy-on 'mail-execute-marks :and-call-fake (lambda () (setq executed t)))
    (spy-on 'gnus-summary-exit :and-call-fake (lambda (&rest _) (setq exited t))))

  (it "leaves at once when nothing is queued"
    (spy-on 'y-or-n-p)
    (with-temp-buffer
      (quit-mail-summary))
    (expect 'y-or-n-p :not :to-have-been-called)
    (expect (list executed exited) :to-equal '(nil t)))
  (it "asks, runs the queue and leaves on yes"
    (spy-on 'y-or-n-p :and-call-fake (lambda (prompt) (setq asked prompt) t))
    (with-temp-buffer
      (setq-local mail-marks '((1 . delete) (2 . delete) (3 . archive)))
      (quit-mail-summary))
    (expect asked :to-equal "Run 2 deletions and 1 archive first? ")
    (expect (list executed exited) :to-equal '(t t)))
  (it "drops the queue and leaves on no"
    (spy-on 'y-or-n-p :and-return-value nil)
    (with-temp-buffer
      (setq-local mail-marks '((1 . archive)))
      (quit-mail-summary))
    (expect (list executed exited) :to-equal '(nil t)))
  (it "stays on C-g"
    (spy-on 'y-or-n-p :and-call-fake (lambda (&rest _) (signal 'quit nil)))
    (with-temp-buffer
      (setq-local mail-marks '((1 . delete)))
      (expect (condition-case nil
                  (progn (quit-mail-summary) 'left)
                (quit 'stayed))
              :to-be 'stayed))
    (expect (list executed exited) :to-equal '(nil nil))))

(describe "mail-queue-description"
  (it "counts each verb in words"
    (with-temp-buffer
      (setq-local mail-marks '((1 . delete)))
      (expect (mail-queue-description) :to-equal "1 deletion")
      (setq-local mail-marks '((1 . archive) (2 . archive)))
      (expect (mail-queue-description) :to-equal "2 archives"))))

(describe "note-entry-marks-h"
  (it "notes a summary's unread, unselected and starred articles as it opens"
    (marks-tests-in-summary '((1 0) (2 0))
      (let ((gnus-newsgroup-unselected (list 5)))
        (setq gnus-newsgroup-unreads (list 1)
              gnus-newsgroup-marked (list 2))
        (note-entry-marks-h)
        ;; the note is a copy, which marking later leaves alone
        (setcar gnus-newsgroup-unreads 9)
        (setcar gnus-newsgroup-marked 9)
        (expect mail-entry-marks :to-equal '((1 5) . (2))))))
  (it "notes a search summary the same way"
    (marks-tests-in-summary '((1 0))
      (let ((gnus-newsgroup-name "nnselect:search")
            (gnus-newsgroup-unselected nil))
        (setq gnus-newsgroup-unreads (list 1))
        (note-entry-marks-h)
        (expect mail-entry-marks :to-equal '((1) . nil))))))

(describe "mail-set-read-and-star"
  (it "gives the article the read and star state asked for, keeping the other"
    (marks-tests-in-summary '((1 0) (2 0))
      (setq gnus-newsgroup-unreads (list 1 2)
            gnus-newsgroup-marked (list 2))
      (mail-set-read-and-star 1 nil t)
      (mail-set-read-and-star 2 nil t)
      (expect gnus-newsgroup-unreads :to-be nil)
      (expect gnus-newsgroup-marked :to-equal '(1 2))))
  (it "passes over an article the summary does not hold"
    (marks-tests-in-summary '((1 0))
      (mail-set-read-and-star 7 nil t)
      (expect marks-tests-marked :to-be nil))))

(describe "carry-search-marks-h"
  (it "gives the open summaries what the search changed, and nothing else"
    ;; the inbox summary under a search would write its older state back
    (let ((inbox (get-buffer-create "*Summary nnmaildir+gmail:inbox*"))
          carried)
      (unwind-protect
          (marks-tests-in-summary '((1 0) (2 0) (3 0))
            (let ((gnus-newsgroup-name "nnselect:search")
                  (gnus-newsgroup-selection [["nnmaildir+gmail:inbox" 11 100]
                                             ["nnmaildir+gmail:inbox" 12 100]
                                             ["nnmaildir+gmail:github" 5 100]])
                  (gnus-newsgroup-articles (list 1 2 3)))
              (setq-local mail-entry-marks (cons (list 1 2) nil))
              ;; read 1, left 2 alone, starred 3
              (setq gnus-newsgroup-unreads (list 2)
                    gnus-newsgroup-marked (list 3))
              (cl-letf (((symbol-function 'mail-set-read-and-star)
                         (lambda (&rest args) (push (cons (buffer-name) args) carried))))
                (carry-search-marks-h))))
        (kill-buffer inbox))
      ;; github has no open summary
      (expect carried :to-equal '(("*Summary nnmaildir+gmail:inbox*" 11 nil nil)))))
  (it "carries nothing when the search is left without saving"
    (let ((inbox (get-buffer-create "*Summary nnmaildir+gmail:inbox*")))
      (unwind-protect
          (marks-tests-in-summary '((1 0))
            (let ((gnus-newsgroup-name "nnselect:search")
                  (gnus-newsgroup-selection [["nnmaildir+gmail:inbox" 11 100]])
                  (gnus-newsgroup-articles (list 1))
                  (gnus-group-is-exiting-without-update-p t))
              ;; read 1, which the inbox summary would learn on a saving exit
              (setq-local mail-entry-marks (cons (list 1) nil))
              (spy-on 'mail-set-read-and-star)
              (carry-search-marks-h)
              (expect 'mail-set-read-and-star :not :to-have-been-called)))
        (kill-buffer inbox))))
  (it "leaves a label's summary alone, whose own exit saves what it changed"
    (let ((inbox (get-buffer-create "*Summary nnmaildir+gmail:inbox*")))
      (unwind-protect
          (marks-tests-in-summary '((1 0))
            (let ((gnus-newsgroup-articles (list 1)))
              ;; read 1 since the summary opened
              (setq-local mail-entry-marks (cons (list 1) nil))
              (spy-on 'mail-set-read-and-star)
              (carry-search-marks-h)
              (expect 'mail-set-read-and-star :not :to-have-been-called)))
        (kill-buffer inbox)))))

(describe "activate-search-hit-groups-h"
  :var (activated)
  (before-each
    (setq activated nil)
    (spy-on 'gnus-activate-group
            :and-call-fake (lambda (group &rest _) (push group activated))))

  (it "activates each group whose active range ends below one of its hits"
    ;; nnselect would save no read mark above the range
    (marks-tests-in-summary '((1 0) (2 0) (3 0) (4 0))
      (let ((gnus-newsgroup-name "nnselect:search")
            (gnus-newsgroup-selection [["nnmaildir+gmail:inbox" 12 100]
                                       ["nnmaildir+gmail:inbox" 1066 100]
                                       ["nnmaildir+gmail:github" 5 100]
                                       ["nnmaildir+gmail:money" 7 100]])
            (gnus-newsgroup-articles (list 1 2 3 4))
            (gnus-active-hashtb (make-hash-table :test #'equal)))
        (puthash "nnmaildir+gmail:inbox" '(2 . 1065) gnus-active-hashtb)
        (puthash "nnmaildir+gmail:github" '(1 . 10) gnus-active-hashtb)
        (activate-search-hit-groups-h)))
    ;; money has no active range at all
    (expect (sort activated #'string<)
            :to-equal '("nnmaildir+gmail:inbox" "nnmaildir+gmail:money")))
  (it "activates nothing when the search is left without saving"
    (marks-tests-in-summary '((1 0))
      (let ((gnus-newsgroup-name "nnselect:search")
            (gnus-newsgroup-selection [["nnmaildir+gmail:inbox" 1066 100]])
            (gnus-newsgroup-articles (list 1))
            (gnus-active-hashtb (make-hash-table :test #'equal))
            (gnus-group-is-exiting-without-update-p t))
        (activate-search-hit-groups-h)))
    (expect activated :to-be nil))
  (it "leaves a label's summary alone"
    (marks-tests-in-summary '((1 0))
      (let ((gnus-newsgroup-articles (list 1))
            (gnus-active-hashtb (make-hash-table :test #'equal)))
        (activate-search-hit-groups-h)))
    (expect activated :to-be nil)))

(defmacro marks-tests-updating (group entry now read &rest body)
  "Run BODY in a summary of GROUP opened at the active range ENTRY.
GROUP's active range is NOW and its info's read ranges READ."
  (declare (indent 4))
  `(with-temp-buffer
     (setq major-mode 'gnus-summary-mode)
     (setq-local gnus-newsgroup-name ,group)
     (setq-local gnus-newsgroup-active ,entry)
     (let ((gnus-active-hashtb (make-hash-table :test #'equal))
           (gnus-newsrc-hashtb (make-hash-table :test #'equal)))
       (puthash ,group ,now gnus-active-hashtb)
       (puthash ,group (list 3 (list ,group 1 ,read)) gnus-newsrc-hashtb)
       ,@body)))

(describe "keep-newer-read-marks-a"
  :var (called)
  (before-each
    (setq called nil))

  (it "counts mail above the summary's range as the info has it"
    ;; Gnus would take the read mark off every article above the range
    (marks-tests-updating "nnmaildir+gmail:inbox" '(2 . 1065) '(2 . 1068)
        '((1 . 1064) 1066 1068)
      (keep-newer-read-marks-a
       (lambda (&rest args) (setq called (cons gnus-newsgroup-active args)))
       "nnmaildir+gmail:inbox" (list 1065))
      (expect gnus-newsgroup-active :to-equal '(2 . 1065)))
    (expect called :to-equal '((2 . 1068) "nnmaildir+gmail:inbox" (1065 1067) nil)))
  (it "passes the call on untouched when nothing arrived since the summary opened"
    (marks-tests-updating "nnmaildir+gmail:inbox" '(2 . 1065) '(2 . 1065) '((1 . 1064))
      (keep-newer-read-marks-a
       (lambda (&rest args) (setq called (cons gnus-newsgroup-active args)))
       "nnmaildir+gmail:inbox" (list 1065)))
    (expect called :to-equal '((2 . 1065) "nnmaildir+gmail:inbox" (1065) nil)))
  (it "passes on a save into a group other than the summary's"
    ;; the summary's range says nothing about that group's articles
    (marks-tests-updating "nnmaildir+gmail:inbox" '(1 . 3) '(2 . 1068) '((1 . 1064) 1066)
      (setq-local gnus-newsgroup-name "nnselect:search")
      (keep-newer-read-marks-a
       (lambda (&rest args) (setq called (cons gnus-newsgroup-active args)))
       "nnmaildir+gmail:inbox" (list 1065)))
    (expect called :to-equal '((1 . 3) "nnmaildir+gmail:inbox" (1065) nil)))
  (it "passes on a call from outside a summary"
    ;; the group buffer's catch-up saves the group without one
    (marks-tests-updating "nnmaildir+gmail:inbox" '(2 . 1065) '(2 . 1068) '((1 . 1064) 1066)
      (setq major-mode 'fundamental-mode)
      (keep-newer-read-marks-a
       (lambda (&rest args) (setq called (cons gnus-newsgroup-active args)))
       "nnmaildir+gmail:inbox" nil))
    (expect called :to-equal '((2 . 1065) "nnmaildir+gmail:inbox" nil nil)))
  (it "passes on a computation, which saves nothing"
    (marks-tests-updating "nnmaildir+gmail:inbox" '(2 . 1065) '(2 . 1068) '((1 . 1064) 1066)
      (keep-newer-read-marks-a
       (lambda (&rest args) (setq called (cons gnus-newsgroup-active args)))
       "nnmaildir+gmail:inbox" (list 1065) t))
    (expect called :to-equal '((2 . 1065) "nnmaildir+gmail:inbox" (1065) t))))

(describe "merge-mark-list"
  (it "takes a change made elsewhere where the summary made none"
    ;; 1 and 2 changed elsewhere only, 3 and 4 in the summary only, 5 on
    ;; both sides, 6 nowhere
    (expect (merge-mark-list '(1 4 6) '(1 3 5 6) '(2 3 6)) :to-equal '(2 4 6)))
  (it "returns the summary's list when nothing changed elsewhere"
    (expect (merge-mark-list '(1 3) '(1 2) '(1 2)) :to-equal '(1 3))))

(describe "keep-group-changes-h"
  (before-each
    (spy-on 'gnus-nnselect-group-p
            :and-call-fake (lambda (group) (string-prefix-p "nnselect:" group))))

  (it "gives a label's summary what its group got meanwhile, keeping its own changes"
    ;; elsewhere 2 and 9 were read, 3 unread, 4 starred and 7 unstarred;
    ;; the summary read 4 and unread 1 itself, and 9 and 10 are unselected
    (marks-tests-updating "nnmaildir+gmail:inbox" '(1 . 10) '(1 . 10) '((1 . 2) 5 (7 . 9))
      (gnus-info-set-marks (gnus-get-info "nnmaildir+gmail:inbox") '((tick 4 6)) t)
      (setq-local mail-entry-marks (cons (list 2 4 6 9 10) (list 6 7)))
      (setq-local gnus-newsgroup-unreads (list 1 2 6))
      (setq-local gnus-newsgroup-unselected (list 9 10))
      (setq-local gnus-newsgroup-marked (list 6 7))
      (keep-group-changes-h)
      (expect gnus-newsgroup-unreads :to-equal '(1 3 6))
      (expect gnus-newsgroup-unselected :to-equal '(10))
      (expect gnus-newsgroup-marked :to-equal '(4 6))))
  (it "changes nothing in a summary that noted nothing as it opened"
    ;; the group counts 3 unread, which the summary never showed
    (marks-tests-updating "nnmaildir+gmail:inbox" '(1 . 3) '(1 . 3) nil
      (setq-local mail-entry-marks nil)
      (setq-local gnus-newsgroup-unreads (list 1 2))
      (setq-local gnus-newsgroup-unselected nil)
      (setq-local gnus-newsgroup-marked nil)
      (keep-group-changes-h)
      (expect gnus-newsgroup-unreads :to-equal '(1 2))))
  (it "leaves a search summary to the hook that maps its hits"
    ;; the search group's own info says nothing about the hits
    (marks-tests-updating "nnselect:search" '(1 . 3) '(1 . 3) '((1 . 3))
      (setq-local mail-entry-marks (cons (list 1 2) nil))
      (setq-local gnus-newsgroup-unreads (list 1 2))
      (setq-local gnus-newsgroup-unselected nil)
      (setq-local gnus-newsgroup-marked nil)
      (keep-group-changes-h)
      (expect gnus-newsgroup-unreads :to-equal '(1 2)))))

(defmacro marks-tests-in-search (&rest body)
  "Run BODY in a search of inbox 11 and 12 and github 5, all unread as it opened.
The inbox's info holds 11 read and 12 starred; github's holds nothing."
  (declare (indent 0))
  `(with-temp-buffer
     (setq-local gnus-newsgroup-name "nnselect:search")
     (setq-local gnus-newsgroup-selection [["nnmaildir+gmail:inbox" 11 100]
                                           ["nnmaildir+gmail:inbox" 12 100]
                                           ["nnmaildir+gmail:github" 5 100]])
     (setq-local gnus-newsgroup-active '(1 . 3))
     (setq-local mail-entry-marks (cons (list 1 2 3) nil))
     (setq-local gnus-newsgroup-unselected nil)
     (setq-local gnus-newsgroup-marked nil)
     (let ((gnus-active-hashtb (make-hash-table :test #'equal))
           (gnus-newsrc-hashtb (make-hash-table :test #'equal)))
       (puthash "nnmaildir+gmail:inbox"
                (list 3 (list "nnmaildir+gmail:inbox" 1 '((1 . 11)) '((tick 12))))
                gnus-newsrc-hashtb)
       (puthash "nnmaildir+gmail:github"
                (list 3 (list "nnmaildir+gmail:github" 1 nil nil))
                gnus-newsrc-hashtb)
       ,@body)))

(describe "keep-search-group-changes-h"
  (before-each
    (spy-on 'gnus-nnselect-group-p
            :and-call-fake (lambda (group) (string-prefix-p "nnselect:" group))))

  (it "gives a search what its hits' groups got meanwhile, keeping its own changes"
    ;; elsewhere inbox 11 was read and 12 starred; the search read github 5
    (marks-tests-in-search
      (setq-local gnus-newsgroup-unreads (list 1 2))
      (keep-search-group-changes-h)
      (expect gnus-newsgroup-unreads :to-equal '(2))
      (expect gnus-newsgroup-marked :to-equal '(2))))
  (it "changes nothing when the search is left without saving"
    (marks-tests-in-search
      (setq-local gnus-newsgroup-unreads (list 1 2))
      (let ((gnus-group-is-exiting-without-update-p t))
        (keep-search-group-changes-h))
      (expect gnus-newsgroup-unreads :to-equal '(1 2))
      (expect gnus-newsgroup-marked :to-be nil)))
  (it "leaves a label's summary to its own hook"
    (marks-tests-in-search
      (setq-local gnus-newsgroup-name "nnmaildir+gmail:inbox")
      (setq-local gnus-newsgroup-unreads (list 1 2))
      (spy-on 'nnselect-request-update-info)
      (keep-search-group-changes-h)
      (expect 'nnselect-request-update-info :not :to-have-been-called)
      (expect gnus-newsgroup-unreads :to-equal '(1 2)))))

;;; marks-tests.el ends here
