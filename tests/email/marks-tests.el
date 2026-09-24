;;; tests/email/marks-tests.el --- deferred delete and archive specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defvar mail-trash-group "nnmaildir+gmail:trash")

(load-module-file "modules/email/autoload/marks.el")

(defun marks-tests-header (number)
  "Header of the article NUMBER."
  (make-full-mail-header number "subject" "ann@example.com"
                         "Mon, 21 Sep 2026 10:00:00 +0000"
                         (format "<%d@x>" number) "" 0 0))

(defvar marks-tests-redrawn nil
  "Articles whose line was redrawn, newest first.")

(defmacro marks-tests-in-summary (article thread &rest body)
  "Run BODY in a stand-in summary with point on ARTICLE inside THREAD.
Redrawing is stubbed and logged in `marks-tests-redrawn'."
  (declare (indent 2))
  `(with-temp-buffer
     (setq marks-tests-redrawn nil)
     (let ((gnus-newsgroup-name "nnmaildir+gmail:inbox")
           (gnus-newsgroup-sparse nil))
       (cl-letf (((symbol-function 'gnus-summary-article-number) (lambda () ,article))
                 ((symbol-function 'gnus-summary-top-thread) #'ignore)
                 ((symbol-function 'gnus-summary-articles-in-thread) (lambda (&rest _) ,thread))
                 ((symbol-function 'gnus-summary-position-point) #'ignore)
                 ((symbol-function 'mail-mark-redraw)
                  (lambda (article) (push article marks-tests-redrawn))))
         ,@body))))

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

(describe "mail-mark-for-deletion and mail-mark-for-archive"
  (it "queue the message at point and redraw its line"
    (marks-tests-in-summary 7 '(7)
      (mail-mark-for-deletion)
      (expect mail-marks :to-equal '((7 . delete)))
      (expect marks-tests-redrawn :to-equal '(7))))
  (it "replace one verb with the other instead of queueing twice"
    (marks-tests-in-summary 7 '(7)
      (mail-mark-for-deletion)
      (mail-mark-for-archive)
      (expect mail-marks :to-equal '((7 . archive)))))
  (it "leave the rest of the queue alone"
    (marks-tests-in-summary 7 '(7)
      (setq mail-marks '((3 . archive)))
      (mail-mark-for-deletion)
      (expect mail-marks :to-have-same-items-as '((3 . archive) (7 . delete)))))
  (it "refuse to archive out of the trash, where dropping the file deletes for good"
    (marks-tests-in-summary 7 '(7)
      (let ((gnus-newsgroup-name mail-trash-group))
        (expect (mail-mark-for-archive) :to-throw 'user-error)
        (expect mail-marks :to-be nil)
        (mail-mark-for-deletion)
        (expect mail-marks :to-equal '((7 . delete)))))))

(describe "mail-unmark"
  (it "takes the message at point out of the queue and redraws it"
    (marks-tests-in-summary 7 '(7)
      (setq mail-marks '((7 . delete) (8 . archive)))
      (mail-unmark)
      (expect mail-marks :to-equal '((8 . archive)))
      (expect marks-tests-redrawn :to-equal '(7))))
  (it "is a no-op on an unqueued message"
    (marks-tests-in-summary 7 '(7)
      (mail-unmark)
      (expect mail-marks :to-be nil))))

(describe "the thread commands"
  (it "queue every real article of the thread at point"
    (marks-tests-in-summary 11 '(10 11 12)
      (mail-mark-thread-for-deletion)
      (expect mail-marks :to-have-same-items-as '((10 . delete) (11 . delete) (12 . delete)))
      (expect marks-tests-redrawn :to-have-same-items-as '(10 11 12))))
  (it "skip the sparse placeholders Gnus invents for missing parents"
    (marks-tests-in-summary 11 '(-1 10 11)
      (let ((gnus-newsgroup-sparse '(-1)))
        (mail-mark-thread-for-archive))
      (expect mail-marks :to-have-same-items-as '((10 . archive) (11 . archive)))))
  (it "unqueue the whole thread"
    (marks-tests-in-summary 11 '(10 11 12)
      (setq mail-marks '((10 . delete) (12 . archive) (20 . delete)))
      (mail-unmark-thread)
      (expect mail-marks :to-equal '((20 . delete))))))

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
