;;; tests/email/reddit-scores-tests.el --- Reddit scores in the thread view -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; each spec binds it, so a suite loaded later still sees config.el's
(defvar reddit-maildir)
;; the thread view's, which thread-tests loads
(defvar mail-thread-buffer-name "*mail thread*")
(defvar mail-thread-group nil)

(load-module-file "modules/email/autoload/reddit.el")
(load-module-file "modules/email/autoload/reddit-api.el")
(load-module-file "modules/email/autoload/reddit-scores.el")
;; the fetch writes the file the session reads
(load-module-file "scripts/reddit-fetch.el")

(defmacro reddit-scores-tests--with-store (&rest body)
  "Run BODY with a `reddit-maildir' holding r.test and no scores read yet."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "reddit-scores-tests" t)))
          (reddit-maildir (expand-file-name "reddit/" root))
          (reddit-scores-cache nil))
     (make-directory (expand-file-name "r.test/.nnmaildir" reddit-maildir) t)
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(defun reddit-scores-tests--save (rows)
  "Save ROWS as r.test's scores, the way the fetch saves them."
  (reddit-fetch-save-state reddit-maildir "r.test" reddit-fetch-scores-file rows))

(defun reddit-scores-tests--header (id)
  "Header of a Reddit message whose Message-ID is ID."
  (make-full-mail-header 1 "Re: Reddit post" "\"u/ann\" <ann@reddit.invalid>"
                         "Mon, 28 Sep 2026 10:00:00 +0000" id "" 0 0))

(defun reddit-scores-tests--line (id)
  "What the line of r.test's message ID shows of its score, and in which face."
  (let ((mail-thread-group "nnmaildir+reddit:r.test"))
    (when-let* ((shown (reddit-score-line (reddit-scores-tests--header id))))
      (list (substring-no-properties shown) (get-text-property 0 'face shown)))))

(describe "reddit-scores"
  (it "reads the scores the fetch saved, by Message-ID"
    (reddit-scores-tests--with-store
      (reddit-scores-tests--save '(("<c1.p1@reddit.com>" 5 1) ("<p1@reddit.com>" 22 nil)))
      (let ((scores (reddit-scores "nnmaildir+reddit:r.test")))
        (expect (gethash "<p1@reddit.com>" scores) :to-equal '(22 nil))
        (expect (gethash "<c1.p1@reddit.com>" scores) :to-equal '(5 1)))))
  (it "has none for a folder the fetch saved no scores for"
    (reddit-scores-tests--with-store
      (expect (reddit-scores "nnmaildir+reddit:r.test") :to-be nil)))
  (it "reads the file again only once the fetch saved it anew"
    (reddit-scores-tests--with-store
      (reddit-scores-tests--save '(("<p1@reddit.com>" 22 nil)))
      (let ((first (reddit-scores "nnmaildir+reddit:r.test")))
        (expect (reddit-scores "nnmaildir+reddit:r.test") :to-be first)
        (reddit-scores-tests--save '(("<p1@reddit.com>" 23 nil)))
        ;; a file system's clock can tick slower than the two saves
        (set-file-times (reddit-fetch-state-file reddit-maildir "r.test" reddit-fetch-scores-file)
                        (time-add nil 10))
        (expect (gethash "<p1@reddit.com>" (reddit-scores "nnmaildir+reddit:r.test"))
                :to-equal '(23 nil))))))

(describe "reddit-score-line"
  (it "shows a score dimmed, orange once the login voted up, blue once it voted down"
    (reddit-scores-tests--with-store
      (reddit-scores-tests--save '(("<c1.p1@reddit.com>" 5 1) ("<c2.p1@reddit.com>" 2 -1)
                                   ("<c3.p1@reddit.com>" 0 nil) ("<p1@reddit.com>" 22 nil)))
      (expect (reddit-scores-tests--line "<p1@reddit.com>") :to-equal '("↑22" reddit-score))
      (expect (reddit-scores-tests--line "<c1.p1@reddit.com>") :to-equal '("↑5" reddit-upvoted))
      (expect (reddit-scores-tests--line "<c2.p1@reddit.com>") :to-equal '("↑2" reddit-downvoted))
      (expect (reddit-scores-tests--line "<c3.p1@reddit.com>") :to-equal '("↑0" reddit-score))))
  (it "points the arrow down below zero, in a color of its own unless the login voted"
    (reddit-scores-tests--with-store
      (reddit-scores-tests--save '(("<c1.p1@reddit.com>" -32 nil) ("<c2.p1@reddit.com>" -2 -1)
                                   ("<c3.p1@reddit.com>" -1 1)))
      (expect (reddit-scores-tests--line "<c1.p1@reddit.com>")
              :to-equal '("↓32" reddit-negative-score))
      (expect (reddit-scores-tests--line "<c2.p1@reddit.com>") :to-equal '("↓2" reddit-downvoted))
      (expect (reddit-scores-tests--line "<c3.p1@reddit.com>") :to-equal '("↓1" reddit-upvoted))))
  (it "shows nothing of a score Reddit hides, only the login's vote on it"
    (reddit-scores-tests--with-store
      (reddit-scores-tests--save '(("<c1.p1@reddit.com>" nil nil) ("<c2.p1@reddit.com>" nil 1)
                                   ("<c3.p1@reddit.com>" nil -1)))
      (expect (reddit-scores-tests--line "<c1.p1@reddit.com>") :to-be nil)
      (expect (reddit-scores-tests--line "<c2.p1@reddit.com>") :to-equal '("↑" reddit-upvoted))
      (expect (reddit-scores-tests--line "<c3.p1@reddit.com>")
              :to-equal '("↓" reddit-downvoted))))
  (it "shows nothing in a folder without scores, for a message they lack, or in mail"
    (reddit-scores-tests--with-store
      (expect (reddit-scores-tests--line "<p1@reddit.com>") :to-be nil)
      (reddit-scores-tests--save '(("<p1@reddit.com>" 22 nil)))
      (expect (reddit-scores-tests--line "<p2@reddit.com>") :to-be nil)
      (let ((mail-thread-group "nnmaildir+gmail:inbox"))
        (expect (reddit-score-line (reddit-scores-tests--header "<p1@reddit.com>"))
                :to-be nil)))))

(describe "redraw-reddit-scores"
  (cl-flet ((redraw (group)
              ;; a buffer of its own, whatever views other suites left
              (let* ((mail-thread-buffer-name " *reddit-scores-tests view*")
                     (view (and group (get-buffer-create mail-thread-buffer-name)))
                     drawn)
                (unwind-protect
                    (progn
                      (when view
                        (with-current-buffer view
                          (setq-local mail-thread-group group)))
                      (cl-letf (((symbol-function 'mail-thread-draw-lines)
                                 (lambda () (push (buffer-name) drawn))))
                        (redraw-reddit-scores)))
                  (when view
                    (kill-buffer view)))
                drawn)))
    (it "draws the lines of the thread view again while it shows a Reddit thread"
      (expect (redraw "nnmaildir+reddit:r.test") :to-equal '(" *reddit-scores-tests view*")))
    (it "leaves a view of mail alone"
      (expect (redraw "nnmaildir+gmail:inbox") :to-be nil))
    (it "does nothing without a thread view"
      (expect (redraw nil) :to-be nil))
    (it "does nothing in an Emacs that loaded only the module's autoloads"
      ;; a fetch ends before any thread view opened, its file unloaded
      (let ((dir (make-temp-file "reddit-scores-tests" t)))
        (unwind-protect
            (let ((loaddefs (expand-file-name "email-loaddefs.el" dir))
                  (inhibit-message t))
              (loaddefs-generate (expand-file-name "modules/email/autoload/" test-config-root)
                                 loaddefs)
              (with-temp-buffer
                (expect (cons (call-process (expand-file-name invocation-name invocation-directory)
                                            nil t nil "-Q" "--batch" "--init-directory" dir
                                            ;; the autoloads name their files from dir
                                            "-L" dir "-l" loaddefs
                                            "--eval" "(redraw-reddit-scores)")
                              (buffer-string))
                        :to-equal '(0 . ""))))
          (delete-directory dir t))))))

(defmacro reddit-scores-tests--voting (&rest body)
  "Run BODY on r.test's message c1.p1 on screen, its votes left unanswered.
BODY sees `posts', each (PATH PARAMS CALLBACK), newest first, `said', what
the echo area got last, and `redraws', how often the view was drawn again."
  (declare (indent 0))
  `(reddit-scores-tests--with-store
     (let ((reddit-votes (make-hash-table :test #'equal))
           (header (reddit-scores-tests--header "<c1.p1@reddit.com>"))
           (redraws 0)
           posts said)
       (cl-letf (((symbol-function 'reddit-vote-target)
                  (lambda () (cons "nnmaildir+reddit:r.test" header)))
                 ((symbol-function 'reddit-api-post)
                  (lambda (path params callback) (push (list path params callback) posts)))
                 ((symbol-function 'redraw-reddit-scores) (lambda () (setq redraws (1+ redraws))))
                 ((symbol-function 'message)
                  (lambda (format &rest args) (setq said (apply #'format format args)))))
         ,@body))))

(defun reddit-scores-tests--dir (post)
  "The dir the vote POST asked for."
  (cdr (assoc "dir" (nth 1 post))))

(defun reddit-scores-tests--answer (post &optional error)
  "Answer the vote POST as Reddit would, refusing it with ERROR if given."
  (funcall (nth 2 post) (unless error (make-hash-table)) error))

(describe "reddit-upvote and reddit-downvote"
  (it "vote the message on screen through the write path, its line showing the vote at once"
    (reddit-scores-tests--voting
      (reddit-scores-tests--save '(("<c1.p1@reddit.com>" 4 nil)))
      (reddit-upvote)
      (expect (seq-take (car posts) 2)
              :to-equal '("/api/vote" (("id" . "t1_c1") ("dir" . "1"))))
      (expect (reddit-scores-tests--line "<c1.p1@reddit.com>") :to-equal '("↑5" reddit-upvoted))
      (expect redraws :to-equal 1)
      (reddit-scores-tests--answer (car posts))
      (expect said :to-equal "Upvoted")
      (expect (reddit-scores-tests--line "<c1.p1@reddit.com>") :to-equal '("↑5" reddit-upvoted))))
  (it "take a vote back on the same key, and turn it over on the other"
    (reddit-scores-tests--voting
      (reddit-scores-tests--save '(("<c1.p1@reddit.com>" 4 nil)))
      (reddit-upvote)
      (reddit-upvote)
      (expect (reddit-scores-tests--dir (car posts)) :to-equal "0")
      (expect (reddit-scores-tests--line "<c1.p1@reddit.com>") :to-equal '("↑4" reddit-score))
      (reddit-downvote)
      (expect (reddit-scores-tests--dir (car posts)) :to-equal "-1")
      (expect (reddit-scores-tests--line "<c1.p1@reddit.com>") :to-equal '("↑3" reddit-downvoted))
      (reddit-scores-tests--answer (car posts))
      (expect said :to-equal "Downvoted")))
  (it "take back a vote the scores file holds"
    (reddit-scores-tests--voting
      (reddit-scores-tests--save '(("<c1.p1@reddit.com>" 5 1)))
      (reddit-upvote)
      (expect (reddit-scores-tests--dir (car posts)) :to-equal "0")
      (expect (reddit-scores-tests--line "<c1.p1@reddit.com>") :to-equal '("↑4" reddit-score))
      (reddit-scores-tests--answer (car posts))
      (expect said :to-equal "Vote taken back")))
  (it "show the vote before again, and say why, when Reddit refuses"
    (reddit-scores-tests--voting
      (reddit-scores-tests--save '(("<c1.p1@reddit.com>" 4 nil)))
      (reddit-upvote)
      (reddit-scores-tests--answer (car posts) "HTTP 403: Forbidden")
      (expect (reddit-scores-tests--line "<c1.p1@reddit.com>") :to-equal '("↑4" reddit-score))
      (expect said :to-equal "Reddit refused the vote: HTTP 403: Forbidden")
      (expect redraws :to-equal 2)))
  (it "keep a vote made since when Reddit refuses an earlier one"
    (reddit-scores-tests--voting
      (reddit-scores-tests--save '(("<c1.p1@reddit.com>" 4 nil)))
      (reddit-upvote)
      (reddit-downvote)
      (reddit-scores-tests--answer (cadr posts) "RATELIMIT: Take a break")
      (expect (reddit-scores-tests--line "<c1.p1@reddit.com>")
              :to-equal '("↑3" reddit-downvoted))))
  (it "trust the scores file again once a fetch read the message anew"
    (reddit-scores-tests--voting
      (reddit-scores-tests--save '(("<c1.p1@reddit.com>" 4 nil)))
      (reddit-upvote)
      ;; others voted too meanwhile
      (reddit-scores-tests--save '(("<c1.p1@reddit.com>" 7 1)))
      (set-file-times (reddit-fetch-state-file reddit-maildir "r.test" reddit-fetch-scores-file)
                      (time-add nil 10))
      (expect (reddit-scores-tests--line "<c1.p1@reddit.com>") :to-equal '("↑7" reddit-upvoted))
      (expect (gethash "<c1.p1@reddit.com>" reddit-votes) :to-be nil)))
  (it "show a vote on a message the scores lack as its arrow alone"
    (reddit-scores-tests--voting
      (reddit-downvote)
      (expect (reddit-scores-tests--dir (car posts)) :to-equal "-1")
      (expect (reddit-scores-tests--line "<c1.p1@reddit.com>") :to-equal '("↓" reddit-downvoted))))
  (it "say so when no Reddit message is on screen"
    (reddit-scores-tests--voting
      (cl-letf (((symbol-function 'reddit-vote-target) #'ignore))
        (expect (reddit-upvote) :to-throw 'user-error '("No Reddit message here")))
      (expect posts :to-be nil)))
  (it "leave read marks alone"
    (reddit-scores-tests--voting
      (let (marked)
        (cl-letf (((symbol-function 'gnus-summary-mark-article)
                   (lambda (&rest args) (push args marked)))
                  ((symbol-function 'mail-thread-mark-read)
                   (lambda (&rest args) (push args marked))))
          (reddit-upvote)
          (reddit-scores-tests--answer (car posts)))
        (expect marked :to-be nil)))))

(describe "reddit-vote-target"
  (it "finds the message on screen through its summary, in a Reddit group only"
    (let ((summary (generate-new-buffer " *reddit-scores-tests summary*"))
          (header (reddit-scores-tests--header "<c1.p1@reddit.com>")))
      (unwind-protect
          (cl-letf (((symbol-function 'mail-on-screen) (lambda () (cons summary 7))))
            (with-current-buffer summary
              (setq-local gnus-newsgroup-data (list (gnus-data-make 7 gnus-unread-mark 1 header 0)))
              (setq-local gnus-newsgroup-name "nnmaildir+reddit:r.test"))
            (expect (reddit-vote-target) :to-equal (cons "nnmaildir+reddit:r.test" header))
            (with-current-buffer summary
              (setq-local gnus-newsgroup-name "nnmaildir+gmail:inbox"))
            (expect (reddit-vote-target) :to-be nil))
        (kill-buffer summary))))
  (it "finds nothing in an article buffer no summary shows"
    (cl-letf (((symbol-function 'mail-on-screen) (lambda () (cons nil nil))))
      (expect (reddit-vote-target) :to-be nil))))

(describe "the score faces"
  (it "draw a score smaller than its line, a vote or one below zero too"
    (dolist (face '(reddit-score reddit-upvoted reddit-downvoted reddit-negative-score))
      (expect (face-attribute face :height nil t) :to-be-less-than 1)))
  (it "draw an upvote in a muted orange, not Reddit's bright orangered"
    (expect (face-attribute 'reddit-upvoted :foreground nil t) :to-equal "#d19a66"))
  (it "give a score below zero a color none of the others has"
    (expect (length (seq-uniq (mapcar (lambda (face) (face-attribute face :foreground nil t))
                                      '(reddit-score reddit-upvoted reddit-downvoted
                                        reddit-negative-score))))
            :to-equal 4)))

;;; reddit-scores-tests.el ends here
