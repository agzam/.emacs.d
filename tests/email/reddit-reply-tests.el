;;; tests/email/reddit-reply-tests.el --- Reddit replies drafted in the thread view -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; config.el's, which reddit.el reads
(defvar reddit-maildir)
(defvar mail-groups)
(defvar mail-load-running)

(load-module-file "modules/email/autoload/thread.el")
(load-module-file "modules/email/autoload/reddit.el")
(load-module-file "modules/email/autoload/reddit-api.el")
(load-module-file "modules/email/autoload/reddit-reply.el")

(defun reddit-reply-tests--header (number id hour &optional references)
  "Header of article NUMBER of r.test, Message-ID ID, sent at HOUR.
REFERENCES names the messages it answers."
  (make-full-mail-header number (if references "Re: Reddit post" "Reddit post")
                         "\"u/ann\" <ann@reddit.invalid>"
                         (format "Mon, 28 Sep 2026 %02d:00:00 +0000" hour)
                         id references 0 0))

(defvar reddit-reply-tests--entries
  (list (cons 50 (reddit-reply-tests--header 50 "<p1@reddit.com>" 10))
        (cons 51 (reddit-reply-tests--header 51 "<c1.p1@reddit.com>" 11 "<p1@reddit.com>"))
        (cons 52 (reddit-reply-tests--header 52 "<c2.p1@reddit.com>" 12
                                             "<p1@reddit.com> <c1.p1@reddit.com>"))
        (cons 54 (reddit-reply-tests--header 54 "<c4.p1@reddit.com>" 14 "<p1@reddit.com>")))
  "A post, a comment on it, a reply to that comment, and a second comment.")

(defmacro reddit-reply-tests--with-view (entry &rest body)
  "Run BODY in a thread view of `reddit-reply-tests--entries' entered on ENTRY.
Rendering, marking read and the labels are stubbed; the view reads r.test."
  (declare (indent 1))
  `(let ((view (generate-new-buffer " *reddit-reply-tests*"))
         (summary (generate-new-buffer " *reddit-reply-tests summary*")))
     (unwind-protect
         (cl-letf (((symbol-function 'mail-thread-render)
                    (lambda (_group article) (format "body of %d" article)))
                   ((symbol-function 'mail-thread-mark-read) #'ignore)
                   ((symbol-function 'mail-label-tables) #'ignore)
                   ((symbol-function 'mail-label-chips-of) #'ignore)
                   ((symbol-function 'message) #'ignore))
           (with-current-buffer view
             (mail-thread-mode)
             (setq mail-thread-group "nnmaildir+reddit:r.test"
                   mail-thread-summary-buffer summary)
             (mail-thread-build reddit-reply-tests--entries ,entry nil)
             ,@body))
       (kill-buffer view)
       (kill-buffer summary))))

(defun reddit-reply-tests--message (article)
  "The message of ARTICLE in the view."
  (seq-find (lambda (message) (eql (mail-thread-message-article message) article))
            mail-thread-messages))

(defun reddit-reply-tests--reply-to (article)
  "Draft a reply to ARTICLE from its line, as r does."
  (goto-char (mail-thread-message-marker (reddit-reply-tests--message article)))
  (reply-on-reddit))

(defun reddit-reply-tests--prefix (pos)
  "Guide bars drawn left of the line at POS, without their face."
  (when-let* ((prefix (get-char-property pos 'line-prefix)))
    (substring-no-properties prefix)))

(defun reddit-reply-tests--text-bounds ()
  "Where the draft's text starts and ends."
  (let ((text (overlay-get mail-thread-draft 'reddit-reply-text)))
    (cons (overlay-start text) (overlay-end text))))

(defun reddit-reply-tests--header-line ()
  "The view's header line as text; batch Emacs formats no mode line."
  (if (stringp header-line-format)
      header-line-format
    (apply #'concat header-line-format)))

(describe "reply-on-reddit"
  (it "drafts right after the message's body, a level deeper, point in it"
    (reddit-reply-tests--with-view 52
      (let ((c2 (reddit-reply-tests--message 52))
            (c4 (reddit-reply-tests--message 54)))
        (reddit-reply-tests--reply-to 52)
        (expect (overlayp mail-thread-draft) :to-be t)
        (expect (buffer-substring-no-properties (mail-thread-body-start c2)
                                                (overlay-start mail-thread-draft))
                :to-equal "body of 52\n\n")
        (expect (buffer-substring-no-properties (overlay-start mail-thread-draft)
                                                (overlay-end mail-thread-draft))
                :to-equal "▼ Reply to u/ann\n\n\n")
        (expect (overlay-end mail-thread-draft) :to-equal (marker-position
                                                         (mail-thread-message-marker c4)))
        (expect (reddit-reply-tests--prefix (overlay-start mail-thread-draft))
                :to-equal "│ │ │ ")
        (expect (point) :to-equal (car (reddit-reply-tests--text-bounds)))
        (expect (reddit-reply-tests--prefix (point)) :to-equal "│ │ │ ")
        (expect (get-char-property (point) 'face) :to-be 'reddit-reply-draft)
        (expect (mail-thread-message-at-point) :to-be c2))))
  (it "edits in the open, a reply mode on and the header line naming whom it answers"
    (reddit-reply-tests--with-view 52
      (reddit-reply-tests--reply-to 52)
      (expect buffer-read-only :to-be nil)
      (expect reddit-reply-mode :to-be t)
      (expect (listp buffer-undo-list) :to-be t)
      (let ((line (reddit-reply-tests--header-line)))
        (expect line :to-match "Reply to u/ann")
        (expect line :to-match "C-c C-c submit")
        (expect line :to-match "C-c C-k abort"))))
  (it "renders a message it answers whose body was never shown, above the draft"
    (reddit-reply-tests--with-view 50
      (expect (mail-thread-rendered-p (reddit-reply-tests--message 52)) :to-be nil)
      (reddit-reply-tests--reply-to 52)
      (expect (mail-thread-message-open-p (reddit-reply-tests--message 52)) :to-be t)
      (expect (buffer-substring-no-properties
               (mail-thread-body-start (reddit-reply-tests--message 52))
               (overlay-start mail-thread-draft))
              :to-equal "body of 52\n\n")))
  (it "answers a post with a top-level comment, above the comments already there"
    (reddit-reply-tests--with-view 50
      (reddit-reply-tests--reply-to 50)
      (expect (overlay-end mail-thread-draft)
              :to-equal (marker-position (mail-thread-message-marker
                                          (reddit-reply-tests--message 51))))
      (expect (reddit-reply-tests--prefix (point)) :to-equal "│ ")))
  (it "goes to the draft already there instead of a second one"
    (reddit-reply-tests--with-view 52
      (reddit-reply-tests--reply-to 52)
      (let ((draft mail-thread-draft))
        (expect (reddit-reply-tests--reply-to 54) :to-throw 'user-error)
        (expect mail-thread-draft :to-be draft)
        (expect (point) :to-equal (car (reddit-reply-tests--text-bounds))))))
  (it "answers nothing outside a Reddit group"
    (reddit-reply-tests--with-view 52
      (setq mail-thread-group "nnmaildir+gmail:inbox")
      (expect (reddit-reply-tests--reply-to 52) :to-throw 'user-error)
      (expect mail-thread-draft :to-be nil)
      (expect buffer-read-only :to-be t))))

(describe "the reply draft"
  (it "takes typing, while every other line of the view refuses it"
    (reddit-reply-tests--with-view 52
      (let ((c2 (reddit-reply-tests--message 52))
            (c4 (reddit-reply-tests--message 54)))
        (reddit-reply-tests--reply-to 52)
        ;; self-insert-command inherits the text around it
        (insert-and-inherit "my repl")
        (insert-and-inherit "y")
        (backward-char 3)
        (insert-and-inherit "-")
        (expect (reddit-reply-text) :to-equal "my re-ply")
        (dolist (pos (list (mail-thread-body-start c2)
                           (overlay-start mail-thread-draft)
                           (mail-thread-message-marker c4)
                           (point-max)))
          (goto-char pos)
          (expect (insert-and-inherit "x") :to-throw 'text-read-only))
        (pcase-let ((`(,start . ,end) (reddit-reply-tests--text-bounds)))
          (expect (delete-region (1- start) start) :to-throw 'text-read-only)
          (expect (delete-region (1- end) (1+ end)) :to-throw 'text-read-only))
        (expect (reddit-reply-text) :to-equal "my re-ply"))))
  (it "keeps the next message on a line of its own whatever text goes"
    (reddit-reply-tests--with-view 52
      (let ((c4 (reddit-reply-tests--message 54)))
        (reddit-reply-tests--reply-to 52)
        (insert-and-inherit "first\nsecond")
        (pcase-let ((`(,start . ,end) (reddit-reply-tests--text-bounds)))
          (delete-region start end))
        (expect (reddit-reply-text) :to-equal "")
        (pcase-let ((`(,start . ,end) (reddit-reply-tests--text-bounds)))
          (expect (buffer-substring-no-properties start end) :to-equal "\n"))
        (expect (buffer-substring-no-properties (- (mail-thread-message-marker c4) 2)
                                                (mail-thread-message-marker c4))
                :to-equal "\n\n")
        (goto-char (car (reddit-reply-tests--text-bounds)))
        (insert-and-inherit "again")
        (expect (reddit-reply-text) :to-equal "again"))))
  (it "keeps no undo history of the view, only the draft's"
    (reddit-reply-tests--with-view 52
      (reddit-reply-tests--reply-to 52)
      (expect buffer-undo-list :to-be nil)
      (insert-and-inherit "typed")
      (primitive-undo 1 buffer-undo-list)
      (expect (reddit-reply-text) :to-equal ""))))

(defvar reddit-reply-tests--posted nil
  "Path and parameters the stand-in for `reddit-api-post' was given.")

(defvar reddit-reply-tests--callback nil
  "Callback the stand-in for `reddit-api-post' was given.")

(defun reddit-reply-tests--submit ()
  "Submit the draft, a stand-in keeping what would go to Reddit."
  (cl-letf (((symbol-function 'reddit-api-post)
             (lambda (path params callback)
               (setq reddit-reply-tests--posted (list path params)
                     reddit-reply-tests--callback callback))))
    (reddit-reply-submit)))

(describe "reddit-reply-submit"
  (before-each
    (setq reddit-reply-tests--posted nil
          reddit-reply-tests--callback nil))
  (it "posts the draft's text as a comment on the message it answers"
    (reddit-reply-tests--with-view 52
      (reddit-reply-tests--reply-to 52)
      (insert-and-inherit "  my reply\n")
      (reddit-reply-tests--submit)
      (expect reddit-reply-tests--posted
              :to-equal '("/api/comment" (("thing_id" . "t1_c2") ("text" . "my reply"))))
      (expect reddit-reply-posting :to-be t)
      (expect (reddit-reply-tests--header-line) :to-match "posting")
      ;; one post at a time, and nothing drops it meanwhile
      (expect (reddit-reply-tests--submit) :to-throw 'user-error)
      (expect (reddit-reply-abort) :to-throw 'user-error)))
  (it "answers a post on its t3 fullname"
    (reddit-reply-tests--with-view 50
      (reddit-reply-tests--reply-to 50)
      (insert-and-inherit "top-level")
      (reddit-reply-tests--submit)
      (expect (cdr (assoc "thing_id" (cadr reddit-reply-tests--posted))) :to-equal "t3_p1")))
  (it "posts no empty reply"
    (reddit-reply-tests--with-view 52
      (reddit-reply-tests--reply-to 52)
      (insert-and-inherit " \n ")
      (expect (reddit-reply-tests--submit) :to-throw 'user-error)
      (expect reddit-reply-tests--posted :to-be nil)
      (expect reddit-reply-posting :to-be nil)))
  (it "once Reddit took it, leaves the view as it was and fetches the thread anew"
    (reddit-reply-tests--with-view 52
      (let ((before (buffer-substring-no-properties (point-min) (point-max)))
            (reddit-thread-fetched-at (list (cons "p1" (float-time)) (cons "p9" (float-time))))
            fetched)
        (reddit-reply-tests--reply-to 52)
        (insert-and-inherit "my reply")
        (reddit-reply-tests--submit)
        (cl-letf (((symbol-function 'fetch-reddit-thread)
                   (lambda (group post)
                     (setq fetched (list group post (mapcar #'car reddit-thread-fetched-at))))))
          (funcall reddit-reply-tests--callback nil nil))
        (expect fetched :to-equal '("nnmaildir+reddit:r.test" "p1" ("p9")))
        (expect (buffer-substring-no-properties (point-min) (point-max)) :to-equal before)
        (expect mail-thread-draft :to-be nil)
        (expect reddit-reply-posting :to-be nil)
        (expect reddit-reply-mode :to-be nil)
        (expect buffer-read-only :to-be t)
        (expect buffer-undo-list :to-be t)
        (expect (text-property-not-all (point-min) (point-max) 'read-only nil) :to-be nil)
        (expect (stringp header-line-format) :to-be t)
        (expect (point) :to-equal (marker-position
                                   (mail-thread-message-marker (reddit-reply-tests--message 52)))))))
  (it "keeps the draft and says why when Reddit refused it"
    (reddit-reply-tests--with-view 52
      (reddit-reply-tests--reply-to 52)
      (insert-and-inherit "my reply")
      (reddit-reply-tests--submit)
      (let (said fetched)
        (cl-letf (((symbol-function 'fetch-reddit-thread) (lambda (&rest args) (setq fetched args)))
                  ((symbol-function 'message)
                   (lambda (format &rest args) (setq said (apply #'format format args)))))
          (funcall reddit-reply-tests--callback nil "RATELIMIT: Take a break for 9 minutes."))
        (expect said :to-equal "Reddit refused the reply: RATELIMIT: Take a break for 9 minutes.")
        (expect fetched :to-be nil))
      (expect (reddit-reply-text) :to-equal "my reply")
      (expect reddit-reply-posting :to-be nil)
      (expect (reddit-reply-tests--header-line) :to-match "C-c C-c submit")))
  (it "fetches every folder once a reply from the inbox or mine posted, as it lands in mine"
    (dolist (group '("nnmaildir+reddit:inbox" "nnmaildir+reddit:mine"))
      (reddit-reply-tests--with-view 52
        (setq mail-thread-group group)
        (reddit-reply-tests--reply-to 52)
        (insert-and-inherit "my reply")
        (reddit-reply-tests--submit)
        (let (calls)
          (cl-letf (((symbol-function 'fetch-reddit-thread)
                     (lambda (&rest args) (push (cons 'thread args) calls)))
                    ((symbol-function 'fetch-reddit) (lambda () (push '(all) calls))))
            (funcall reddit-reply-tests--callback nil nil))
          (expect calls :to-equal '((all)))
          (expect mail-thread-draft :to-be nil)))))
  (it "fetches the thread even when the view went meanwhile"
    (let (gone fetched)
      (reddit-reply-tests--with-view 52
        (setq gone (current-buffer))
        (reddit-reply-tests--reply-to 52)
        (insert-and-inherit "my reply")
        (reddit-reply-tests--submit))
      (expect (buffer-live-p gone) :to-be nil)
      (cl-letf (((symbol-function 'fetch-reddit-thread) (lambda (&rest args) (setq fetched args)))
                ((symbol-function 'message) #'ignore))
        (funcall reddit-reply-tests--callback nil nil))
      (expect fetched :to-equal '("nnmaildir+reddit:r.test" "p1")))))

(describe "reddit-reply-abort"
  (it "drops an empty draft unasked, and the view reads as before"
    (reddit-reply-tests--with-view 52
      (let ((before (buffer-substring-no-properties (point-min) (point-max)))
            asked)
        (reddit-reply-tests--reply-to 52)
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) (setq asked t))))
          (reddit-reply-abort))
        (expect asked :to-be nil)
        (expect (buffer-substring-no-properties (point-min) (point-max)) :to-equal before)
        (expect buffer-read-only :to-be t)
        (expect reddit-reply-mode :to-be nil))))
  (it "asks before it drops typed text, which a no keeps"
    (reddit-reply-tests--with-view 52
      (reddit-reply-tests--reply-to 52)
      (insert-and-inherit "my reply")
      (cl-letf (((symbol-function 'yes-or-no-p) #'ignore))
        (reddit-reply-abort))
      (expect (reddit-reply-text) :to-equal "my reply")
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (reddit-reply-abort))
      (expect mail-thread-draft :to-be nil)))
  (it "lays the thread out when a reload waited for the draft"
    (reddit-reply-tests--with-view 52
      (reddit-reply-tests--reply-to 52)
      (setq mail-thread-reload-waiting t)
      (let (reloaded)
        (cl-letf (((symbol-function 'mail-thread-reload) (lambda () (setq reloaded t))))
          (reddit-reply-abort))
        (expect reloaded :to-be t)
        (expect mail-thread-reload-waiting :to-be nil)))))

(describe "reddit-on-screen-p"
  (it "tells a Reddit group's thread view, summary and article from mail's"
    (reddit-reply-tests--with-view 52
      (expect (reddit-on-screen-p) :to-be-truthy)
      (setq mail-thread-group "nnmaildir+gmail:inbox")
      (expect (reddit-on-screen-p) :to-be nil))
    (with-temp-buffer
      (setq major-mode 'gnus-summary-mode)
      (let ((gnus-newsgroup-name "nnmaildir+reddit:r.test"))
        (expect (reddit-on-screen-p) :to-be-truthy))
      (let ((gnus-newsgroup-name "nnmaildir+gmail:inbox"))
        (expect (reddit-on-screen-p) :to-be nil))
      (setq major-mode 'gnus-article-mode)
      (let ((gnus-article-current '("nnmaildir+reddit:r.test" . 52)))
        (expect (reddit-on-screen-p) :to-be-truthy)))
    (with-temp-buffer
      (let ((gnus-newsgroup-name "nnmaildir+reddit:r.test"))
        (expect (reddit-on-screen-p) :to-be nil)))))

(describe "reddit-reply-mode-map"
  (it "posts on C-c C-c and drops on C-c C-k"
    (expect (keymap-lookup reddit-reply-mode-map "C-c C-c") :to-be 'reddit-reply-submit)
    (expect (keymap-lookup reddit-reply-mode-map "C-c C-k") :to-be 'reddit-reply-abort))
  (it "types the keys special-mode takes from typing, and deletes on DEL"
    (expect (keymap-lookup reddit-reply-mode-map "<remap> <self-insert-command>")
            :to-be 'self-insert-command)
    (dolist (key '("q" "g" "<" ">" "-" "1" "9"))
      (expect (keymap-lookup reddit-reply-mode-map key) :to-be 'self-insert-command))
    (expect (keymap-lookup reddit-reply-mode-map "DEL") :to-be 'delete-backward-char)))

;;; reddit-reply-tests.el ends here
