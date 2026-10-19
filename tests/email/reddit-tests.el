;;; tests/email/reddit-tests.el --- Reddit fetch specs, the session side -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; each spec binds these, so a suite loaded later still sees config.el's
(defvar reddit-maildir)
(defvar mail-groups)
(defvar mail-load-running)
;; the thread view's, which thread-tests loads
(defvar mail-thread-buffer-name "*mail thread*")
(defvar mail-thread-group nil)
(defvar mail-thread-summary-buffer nil)

(load-module-file "modules/email/autoload/reddit.el")

(defmacro reddit-tests--with-store (&rest body)
  "Run BODY with an empty `reddit-maildir' and the fetch's paths in a temp dir."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "reddit-tests" t)))
          (reddit-maildir (expand-file-name "reddit/" root))
          (reddit-fetch-directory (expand-file-name "cache/reddit-fetch/" root))
          (reddit-url "http://reddit.test")
          (mail-groups '("nnmaildir+gmail:inbox" "nnmaildir+reddit:r.test"))
          (reddit-fetch-process nil)
          (reddit-fetch-pending nil)
          (reddit-thread-fetched-at nil)
          (reddit-thread-pending nil)
          (mail-load-running nil)
          (reddit-fetched-at nil))
     (unwind-protect (progn ,@body)
       (when (process-live-p reddit-fetch-process)
         (delete-process reddit-fetch-process))
       (delete-directory root t))))

(defun reddit-tests--stand-in (root &optional exit output)
  "A reddit-fetch script under ROOT that waits for ROOT/go, then exits EXIT.
It records its arguments and whether its input ended in ROOT/ran, and
says OUTPUT before it exits."
  (let ((script (expand-file-name "stand-in.el" root)))
    (with-temp-file script
      (prin1 `(defun reddit-fetch-main (&rest args)
                (let ((eof (condition-case nil (progn (read-from-minibuffer "") nil)
                             (error t))))
                  (while (not (file-exists-p ,(expand-file-name "go" root)))
                    (sleep-for 0.05))
                  (with-temp-file ,(expand-file-name "ran" root)
                    (prin1 (list args eof) (current-buffer))))
                (when ,output
                  (message "%s" ,output))
                (kill-emacs ,(or exit 0)))
             (current-buffer)))
    script))

(defun reddit-tests--ran (root)
  "What the stand-in under ROOT recorded, or nil before it ran."
  (let ((file (expand-file-name "ran" root)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (read (current-buffer))))))

(defun reddit-tests--wait-exit ()
  "Wait up to 30 s for `reddit-fetch-process' to end and its sentinel to run."
  (with-timeout (30)
    (while (or (process-live-p reddit-fetch-process)
               (not (memq (process-status reddit-fetch-process) '(exit signal))))
      (accept-process-output nil 0.05)))
  ;; the sentinel runs once more output is read
  (accept-process-output nil 0.1))

(describe "reddit-group-p"
  (it "tells the groups of the Reddit store from every other"
    (expect (reddit-group-p "nnmaildir+reddit:r.emacs") :to-be-truthy)
    (dolist (group '("nnmaildir+gmail:inbox" "nnmaildir+news:gmane.emacs.devel"
                     "nnatom+www.reddit.com/r/emacs/new/.rss:r/emacs"))
      (expect (reddit-group-p group) :to-be nil))))

(describe "ensure-reddit-folders"
  (it "creates a maildir for each Reddit group mail-groups names, and nothing else"
    (reddit-tests--with-store
      (ensure-reddit-folders)
      (expect (directory-files reddit-maildir nil "\\`[^.]") :to-equal '("r.test"))
      (expect (directory-files (expand-file-name "r.test" reddit-maildir) nil "\\`[^.]")
              :to-equal '("cur" "new" "tmp"))
      (ensure-reddit-folders)
      (expect (directory-files reddit-maildir nil "\\`[^.]") :to-equal '("r.test")))))

(describe "reddit-fetch-command"
  (it "runs the fetch script in a batch Emacs of its own, sandboxed"
    (reddit-tests--with-store
      (let ((command (reddit-fetch-command)))
        (expect (car command) :to-equal (expand-file-name invocation-name invocation-directory))
        (expect (seq-take (cdr command) 4)
                :to-equal (list "-Q" "--batch" "--init-directory" reddit-fetch-directory))
        (expect (cadr (member "-l" command)) :to-equal reddit-fetch-script)
        (expect (car (last command))
                :to-equal (format "(reddit-fetch-main %S %S)" reddit-maildir "http://reddit.test"))
        ;; the fetch needs no package of this session
        (expect (member "-L" command) :to-be nil))))
  (it "limits the fetch to the thread it is given"
    (reddit-tests--with-store
      (expect (car (last (reddit-fetch-command '("nnmaildir+reddit:r.test" . "p1"))))
              :to-equal (format "(reddit-fetch-main %S %S '((\"r.test\" \"p1\")))"
                                reddit-maildir "http://reddit.test")))))

(describe "reddit-post-id"
  (it "reads the post of a post's or a comment's Message-ID, and none of other mail"
    (expect (reddit-post-id "<p1@reddit.com>") :to-equal "p1")
    (expect (reddit-post-id "<c2.p1@reddit.com>") :to-equal "p1")
    (expect (reddit-post-id "<1@x>") :to-be nil)
    (expect (reddit-post-id nil) :to-be nil)))

(describe "fetch-reddit-thread"
  (it "fetches the thread in the background, and not again within the interval"
    (reddit-tests--with-store
      (let* ((root (file-name-directory (directory-file-name reddit-maildir)))
             (reddit-fetch-script (reddit-tests--stand-in root))
             (reddit-thread-fetch-interval 60))
        (write-region "" nil (expand-file-name "go" root))
        (fetch-reddit-thread "nnmaildir+reddit:r.test" "p1")
        (expect (process-live-p reddit-fetch-process) :to-be-truthy)
        (reddit-tests--wait-exit)
        (expect (car (reddit-tests--ran root))
                :to-equal (list reddit-maildir "http://reddit.test" '(("r.test" "p1"))))
        (delete-file (expand-file-name "ran" root))
        (fetch-reddit-thread "nnmaildir+reddit:r.test" "p1")
        (expect (process-live-p reddit-fetch-process) :to-be nil)
        ;; a thread fetched an interval ago is fetched again
        (setf (alist-get "p1" reddit-thread-fetched-at nil nil #'equal) (- (float-time) 61))
        (fetch-reddit-thread "nnmaildir+reddit:r.test" "p1")
        (expect (process-live-p reddit-fetch-process) :to-be-truthy))))
  (it "waits for a running fetch, whose end starts it"
    (reddit-tests--with-store
      (let* ((root (file-name-directory (directory-file-name reddit-maildir)))
             (reddit-fetch-script (reddit-tests--stand-in root)))
        (fetch-reddit)
        (let ((first reddit-fetch-process))
          (fetch-reddit-thread "nnmaildir+reddit:r.test" "p1")
          (expect reddit-fetch-process :to-be first)
          (expect reddit-thread-pending :to-equal '("nnmaildir+reddit:r.test" . "p1"))
          (write-region "" nil (expand-file-name "go" root))
          (cl-letf (((symbol-function 'gnus-alive-p) #'ignore))
            (reddit-tests--wait-exit))
          (expect reddit-fetch-process :not :to-be first)
          (expect reddit-thread-pending :to-be nil)))))
  (it "waits for a batch reading a Reddit group, whose end starts it"
    (reddit-tests--with-store
      (let ((mail-load-running '("nnmaildir+reddit:r.test")))
        (fetch-reddit-thread "nnmaildir+reddit:r.test" "p1")
        (expect reddit-fetch-process :to-be nil)
        (expect reddit-thread-pending :to-equal '("nnmaildir+reddit:r.test" . "p1")))
      (let (started)
        (cl-letf (((symbol-function 'start-reddit-fetch)
                   (lambda (command _sentinel) (setq started command))))
          (fetch-pending-reddit-thread))
        (expect (car (last started)) :to-match "\"p1\"")
        (expect reddit-thread-pending :to-be nil)))))

(describe "reddit-thread-sentinel"
  (cl-flet ((fetch (output exit)
              (let* ((root (file-name-directory (directory-file-name reddit-maildir)))
                     (reddit-fetch-script (reddit-tests--stand-in root exit output))
                     calls)
                (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                          ((symbol-function 'load-mail-groups)
                           (lambda (&rest args) (push (cons 'load args) calls)))
                          ((symbol-function 'wait-for-mail-load)
                           (lambda (key _what _predicate action)
                             (push (list 'wait key) calls)
                             (funcall action)))
                          ((symbol-function 'show-reddit-replies)
                           (lambda (&rest args) (push (cons 'show args) calls)))
                          ((symbol-function 'message)
                           (lambda (format &rest args)
                             (push (list 'said (apply #'format format args)) calls))))
                  (write-region "" nil (expand-file-name "go" root))
                  (fetch-reddit-thread "nnmaildir+reddit:r.test" "p1")
                  (reddit-tests--wait-exit))
                (nreverse calls))))
    (it "reads the group ahead of the others once replies came, then shows them"
      (reddit-tests--with-store
        (expect (fetch "r.test: 0 posts, 2 comments, 0 threads waiting" 0)
                :to-equal '((load ("nnmaildir+reddit:r.test") t)
                            (wait reddit-thread)
                            (show "nnmaildir+reddit:r.test" "p1" 2)))))
    (it "reads nothing when the fetch brought nothing"
      (reddit-tests--with-store
        (expect (fetch "r.test: 0 posts, 0 comments, 0 threads waiting" 0) :to-equal nil)))
    (it "says a fetch failed, and still shows what it brought first"
      (reddit-tests--with-store
        (let ((calls (fetch "r.test: 0 posts, 1 comments, 1 threads waiting; blocked" 1)))
          (expect (cadr (assq 'said calls)) :to-match "Reddit thread fetch failed")
          (expect (assq 'show calls) :to-equal '(show "nnmaildir+reddit:r.test" "p1" 1)))))))

(describe "show-reddit-replies"
  (cl-flet ((show (ids displayed)
              (let ((summary (get-buffer-create "*Summary nnmaildir+reddit:r.test*"))
                    (view (get-buffer-create mail-thread-buffer-name))
                    calls)
                (unwind-protect
                    (save-window-excursion
                      (with-current-buffer view
                        (setq-local mail-thread-summary-buffer summary))
                      (when displayed
                        (set-window-buffer (selected-window) view))
                      (cl-letf (((symbol-function 'gnus-summary-insert-new-articles)
                                 (lambda () (push (list 'insert (buffer-name)) calls)))
                                ((symbol-function 'mail-thread-message-ids) (lambda () ids))
                                ((symbol-function 'mail-thread-reload)
                                 (lambda () (push (list 'reload (buffer-name)) calls)))
                                ((symbol-function 'message)
                                 (lambda (format &rest args)
                                   (push (list 'said (apply #'format format args)) calls))))
                        (show-reddit-replies "nnmaildir+reddit:r.test" "p1" 2)))
                  (kill-buffer summary)
                  (kill-buffer view))
                (nreverse calls))))
    (it "has the summary take the replies in, and the view showing the thread lay it out again"
      (expect (show '("<p1@reddit.com>" "<c1.p1@reddit.com>") t)
              :to-equal `((insert "*Summary nnmaildir+reddit:r.test*")
                          (reload ,mail-thread-buffer-name)
                          (said "2 new replies"))))
    (it "leaves alone a view that shows another thread"
      (expect (show '("<p2@reddit.com>") t)
              :to-equal '((insert "*Summary nnmaildir+reddit:r.test*"))))
    (it "leaves alone a view no window shows: laying it out renders, and marks read, unseen"
      (expect (show '("<p1@reddit.com>") nil)
              :to-equal '((insert "*Summary nnmaildir+reddit:r.test*"))))))

(describe "fetch-reddit-thread-h"
  (it "fetches the thread a view opened from a Reddit group, and nothing for mail"
    (let (fetched)
      (cl-letf (((symbol-function 'fetch-reddit-thread)
                 (lambda (&rest args) (push args fetched)))
                ((symbol-function 'mail-thread-message-ids)
                 (lambda () '("<p1@reddit.com>" "<c1.p1@reddit.com>"))))
        (let ((mail-thread-group "nnmaildir+reddit:r.test"))
          (fetch-reddit-thread-h))
        (let ((mail-thread-group "nnmaildir+gmail:inbox"))
          (fetch-reddit-thread-h)))
      (expect fetched :to-equal '(("nnmaildir+reddit:r.test" "p1"))))))

(describe "fetch-reddit"
  (it "fetches in the background, never twice at once, and the fetch reads no input"
    (reddit-tests--with-store
      (let* ((root (file-name-directory (directory-file-name reddit-maildir)))
             (reddit-fetch-script (reddit-tests--stand-in root))
             (start (float-time)))
        (fetch-reddit)
        (expect (< (- (float-time) start) 0.5) :to-be t)
        (let ((first reddit-fetch-process))
          (expect (process-live-p first) :to-be-truthy)
          (fetch-reddit)
          (expect reddit-fetch-process :to-be first)
          (expect (seq-count (lambda (process) (equal (process-name process) "reddit-fetch"))
                             (process-list))
                  :to-equal 1))
        ;; the fetch fills only the folders it finds
        (expect (file-directory-p (expand-file-name "r.test/cur" reddit-maildir)) :to-be t)
        (expect (file-directory-p reddit-fetch-directory) :to-be t)
        (write-region "" nil (expand-file-name "go" root))
        (reddit-tests--wait-exit)
        (pcase-let ((`(,args ,eof) (reddit-tests--ran root)))
          (expect args :to-equal (list reddit-maildir "http://reddit.test"))
          (expect eof :to-be t))))))

(describe "fetch-reddit while a Reddit group loads"
  (it "waits for the batch reading it, which starts the fetch when done"
    ;; both would number the messages the fetch delivers
    (reddit-tests--with-store
      (let ((mail-load-running (list "nnmaildir+gmail:inbox" "nnmaildir+reddit:r.test")))
        (fetch-reddit)
        (expect reddit-fetch-process :to-be nil)
        (expect reddit-fetch-pending :to-be t))
      (let ((made nil))
        (cl-letf (((symbol-function 'make-process) (lambda (&rest _) (setq made t) 'fetch))
                  ((symbol-function 'process-send-eof) #'ignore))
          (fetch-pending-reddit))
        (expect made :to-be t)
        (expect reddit-fetch-pending :to-be nil)))))

(describe "fetch-pending-reddit"
  (it "starts no fetch unless a load held one back"
    (let ((reddit-fetch-pending nil) fetched)
      (cl-letf (((symbol-function 'fetch-reddit) (lambda () (setq fetched t))))
        (fetch-pending-reddit))
      (expect fetched :to-be nil))))

(describe "reddit-group-parked-p"
  (it "holds a Reddit group back only while a Reddit fetch runs"
    (let ((reddit-fetch-process 'fetch))
      (cl-letf (((symbol-function 'process-live-p) (lambda (process) (eq process 'fetch))))
        (expect (reddit-group-parked-p "nnmaildir+reddit:r.test") :to-be-truthy)
        (expect (reddit-group-parked-p "nnmaildir+news:gmane.test") :to-be nil))
      (cl-letf (((symbol-function 'process-live-p) #'ignore))
        (expect (reddit-group-parked-p "nnmaildir+reddit:r.test") :to-be nil)))))

(describe "reddit-fetch-sentinel"
  (it "reads every Reddit group again in the background once a fetch ends well"
    (reddit-tests--with-store
      (let* ((root (file-name-directory (directory-file-name reddit-maildir)))
             (reddit-fetch-script (reddit-tests--stand-in root 0))
             (gnus-group-list '("nnmaildir+gmail:inbox" "nnmaildir+reddit:r.test"))
             asked)
        (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                  ((symbol-function 'load-mail-groups) (lambda (&rest args) (push args asked))))
          (write-region "" nil (expand-file-name "go" root))
          (fetch-reddit)
          (reddit-tests--wait-exit))
        (expect asked :to-equal '((("nnmaildir+reddit:r.test"))))
        (expect (numberp reddit-fetched-at) :to-be t))))
  (it "says a fetch failed, and still reads for what it delivered first"
    (reddit-tests--with-store
      (let* ((root (file-name-directory (directory-file-name reddit-maildir)))
             (reddit-fetch-script (reddit-tests--stand-in root 1))
             (gnus-group-list '("nnmaildir+reddit:r.test"))
             asked said)
        (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                  ((symbol-function 'load-mail-groups) (lambda (&rest args) (push args asked)))
                  ((symbol-function 'message)
                   (lambda (format &rest args) (setq said (apply #'format format args)))))
          (write-region "" nil (expand-file-name "go" root))
          (fetch-reddit)
          (reddit-tests--wait-exit))
        (expect asked :to-equal '((("nnmaildir+reddit:r.test"))))
        (expect said :to-match "Reddit fetch failed")
        (expect reddit-fetched-at :to-be nil)))))

(describe "fetch-stale-reddit"
  (it "fetches when no fetch ended well within the interval"
    (let ((reddit-fetch-interval 1800) fetched)
      (cl-letf (((symbol-function 'fetch-reddit) (lambda () (setq fetched t))))
        (let ((reddit-fetched-at nil))
          (fetch-stale-reddit))
        (expect fetched :to-be t)
        (setq fetched nil)
        (let ((reddit-fetched-at (- (float-time) 3600)))
          (fetch-stale-reddit))
        (expect fetched :to-be t)
        (setq fetched nil)
        (let ((reddit-fetched-at (- (float-time) 60)))
          (fetch-stale-reddit))
        (expect fetched :to-be nil)))))

(describe "start-reddit-fetch-timer"
  (it "sets one repeating fetch, the first an interval away, and none in batch"
    (let ((reddit-fetch-timer nil)
          (reddit-fetch-interval 1800))
      (unwind-protect
          (progn
            (start-reddit-fetch-timer)
            (expect reddit-fetch-timer :to-be nil)
            (let ((noninteractive nil))
              (start-reddit-fetch-timer)
              (start-reddit-fetch-timer))
            (expect (timerp reddit-fetch-timer) :to-be t)
            (expect (timer--repeat-delay reddit-fetch-timer) :to-equal 1800)
            (expect (< 1700 (float-time (time-subtract (timer--time reddit-fetch-timer) nil)))
                    :to-be t)
            (expect (seq-count (lambda (timer) (eq (timer--function timer) #'fetch-reddit))
                               timer-list)
                    :to-equal 1))
        (when (timerp reddit-fetch-timer)
          (cancel-timer reddit-fetch-timer))))))

;;; reddit-tests.el ends here
