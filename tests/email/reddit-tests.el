;;; tests/email/reddit-tests.el --- Reddit fetch specs, the session side -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; each spec binds these, so a suite loaded later still sees config.el's
(defvar reddit-maildir)
(defvar reddit-topic)
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
                :to-equal (format "(reddit-fetch-main %S %S '((\"r.test\")))"
                                  reddit-maildir "http://reddit.test"))
        ;; the fetch needs no package of this session
        (expect (member "-L" command) :to-be nil))))
  (it "fetches only the folders mail-groups names, not one made for a single thread"
    (reddit-tests--with-store
      (let ((mail-groups '("nnmaildir+gmail:inbox" "nnmaildir+reddit:r.test"
                           "nnmaildir+reddit:inbox" "nnmaildir+reddit:mine")))
        (make-directory (expand-file-name "r.other/cur" reddit-maildir) t)
        (expect (car (last (reddit-fetch-command)))
                :to-equal (format "(reddit-fetch-main %S %S '((\"r.test\") (\"inbox\") (\"mine\")))"
                                  reddit-maildir "http://reddit.test")))))
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
                          ((symbol-function 'forget-shown-articles)
                           (lambda () (push '(forget) calls)))
                          ((symbol-function 'message)
                           (lambda (format &rest args)
                             (push (list 'said (apply #'format format args)) calls))))
                  (write-region "" nil (expand-file-name "go" root))
                  (fetch-reddit-thread "nnmaildir+reddit:r.test" "p1")
                  (reddit-tests--wait-exit))
                (nreverse calls))))
    (it "reads the group ahead of the others once replies came, then shows them"
      (reddit-tests--with-store
        (expect (fetch "r.test: 0 posts, 2 comments, 0 edits, 0 threads waiting" 0)
                :to-equal '((load ("nnmaildir+reddit:r.test") t)
                            (wait reddit-thread)
                            (show "nnmaildir+reddit:r.test" "p1" 2 0 nil)))))
    (it "reads the group and shows the thread for edits alone, Gnus's copies dropped first"
      (reddit-tests--with-store
        (expect (fetch "r.test: 0 posts, 0 comments, 2 edits, 0 threads waiting" 0)
                :to-equal '((forget)
                            (load ("nnmaildir+reddit:r.test") t)
                            (wait reddit-thread)
                            (show "nnmaildir+reddit:r.test" "p1" 0 2 nil)))))
    (it "reads nothing when the fetch brought nothing"
      (reddit-tests--with-store
        (expect (fetch "r.test: 0 posts, 0 comments, 0 edits, 0 threads waiting" 0)
                :to-equal nil)))
    (it "reads and shows a thread the user asked for, though the fetch brought nothing"
      (reddit-tests--with-store
        (let ((reddit-thread-asked "p1"))
          (expect (fetch "r.test: 0 posts, 0 comments, 0 edits, 0 threads waiting" 0)
                  :to-equal '((load ("nnmaildir+reddit:r.test") t)
                              (wait reddit-thread)
                              (show "nnmaildir+reddit:r.test" "p1" 0 0 t)))
          (expect reddit-thread-asked :to-be nil))))
    (it "shows nothing more for a failed fetch the user asked for, which says it failed"
      (reddit-tests--with-store
        (let* ((reddit-thread-asked "p1")
               (calls (fetch "r.test: 0 posts, 0 comments, 0 edits, 1 threads waiting; blocked" 1)))
          (expect (mapcar #'car calls) :to-equal '(said))
          (expect (cadr (car calls)) :to-match "Reddit thread fetch failed"))))
    (it "leaves the ask of another thread for that thread's fetch"
      (reddit-tests--with-store
        (let ((reddit-thread-asked "p2"))
          (expect (fetch "r.test: 0 posts, 0 comments, 0 edits, 0 threads waiting" 0)
                  :to-equal nil)
          (expect reddit-thread-asked :to-equal "p2"))))
    (it "says a fetch failed, and still shows what it brought first"
      (reddit-tests--with-store
        (let ((calls (fetch "r.test: 0 posts, 1 comments, 0 edits, 1 threads waiting; blocked" 1)))
          (expect (cadr (assq 'said calls)) :to-match "Reddit thread fetch failed")
          (expect (assq 'show calls) :to-equal '(show "nnmaildir+reddit:r.test" "p1" 1 0 nil)))))
    (it "runs the end hook once the fetch ended, though it brought nothing"
      (reddit-tests--with-store
        (let* ((ended 0)
               (reddit-fetch-end-hook (list (lambda () (setq ended (1+ ended))))))
          (fetch "r.test: 0 posts, 0 comments, 0 edits, 0 threads waiting" 0)
          (expect ended :to-equal 1))))
    (it "calls the function waiting on the thread once its group is read, though nothing came"
      (reddit-tests--with-store
        (let* ((ran 0)
               (reddit-thread-then (cons "p1" (lambda () (setq ran (1+ ran))))))
          (expect (fetch "r.test: 0 posts, 0 comments, 0 edits, 0 threads waiting" 0)
                  :to-equal '((load ("nnmaildir+reddit:r.test") t)
                              (wait reddit-thread)
                              (show "nnmaildir+reddit:r.test" "p1" 0 0 nil)))
          (expect ran :to-equal 1)
          (expect reddit-thread-then :to-be nil))))
    (it "drops the function waiting on the thread when the fetch failed, which says so"
      (reddit-tests--with-store
        (let* ((ran 0)
               (reddit-thread-then (cons "p1" (lambda () (setq ran (1+ ran)))))
               (calls (fetch "r.test: 0 posts, 0 comments, 0 edits, 1 threads waiting; blocked" 1)))
          (expect (mapcar #'car calls) :to-equal '(said))
          (expect ran :to-equal 0)
          (expect reddit-thread-then :to-be nil))))
    (it "leaves the function waiting on another thread to that thread's fetch"
      (reddit-tests--with-store
        (let* ((ran 0)
               (reddit-thread-then (cons "p2" (lambda () (setq ran (1+ ran))))))
          (fetch "r.test: 0 posts, 0 comments, 0 edits, 0 threads waiting" 0)
          (expect ran :to-equal 0)
          (expect (car reddit-thread-then) :to-equal "p2"))))))

(describe "show-reddit-replies"
  (cl-flet ((show (ids displayed count edits &optional asked)
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
                        (show-reddit-replies "nnmaildir+reddit:r.test" "p1" count edits asked)))
                  (kill-buffer summary)
                  (kill-buffer view))
                (nreverse calls))))
    (it "has the summary take the replies in, and the view showing the thread lay it out again"
      (expect (show '("<p1@reddit.com>" "<c1.p1@reddit.com>") t 2 0)
              :to-equal `((insert "*Summary nnmaildir+reddit:r.test*")
                          (reload ,mail-thread-buffer-name)
                          (said "2 new replies"))))
    (it "lays the thread out again for edits alone, the summary having nothing to take in"
      (expect (show '("<p1@reddit.com>") t 0 1)
              :to-equal `((reload ,mail-thread-buffer-name)
                          (said "1 edited"))))
    (it "says both what came and what was edited"
      (expect (cdr (assq 'said (show '("<p1@reddit.com>") t 1 2)))
              :to-equal '("1 new reply, 2 edited")))
    (it "leaves alone a view that shows another thread"
      (expect (show '("<p2@reddit.com>") t 2 0)
              :to-equal '((insert "*Summary nnmaildir+reddit:r.test*"))))
    (it "leaves alone a view no window shows: laying it out renders, and marks read, unseen"
      (expect (show '("<p1@reddit.com>") nil 2 0)
              :to-equal '((insert "*Summary nnmaildir+reddit:r.test*"))))
    (it "takes in and lays out what the store holds for a fetch the user asked for, though it brought nothing"
      ;; an earlier fetch may have filled the store
      (expect (show '("<p1@reddit.com>") t 0 0 t)
              :to-equal `((insert "*Summary nnmaildir+reddit:r.test*")
                          (reload ,mail-thread-buffer-name)
                          (said "The thread is up to date"))))))

(describe "subreddit-group-p"
  (it "tells a subreddit's group from the inbox, mine and every other"
    (expect (subreddit-group-p "nnmaildir+reddit:r.emacs") :to-be-truthy)
    (dolist (group '("nnmaildir+reddit:inbox" "nnmaildir+reddit:mine" "nnmaildir+gmail:inbox"
                     "nnmaildir+news:gmane.emacs.devel"))
      (expect (subreddit-group-p group) :to-be nil))))

(describe "fetch-reddit-thread-h"
  (it "fetches the thread a view opened from a subreddit, and nothing for the inbox, mine or mail"
    (let (fetched)
      (cl-letf (((symbol-function 'fetch-reddit-thread)
                 (lambda (&rest args) (push args fetched)))
                ((symbol-function 'mail-thread-message-ids)
                 (lambda () '("<p1@reddit.com>" "<c1.p1@reddit.com>"))))
        (dolist (group '("nnmaildir+reddit:r.test" "nnmaildir+reddit:inbox"
                         "nnmaildir+reddit:mine" "nnmaildir+gmail:inbox"))
          (let ((mail-thread-group group))
            (fetch-reddit-thread-h))))
      (expect fetched :to-equal '(("nnmaildir+reddit:r.test" "p1"))))))

(describe "refetch-reddit-thread"
  (it "fetches the view's thread at once, though its opening fetched it, to show it whatever came"
    (reddit-tests--with-store
      (let* ((root (file-name-directory (directory-file-name reddit-maildir)))
             (reddit-fetch-script (reddit-tests--stand-in root))
             (reddit-thread-asked nil)
             (reddit-thread-fetched-at (list (cons "p1" (float-time)))))
        (cl-letf (((symbol-function 'mail-thread-message-ids)
                   (lambda () '("<p1@reddit.com>" "<c1.p1@reddit.com>")))
                  ((symbol-function 'message) #'ignore)
                  ((symbol-function 'gnus-alive-p) #'ignore))
          (let ((mail-thread-group "nnmaildir+reddit:r.test"))
            (refetch-reddit-thread))
          (expect (process-live-p reddit-fetch-process) :to-be-truthy)
          (expect reddit-thread-asked :to-equal "p1")
          (write-region "" nil (expand-file-name "go" root))
          (reddit-tests--wait-exit))
        (expect (car (reddit-tests--ran root))
                :to-equal (list reddit-maildir "http://reddit.test" '(("r.test" "p1"))))
        ;; its fetch's end took the ask
        (expect reddit-thread-asked :to-be nil))))
  (it "refuses a thread of mail"
    (cl-letf (((symbol-function 'mail-thread-message-ids) (lambda () '("<1@x>"))))
      (let ((mail-thread-group "nnmaildir+gmail:inbox"))
        (expect (refetch-reddit-thread) :to-throw 'user-error)))))

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
          (expect args :to-equal (list reddit-maildir "http://reddit.test" '(("r.test"))))
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
        (expect reddit-fetched-at :to-be nil))))
  (it "runs the end hook once a fetch ended, a failed one too"
    (reddit-tests--with-store
      (let* ((root (file-name-directory (directory-file-name reddit-maildir)))
             (reddit-fetch-script (reddit-tests--stand-in root 1))
             (gnus-group-list '("nnmaildir+reddit:r.test"))
             (ended 0)
             (reddit-fetch-end-hook (list (lambda () (setq ended (1+ ended))))))
        (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                  ((symbol-function 'load-mail-groups) #'ignore)
                  ((symbol-function 'message) #'ignore))
          (write-region "" nil (expand-file-name "go" root))
          (fetch-reddit)
          (reddit-tests--wait-exit))
        (expect ended :to-equal 1))))
  (it "drops Gnus's copies of the articles it showed once a fetch wrote edits, and only then"
    (reddit-tests--with-store
      (let* ((root (file-name-directory (directory-file-name reddit-maildir)))
             (gnus-group-list '("nnmaildir+reddit:r.test"))
             forgot)
        (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                  ((symbol-function 'load-mail-groups) #'ignore)
                  ((symbol-function 'forget-shown-articles) (lambda () (push 'forgot forgot))))
          (write-region "" nil (expand-file-name "go" root))
          (let ((reddit-fetch-script
                 (reddit-tests--stand-in
                  root 0 "r.test: 0 posts, 3 comments, 0 edits, 0 threads waiting")))
            (fetch-reddit)
            (reddit-tests--wait-exit))
          (expect forgot :to-be nil)
          (let ((reddit-fetch-script
                 (reddit-tests--stand-in
                  root 0 "r.test: 0 posts, 0 comments, 1 edits, 0 threads waiting")))
            (fetch-reddit)
            (reddit-tests--wait-exit)))
        (expect forgot :to-equal '(forgot))))))

(describe "reddit-fetch-count"
  (it "sums what each folder's line of the fetch reports"
    (let ((buffer (generate-new-buffer " *reddit-tests-fetch*")))
      (unwind-protect
          (cl-letf (((symbol-function 'process-buffer) (lambda (_) buffer)))
            (with-current-buffer buffer
              (insert "r.a: 1 posts, 2 comments, 3 edits, 4 threads waiting\n"
                      "r.b: 0 posts, 5 comments, 6 edits, 0 threads waiting; blocked\n"
                      "r.c: no JSON in the page\n"))
            (expect (reddit-fetch-count 'fetch "comments") :to-equal 7)
            (expect (reddit-fetch-count 'fetch "edits") :to-equal 9))
        (kill-buffer buffer)))))

(describe "forget-shown-articles"
  (it "drops the backlog and the original article, so Gnus reads the files again"
    (let ((gnus-keep-backlog 20)
          (gnus-backlog-articles nil)
          ;; the backlog asks Gnus's group table whether a group is virtual
          (gnus-newsrc-hashtb (gnus-make-hashtable))
          (gnus-original-article-buffer " *reddit-tests-original*")
          (group "nnmaildir+reddit:r.test"))
      (unwind-protect
          (progn
            (with-temp-buffer
              (insert "the older text")
              (gnus-backlog-enter-article group 7 (current-buffer)))
            (with-current-buffer (get-buffer-create gnus-original-article-buffer)
              (setq-local gnus-original-article (cons group 7)))
            (expect (with-temp-buffer (gnus-backlog-request-article group 7 (current-buffer)))
                    :to-be-truthy)
            (forget-shown-articles)
            (expect (with-temp-buffer (gnus-backlog-request-article group 7 (current-buffer)))
                    :to-be nil)
            (expect (buffer-local-value 'gnus-original-article
                                        (get-buffer gnus-original-article-buffer))
                    :to-be nil))
        (gnus-backlog-shutdown)
        (when (get-buffer gnus-original-article-buffer)
          (kill-buffer gnus-original-article-buffer))))))

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

(defun reddit-tests--header (id)
  "Header of article 7, a Reddit message whose Message-ID is ID."
  (make-full-mail-header 7 "Re: Reddit post" "\"u/ann\" <ann@reddit.invalid>"
                         "Mon, 28 Sep 2026 10:00:00 +0000" id "" 0 0))

(describe "reddit-message-on-screen"
  (it "finds the message on screen through its summary, in a Reddit group only"
    (let ((summary (generate-new-buffer " *reddit-tests summary*"))
          (header (reddit-tests--header "<c1.p1@reddit.com>")))
      (unwind-protect
          (cl-letf (((symbol-function 'mail-on-screen) (lambda () (cons summary 7))))
            (with-current-buffer summary
              (setq-local gnus-newsgroup-data (list (gnus-data-make 7 gnus-unread-mark 1 header 0)))
              (setq-local gnus-newsgroup-name "nnmaildir+reddit:inbox"))
            (expect (reddit-message-on-screen) :to-equal (cons "nnmaildir+reddit:inbox" header))
            (with-current-buffer summary
              (setq-local gnus-newsgroup-name "nnmaildir+gmail:inbox"))
            (expect (reddit-message-on-screen) :to-be nil))
        (kill-buffer summary))))
  (it "finds nothing in an article buffer no summary shows"
    (cl-letf (((symbol-function 'mail-on-screen) (lambda () (cons nil nil))))
      (expect (reddit-message-on-screen) :to-be nil))))

(describe "subreddit-copy"
  (it "finds the copy a subreddit's group holds, never the inbox's or mine's"
    (let ((groups (make-hash-table :test #'equal)))
      (pcase-dolist (`(,group . ,articles)
                     '(("nnmaildir+reddit:inbox" ("<c2.p1@reddit.com>" . 3))
                       ("nnmaildir+reddit:mine" ("<c2.p1@reddit.com>" . 4))
                       ("nnmaildir+reddit:r.test" ("<p1@reddit.com>" . 50)
                        ("<c2.p1@reddit.com>" . 52))))
        (let ((mlist (make-hash-table :test #'equal)))
          (pcase-dolist (`(,id . ,number) articles)
            (puthash id (make-nnmaildir--art :msgid id :num number) mlist))
          (puthash group (make-nnmaildir--grp :mlist mlist) groups)))
      ;; nnmaildir holds no r.other yet
      (cl-letf (((symbol-function 'mail-group-loaded-p) (lambda (group) (gethash group groups))))
        (let ((gnus-group-list '("nnmaildir+reddit:inbox" "nnmaildir+reddit:mine"
                                 "nnmaildir+reddit:r.other" "nnmaildir+reddit:r.test"
                                 "nnmaildir+gmail:inbox")))
          (expect (subreddit-copy "<c2.p1@reddit.com>") :to-equal '("nnmaildir+reddit:r.test" . 52))
          (expect (subreddit-copy "<c9.p9@reddit.com>") :to-be nil))))))

(describe "show-in-subreddit"
  (cl-flet ((show (&optional unloaded)
              ;; the loads it asks for, its wait, and what it looks for once they end
              (let ((pending unloaded)
                    calls)
                (cl-letf (((symbol-function 'reddit-message-on-screen)
                           (lambda () (cons "nnmaildir+reddit:inbox"
                                            (reddit-tests--header "<c2.p1@reddit.com>"))))
                          ((symbol-function 'reddit-message-link)
                           (lambda (group article) (push (list 'link group article) calls) "LINK"))
                          ((symbol-function 'mail-group-loaded-p)
                           (lambda (group) (not (member group unloaded))))
                          ((symbol-function 'mail-group-pending-p)
                           (lambda (group) (member group pending)))
                          ((symbol-function 'load-mail-groups)
                           (lambda (groups &optional first) (push (list 'load groups first) calls)))
                          ((symbol-function 'wait-for-mail-load)
                           (lambda (_key _what predicate action)
                             (push (list 'wait (and (funcall predicate) t)) calls)
                             ;; the loads end
                             (setq pending nil)
                             (when (funcall predicate)
                               (funcall action))))
                          ((symbol-function 'show-subreddit-copy)
                           (lambda (&rest args) (push (cons 'show args) calls))))
                  (let ((gnus-group-list '("nnmaildir+reddit:inbox" "nnmaildir+reddit:r.test")))
                    (show-in-subreddit)))
                (nreverse calls))))
    (it "looks for the copy of the message on screen, its permalink at hand"
      (expect (show)
              :to-equal '((link "nnmaildir+reddit:inbox" 7) (wait t)
                          (show "<c2.p1@reddit.com>" "LINK"))))
    (it "reads a subreddit's group nnmaildir lacks first, and looks once it is read"
      (expect (show '("nnmaildir+reddit:r.test"))
              :to-equal '((link "nnmaildir+reddit:inbox" 7) (load ("nnmaildir+reddit:r.test") t)
                          (wait nil) (show "<c2.p1@reddit.com>" "LINK")))))
  (it "refuses where no Reddit message is on screen"
    (cl-letf (((symbol-function 'reddit-message-on-screen) #'ignore))
      (expect (show-in-subreddit) :to-throw 'user-error '("No Reddit message here")))))

(defmacro reddit-tests--showing (&rest body)
  "Run BODY with the steps of `show-subreddit-copy' stubbed and logged in `calls'.
BODY sets `copy', what `subreddit-copy' finds, and `shown', whether a
window shows Gnus.  The subreddit Gnus knows is r.test."
  (declare (indent 0))
  `(let ((copy nil)
         (shown t)
         (calls nil)
         (gnus-group-list '("nnmaildir+reddit:inbox" "nnmaildir+reddit:r.test"))
         (reddit-thread-then nil)
         (reddit-thread-fetched-at nil))
     (cl-letf (((symbol-function 'subreddit-copy) (lambda (_id) copy))
               ((symbol-function 'gnus-shown-p) (lambda () shown))
               ((symbol-function 'goto-mail-summary-article)
                (lambda (&rest args) (push (cons 'goto args) calls)))
               ((symbol-function 'ensure-subreddit-group)
                (lambda (group) (push (list 'ensure group) calls)))
               ((symbol-function 'fetch-reddit-thread)
                (lambda (group post) (push (list 'fetch group post) calls)))
               ((symbol-function 'message)
                (lambda (format &rest args) (push (list 'said (apply #'format format args)) calls))))
       ,@body)))

(defun reddit-tests--no-copy ()
  "What `show-subreddit-copy' says when no group holds the message."
  (list 'said (format "No subreddit's group holds this message; %s opens it on Reddit"
                      (substitute-command-keys "\\[open-mail-on-web]"))))

(describe "show-subreddit-copy"
  (it "goes to the copy a subreddit's group holds"
    (reddit-tests--showing
      (setq copy '("nnmaildir+reddit:r.test" . 52))
      (show-subreddit-copy "<c2.p1@reddit.com>" "https://www.reddit.com/r/test/comments/p1/slug/c2/")
      (expect calls :to-equal '((goto "nnmaildir+reddit:r.test" 52)))))
  (it "fetches a thread the subreddit's group lacks into it, then goes to the copy"
    (reddit-tests--showing
      (setq reddit-thread-fetched-at (list (cons "p9" (float-time))))
      (show-subreddit-copy "<c9.p9@reddit.com>" "https://www.reddit.com/r/test/comments/p9/slug/c9/")
      (expect (reverse calls) :to-equal '((ensure "nnmaildir+reddit:r.test")
                                          (fetch "nnmaildir+reddit:r.test" "p9")
                                          (said "Fetching the thread into r.test...")))
      ;; a thread fetched within the interval would not be fetched again
      (expect reddit-thread-fetched-at :to-be nil)
      (expect (car reddit-thread-then) :to-equal "p9")
      (setq calls nil
            copy '("nnmaildir+reddit:r.test" . 60))
      (funcall (cdr reddit-thread-then))
      (expect calls :to-equal '((goto "nnmaildir+reddit:r.test" 60)))))
  (it "fetches a thread of a subreddit Gnus has no group for into one made for it"
    (reddit-tests--showing
      (show-subreddit-copy "<c9.p9@reddit.com>" "https://www.reddit.com/r/other/comments/p9/slug/c9/")
      (expect (seq-take (reverse calls) 2) :to-equal '((ensure "nnmaildir+reddit:r.other")
                                                       (fetch "nnmaildir+reddit:r.other" "p9")))))
  (it "says where the thread came in when no window shows Gnus by then"
    (reddit-tests--showing
      (show-subreddit-copy "<c9.p9@reddit.com>" "https://www.reddit.com/r/test/comments/p9/slug/c9/")
      (setq calls nil
            copy '("nnmaildir+reddit:r.test" . 60)
            shown nil)
      (funcall (cdr reddit-thread-then))
      (expect calls :to-equal '((said "The thread is in r.test")))))
  (it "says there is no copy when the thread came without it, and fetches nothing more"
    (reddit-tests--showing
      (show-subreddit-copy "<c9.p9@reddit.com>" "https://www.reddit.com/r/test/comments/p9/slug/c9/")
      (setq calls nil)
      (funcall (cdr reddit-thread-then))
      (expect calls :to-equal (list (reddit-tests--no-copy)))))
  (it "says there is no copy when no permalink names a subreddit"
    (reddit-tests--showing
      (show-subreddit-copy "<c9.p9@reddit.com>" nil)
      (expect calls :to-equal (list (reddit-tests--no-copy)))
      (expect reddit-thread-then :to-be nil))))

(describe "reddit-message-link"
  (it "reads the permalink the Archived-At header names, and none where there is none"
    (let ((nntp-server-buffer (generate-new-buffer " *reddit-tests nntp*")))
      (unwind-protect
          (cl-letf (((symbol-function 'gnus-request-head)
                     (lambda (article group)
                       (with-current-buffer nntp-server-buffer
                         (erase-buffer)
                         (insert "Message-ID: <c9.p9@reddit.com>\n"
                                 (if (eql article 7)
                                     "Archived-At: <https://www.reddit.com/r/test/comments/p9/slug/c9/>\n"
                                   "")))
                       (equal group "nnmaildir+reddit:inbox"))))
            (expect (reddit-message-link "nnmaildir+reddit:inbox" 7)
                    :to-equal "https://www.reddit.com/r/test/comments/p9/slug/c9/")
            (expect (reddit-message-link "nnmaildir+reddit:inbox" 8) :to-be nil)
            (expect (reddit-message-link "nnmaildir+gmail:inbox" 7) :to-be nil))
        (kill-buffer nntp-server-buffer)))))

(describe "subreddit-group-of-link"
  (it "names the group of the subreddit a permalink lies in, as Gnus spells it"
    (let ((gnus-group-list '("nnmaildir+reddit:inbox" "nnmaildir+reddit:r.emacs")))
      (expect (subreddit-group-of-link "https://www.reddit.com/r/emacs/comments/p1/slug/c1/?context=3")
              :to-equal "nnmaildir+reddit:r.emacs")
      (expect (subreddit-group-of-link "https://www.reddit.com/r/Emacs/comments/p1/slug/")
              :to-equal "nnmaildir+reddit:r.emacs")
      (expect (subreddit-group-of-link "https://www.reddit.com/r/AskReddit/comments/p2/slug/")
              :to-equal "nnmaildir+reddit:r.AskReddit")
      (dolist (link '(nil "https://www.reddit.com/message/messages/m1" "https://example.com/x"))
        (expect (subreddit-group-of-link link) :to-be nil)))))

(describe "ensure-subreddit-group"
  (it "makes the folder, and the group above the routine scan under the Reddit topic, once"
    (reddit-tests--with-store
      (let ((reddit-topic "Reddit")
            (gnus-topic-alist '(("Reddit" "nnmaildir+reddit:inbox")))
            (gnus-activate-level 3)
            ;; gnus-group-entry, a macro, reads it
            (gnus-newsrc-hashtb (gnus-make-hashtable))
            calls)
        (cl-letf (((symbol-function 'subscribe-mail-group)
                   (lambda (group)
                     (puthash group (list 3 nil) gnus-newsrc-hashtb)
                     (push (list 'subscribe group) calls)))
                  ((symbol-function 'gnus-group-level) (lambda (_group) 3))
                  ((symbol-function 'gnus-group-change-level)
                   (lambda (group level old) (push (list 'level group level old) calls)))
                  ((symbol-function 'put-group-in-topic)
                   (lambda (group topic) (push (list 'topic group topic) calls))))
          (ensure-subreddit-group "nnmaildir+reddit:r.other")
          (ensure-subreddit-group "nnmaildir+reddit:r.other"))
        (expect (directory-files (expand-file-name "r.other" reddit-maildir) nil "\\`[^.]")
                :to-equal '("cur" "new" "tmp"))
        (expect (nreverse calls)
                :to-equal '((subscribe "nnmaildir+reddit:r.other")
                            (level "nnmaildir+reddit:r.other" 4 3)
                            (topic "nnmaildir+reddit:r.other" "Reddit"))))))
  (it "leaves the group where Gnus put it when the topic is missing"
    (reddit-tests--with-store
      (let ((reddit-topic "Reddit")
            (gnus-topic-alist '(("Gnus")))
            (gnus-newsrc-hashtb (gnus-make-hashtable))
            moved)
        (cl-letf (((symbol-function 'subscribe-mail-group)
                   (lambda (group) (puthash group (list 3 nil) gnus-newsrc-hashtb)))
                  ((symbol-function 'gnus-group-level) (lambda (_group) 3))
                  ((symbol-function 'gnus-group-change-level) #'ignore)
                  ((symbol-function 'put-group-in-topic) (lambda (&rest args) (push args moved))))
          (ensure-subreddit-group "nnmaildir+reddit:r.other"))
        (expect moved :to-be nil)))))

;;; reddit-tests.el ends here
