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
          (mail-load-running nil)
          (reddit-fetched-at nil))
     (unwind-protect (progn ,@body)
       (when (process-live-p reddit-fetch-process)
         (delete-process reddit-fetch-process))
       (delete-directory root t))))

(defun reddit-tests--stand-in (root &optional exit)
  "A reddit-fetch script under ROOT that waits for ROOT/go, then exits EXIT.
It records its arguments and whether its input ended in ROOT/ran."
  (let ((script (expand-file-name "stand-in.el" root)))
    (with-temp-file script
      (prin1 `(defun reddit-fetch-main (&rest args)
                (let ((eof (condition-case nil (progn (read-from-minibuffer "") nil)
                             (error t))))
                  (while (not (file-exists-p ,(expand-file-name "go" root)))
                    (sleep-for 0.05))
                  (with-temp-file ,(expand-file-name "ran" root)
                    (prin1 (list args eof) (current-buffer))))
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
        (expect (member "-L" command) :to-be nil)))))

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
