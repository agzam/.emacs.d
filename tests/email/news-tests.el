;;; tests/email/news-tests.el --- news fetch specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; each spec binds these, so a suite loaded later still sees config.el's
(defvar news-maildir)
(defvar news-server)
(defvar mail-groups)

(load-module-file "modules/email/autoload/news.el")
;; nnmaildir's header parser reads a decoder variable gnus-sum defines
(require 'gnus-sum)

(defmacro news-tests--with-store (&rest body)
  "Run BODY with an empty `news-maildir' and cache paths in a temp dir."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "news-tests" t)))
          (news-maildir (expand-file-name "news/" root))
          (news-active-file (expand-file-name "cache/news-active" root))
          (news-fetch-directory (expand-file-name "cache/news-fetch/" root))
          (news-server "news.example.org")
          (mail-groups '("nnmaildir+gmail:inbox" "nnmaildir+news:gmane.test"))
          (news-fetch-process nil)
          (news-fetched-at nil))
     (unwind-protect (progn ,@body)
       (when (process-live-p news-fetch-process)
         (delete-process news-fetch-process))
       (delete-directory root t))))

(defun news-tests--stand-in (root &optional exit)
  "A news-fetch script under ROOT that waits for ROOT/go, then exits EXIT.
It records its arguments and whether its input ended in ROOT/ran."
  (let ((script (expand-file-name "stand-in.el" root)))
    (with-temp-file script
      (prin1 `(defun news-fetch-main (&rest args)
                (let ((eof (condition-case nil (progn (read-from-minibuffer "") nil)
                             (error t))))
                  (while (not (file-exists-p ,(expand-file-name "go" root)))
                    (sleep-for 0.05))
                  (with-temp-file ,(expand-file-name "ran" root)
                    (prin1 (list args eof) (current-buffer))))
                (kill-emacs ,(or exit 0)))
             (current-buffer)))
    script))

(defun news-tests--ran (root)
  "What the stand-in under ROOT recorded, or nil before it ran."
  (let ((file (expand-file-name "ran" root)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (read (current-buffer))))))

(defun news-tests--wait-exit ()
  "Wait up to 30 s for `news-fetch-process' to end and its sentinel to run."
  (with-timeout (30)
    (while (or (process-live-p news-fetch-process)
               (not (memq (process-status news-fetch-process) '(exit signal))))
      (accept-process-output nil 0.05)))
  ;; the sentinel runs once more output is read
  (accept-process-output nil 0.1))

(describe "news-group-p"
  (it "tells the groups of the news store from every other"
    (expect (news-group-p "nnmaildir+news:gmane.emacs.devel") :to-be-truthy)
    (dolist (group '("nnmaildir+gmail:inbox" "nntp+news.gmane.io:gmane.emacs.devel"
                     "nnmaildir+newsletters:x"))
      (expect (news-group-p group) :to-be nil))))

(describe "ensure-news-folders"
  (it "creates a maildir for each news group mail-groups names, and nothing else"
    (news-tests--with-store
      (ensure-news-folders)
      (expect (directory-files news-maildir nil "\\`[^.]") :to-equal '("gmane.test"))
      (expect (directory-files (expand-file-name "gmane.test" news-maildir) nil "\\`[^.]")
              :to-equal '("cur" "new" "tmp"))
      ;; a second call finds everything in place
      (ensure-news-folders)
      (expect (directory-files news-maildir nil "\\`[^.]") :to-equal '("gmane.test")))))

(describe "read-news-groups"
  (it "names the news groups nnmaildir has read this session, and nothing without one"
    (let ((nnmaildir--servers nil))
      (expect (read-news-groups) :to-be nil)
      (let ((server (make-nnmaildir--srv :address "news" :groups (make-hash-table :test #'equal))))
        (setf (alist-get "news" nnmaildir--servers nil nil #'equal) server)
        (puthash "gmane.test" 'group (nnmaildir--srv-groups server))
        (expect (read-news-groups) :to-equal '("nnmaildir+news:gmane.test"))))))

(defmacro news-tests--with-news-server (files &rest body)
  "Run BODY with nnmaildir serving `news-maildir' as server news.
FILES, relative to the store, are written first; a newsrc knows
gmane.test and nothing else."
  (declare (indent 1))
  `(news-tests--with-store
     (let* ((method `(nnmaildir "news" (directory ,news-maildir) (get-new-mail nil)))
            (gnus-secondary-select-methods (list method))
            (gnus-server-method-cache nil)
            (gnus-newsrc-hashtb (make-hash-table :test #'equal))
            (info (gnus-info-make "nnmaildir+news:gmane.test" 4 nil nil method)))
       (puthash "nnmaildir+news:gmane.test" (list nil info) gnus-newsrc-hashtb)
       (dolist (sub '("cur" "new" "tmp"))
         (make-directory (expand-file-name (concat "gmane.test/" sub) news-maildir) t))
       (dolist (file ,files)
         (with-temp-file (expand-file-name file news-maildir)
           (insert "From: Ann <ann@example.com>\nSubject: " file
                   "\nMessage-ID: <" (md5 file) "@test>\n\nbody\n")))
       ;; a Gnus start makes the buffer nnmaildir answers requests in
       (nnheader-init-server-buffer)
       (unwind-protect
           (progn
             (nnmaildir-open-server "news" (cddr method))
             ,@body)
         (setf (alist-get "news" nnmaildir--servers nil 'remove #'equal) nil)
         (setq nnmaildir--cur-server nil)))))

(defun news-tests--read-group (group server &rest args)
  "Read GROUP on SERVER the way a first entry does: scan it, then ask for it.
ARGS go to the request."
  (nnmaildir-request-scan group server)
  (apply #'nnmaildir-request-group group server args))

(describe "merge-news-flags-a"
  (it "merges the flags of a news group an entry reads for the first time"
    ;; the fetch delivered the older post read
    (news-tests--with-news-server '("gmane.test/cur/1.old:2,S" "gmane.test/cur/2.new:2,")
      (expect (merge-news-flags-a #'news-tests--read-group "gmane.test" "news") :to-be t)
      (let ((read (gnus-info-read info)))
        (expect (range-member-p (nnmaildir-base-name-to-article-number "1.old" "gmane.test" "news")
                                read)
                :to-be t)
        (expect (range-member-p (nnmaildir-base-name-to-article-number "2.new" "gmane.test" "news")
                                read)
                :to-be nil))))
  (it "leaves a group it read before alone, and every group of another server"
    (news-tests--with-news-server '("gmane.test/cur/1.old:2,S")
      (let (merged)
        (cl-letf (((symbol-function 'gnus-request-update-info)
                   (lambda (&rest args) (push args merged))))
          (news-tests--read-group "gmane.test" "news")
          (merge-news-flags-a #'news-tests--read-group "gmane.test" "news")
          (merge-news-flags-a #'ignore "inbox" "gmail"))
        (expect merged :to-be nil)))))

(describe "news-active-groups"
  (it "reads the saved group list once per change, each group with its article count"
    (news-tests--with-store
      (let ((news-active-cache nil)
            reads)
        (expect (news-active-groups) :to-be nil)
        (make-directory (file-name-directory news-active-file) t)
        (write-region (concat "gmane.emacs.devel 0000346740 0000000827 y\n"
                              "gmane.empty 0000000004 0000000005 y\n")
                      nil news-active-file nil 'silent)
        (advice-add 'read-news-active-file :before (lambda () (push t reads))
                    '((name . news-tests)))
        (unwind-protect
            (progn
              (expect (news-active-groups)
                      :to-equal '(("nnmaildir+news:gmane.emacs.devel" . 345914)
                                  ("nnmaildir+news:gmane.empty" . 0)))
              (news-active-groups)
              (expect (length reads) :to-equal 1)
              ;; a fetch saved a newer list
              (write-region "gmane.other 0000000009 0000000001 m\n" nil news-active-file
                            nil 'silent)
              (set-file-times news-active-file (time-add nil 60))
              (expect (news-active-groups) :to-equal '(("nnmaildir+news:gmane.other" . 9)))
              (expect (length reads) :to-equal 2))
          (advice-remove 'read-news-active-file 'news-tests))))))

(describe "news-fetch-command"
  (it "runs the fetch script in a batch Emacs of its own, sandboxed"
    (news-tests--with-store
      (let ((command (news-fetch-command)))
        (expect (car command) :to-equal (expand-file-name invocation-name invocation-directory))
        (expect (seq-take (cdr command) 4)
                :to-equal (list "-Q" "--batch" "--init-directory" news-fetch-directory))
        (expect (cadr (member "-l" command)) :to-equal news-fetch-script)
        (expect (car (last command))
                :to-equal (format "(news-fetch-main %S %S %S)"
                                  news-maildir news-active-file "news.example.org"))))))

(describe "fetch-news"
  (it "fetches in the background, never twice at once, and the fetch reads no input"
    (news-tests--with-store
      (let* ((root (file-name-directory (directory-file-name news-maildir)))
             (news-fetch-script (news-tests--stand-in root))
             (start (float-time)))
        (fetch-news)
        (expect (< (- (float-time) start) 0.5) :to-be t)
        (let ((first news-fetch-process))
          (expect (process-live-p first) :to-be-truthy)
          (fetch-news)
          (expect news-fetch-process :to-be first)
          (expect (seq-count (lambda (process) (equal (process-name process) "news-fetch"))
                             (process-list))
                  :to-equal 1))
        ;; the configured group has a folder before the fetch looks, and
        ;; the fetch's own init directory exists for what TLS saves there
        (expect (file-directory-p (expand-file-name "gmane.test/cur" news-maildir)) :to-be t)
        (expect (file-directory-p news-fetch-directory) :to-be t)
        (write-region "" nil (expand-file-name "go" root))
        (news-tests--wait-exit)
        (pcase-let ((`(,args ,eof) (news-tests--ran root)))
          (expect args :to-equal (list news-maildir news-active-file "news.example.org"))
          (expect eof :to-be t))))))

(describe "news-fetch-sentinel"
  (it "refreshes the news groups Gnus read once a fetch ends well, and no other"
    ;; a group Gnus has not read would be read whole in the sentinel
    (news-tests--with-store
      (let* ((root (file-name-directory (directory-file-name news-maildir)))
             (news-fetch-script (news-tests--stand-in root 0))
             refreshed)
        (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                  ((symbol-function 'read-news-groups) (lambda () '("nnmaildir+news:gmane.test")))
                  ((symbol-function 'refresh-mail-group) (lambda (group) (push group refreshed)))
                  ((symbol-function 'gnus-group-update-group) #'ignore))
          (write-region "" nil (expand-file-name "go" root))
          (fetch-news)
          (news-tests--wait-exit))
        (expect refreshed :to-equal '("nnmaildir+news:gmane.test"))
        (expect (numberp news-fetched-at) :to-be t))))
  (it "says a fetch failed, and still refreshes for the posts it delivered first"
    (news-tests--with-store
      (let* ((root (file-name-directory (directory-file-name news-maildir)))
             (news-fetch-script (news-tests--stand-in root 1))
             refreshed said)
        (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                  ((symbol-function 'read-news-groups) (lambda () '("nnmaildir+news:gmane.test")))
                  ((symbol-function 'refresh-mail-group) (lambda (group) (push group refreshed)))
                  ((symbol-function 'gnus-group-update-group) #'ignore)
                  ((symbol-function 'message)
                   (lambda (format &rest args) (setq said (apply #'format format args)))))
          (write-region "" nil (expand-file-name "go" root))
          (fetch-news)
          (news-tests--wait-exit))
        (expect refreshed :to-equal '("nnmaildir+news:gmane.test"))
        (expect said :to-match "News fetch failed")
        (expect news-fetched-at :to-be nil)))))

(describe "fetch-stale-news"
  (it "fetches when no fetch ended well within the interval"
    (let ((news-fetch-interval 1800) fetched)
      (cl-letf (((symbol-function 'fetch-news) (lambda () (setq fetched t))))
        (let ((news-fetched-at nil))
          (fetch-stale-news))
        (expect fetched :to-be t)
        (setq fetched nil)
        (let ((news-fetched-at (- (float-time) 3600)))
          (fetch-stale-news))
        (expect fetched :to-be t)
        (setq fetched nil)
        (let ((news-fetched-at (- (float-time) 60)))
          (fetch-stale-news))
        (expect fetched :to-be nil)))))

(describe "start-news-fetch-timer"
  (it "sets one repeating fetch, the first an interval away, and none in batch"
    (let ((news-fetch-timer nil)
          (news-fetch-interval 1800))
      (unwind-protect
          (progn
            (start-news-fetch-timer)
            (expect news-fetch-timer :to-be nil)
            (let ((noninteractive nil))
              (start-news-fetch-timer)
              (start-news-fetch-timer))
            (expect (timerp news-fetch-timer) :to-be t)
            (expect (timer--repeat-delay news-fetch-timer) :to-equal 1800)
            (expect (< 1700 (float-time (time-subtract (timer--time news-fetch-timer) nil)))
                    :to-be t)
            (expect (seq-count (lambda (timer) (eq (timer--function timer) #'fetch-news))
                               timer-list)
                    :to-equal 1))
        (when (timerp news-fetch-timer)
          (cancel-timer news-fetch-timer))))))

;;; news-tests.el ends here
