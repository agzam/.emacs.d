;;; tests/email/labels-tests.el --- label menu specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defvar gmail-maildir "/nonexistent-labels-tests/")
(defvar news-maildir "/nonexistent-labels-tests-news/")
(defvar mail-inbox-group "nnmaildir+gmail:inbox")
(defvar mail-trash-group "nnmaildir+gmail:trash")
(defvar mail-archive-group "nnmaildir+gmail:archive")

(load-module-file "scripts/mail-load.el")
(load-module-file "modules/email/autoload/load.el")
(load-module-file "modules/email/autoload/mail.el")
(load-module-file "modules/email/autoload/news.el")
(load-module-file "modules/email/autoload/marks.el")
(load-module-file "modules/email/autoload/labels.el")

(defconst labels-tests-today
  '("Inbox" "clojure" "emacs" "forwarded" "friends" "github" "Health" "job" "kids"
    "money" "new" "org-mode" "subscriptions")
  "The account's labels, in menu order.")

(defmacro labels-tests-with-keys-file (&rest body)
  "Run BODY with the remembered keys in a file of its own."
  (declare (indent 0))
  `(let ((mail-label-keys-file (make-temp-file "labels-tests-keys" nil ".eld"))
         (mail-label-keys nil))
     (delete-file mail-label-keys-file)
     (unwind-protect (progn ,@body)
       (when (file-exists-p mail-label-keys-file)
         (delete-file mail-label-keys-file)))))

(defun labels-tests-header (article)
  "Header of ARTICLE, whose Message-ID is <ARTICLE@x>."
  (make-full-mail-header article "subject" "ann@example.com"
                         "Mon, 21 Sep 2026 10:00:00 +0000"
                         (format "<%d@x>" article) "" 0 0))

(defun labels-tests-group (name)
  "The Gmail group of the label NAME."
  (concat "nnmaildir+gmail:" name))

(defmacro labels-tests-with-articles (articles &rest body)
  "Run BODY with Gnus's data holding ARTICLES, whose headers Gnus looks up inline."
  (declare (indent 1))
  `(let ((gnus-newsgroup-data (mapcar (lambda (article)
                                        (gnus-data-make article gnus-read-mark 1
                                                        (labels-tests-header article) 0))
                                      ,articles))
         (gnus-newsgroup-data-reverse nil))
     ,@body))

(describe "mail-label-key-candidates"
  (it "tries the first letters of the words, both shifted, then the other letters"
    (expect (seq-take (mail-label-key-candidates "job-search") 6)
            :to-equal '("j" "s" "J" "S" "b" "r")))
  (it "ends with every letter and digit, so a key is always left"
    (expect (length (mail-label-key-candidates "ab")) :to-equal (+ 26 26 10 -1)))
  (it "never offers q, which quits every menu"
    (expect (member "q" (mail-label-key-candidates "quotes")) :to-be nil)
    (expect (car (mail-label-key-candidates "quotes")) :to-equal "Q")))

(describe "mail-label-assign-keys"
  (it "gives today's labels their first letters, the second f shifted"
    (expect (sort (mail-label-assign-keys labels-tests-today nil)
                  (lambda (a b) (string< (car a) (car b))))
            :to-equal '(("Health" . "h") ("Inbox" . "i") ("clojure" . "c") ("emacs" . "e")
                        ("forwarded" . "f") ("friends" . "F") ("github" . "g") ("job" . "j")
                        ("kids" . "k") ("money" . "m") ("new" . "n") ("org-mode" . "o")
                        ("subscriptions" . "s"))))
  (it "hands out every first letter before a label takes a second choice"
    ;; fc comes first, but c's first letter is c
    (let ((keys (mail-label-assign-keys '("fa" "fb" "fc" "c") nil)))
      (expect (cdr (assoc "c" keys)) :to-equal "c")
      (expect (cdr (assoc "fc" keys)) :to-equal "C")))
  (it "keeps the keys it is given"
    (let ((keys (mail-label-assign-keys '("family" "forwarded" "friends" "money")
                                        '(("forwarded" . "f") ("friends" . "F")))))
      (expect (mapcar (lambda (name) (cdr (assoc name keys)))
                      '("family" "forwarded" "friends" "money"))
              :to-equal '("l" "f" "F" "m"))))
  (it "drops a kept key whose label is gone"
    (expect (mail-label-assign-keys '("job") '(("gone" . "g") ("job" . "j")))
            :to-equal '(("job" . "j")))))

(describe "mail-label-keys-for"
  (it "remembers the keys, so a new label moves none"
    (labels-tests-with-keys-file
      (mail-label-keys-for labels-tests-today)
      (let ((keys (mail-label-keys-for (append labels-tests-today '("family")))))
        (expect (cdr (assoc "forwarded" keys)) :to-equal "f")
        (expect (cdr (assoc "family" keys)) :to-equal "l")))
    ;; without the memory family would take forwarded's f
    (labels-tests-with-keys-file
      (expect (cdr (assoc "family" (mail-label-keys-for
                                    (cons "family" labels-tests-today))))
              :to-equal "f")))
  (it "lets a pinned key win, and moves the label that had it"
    (labels-tests-with-keys-file
      (mail-label-keys-for labels-tests-today)
      (let* ((mail-label-keys '(("family" . "f")))
             (keys (mail-label-keys-for (append labels-tests-today '("family")))))
        (expect (cdr (assoc "family" keys)) :to-equal "f")
        (expect (cdr (assoc "forwarded" keys)) :to-equal "r"))))
  (it "keeps no pinned key in the file, so dropping the pin frees it"
    (labels-tests-with-keys-file
      (let ((mail-label-keys '(("job" . "x"))))
        (mail-label-keys-for '("job" "kids")))
      (expect (mail-label-remembered-keys) :to-equal '(("kids" . "k"))))))

(describe "mail-label-rows-for"
  (it "keeps three rows while the columns fit"
    ;; today's labels, " k ✓ name", in the summary beside an article
    (expect (mail-label-rows-for (mapcar (lambda (name) (+ 5 (length name))) labels-tests-today)
                                 154)
            :to-equal 3))
  (it "takes more rows when three overflow the window, as few as fit"
    (let* ((widths (make-list 42 15))
           (rows (mail-label-rows-for widths 154))
           (columns (ceiling 42 rows)))
      (expect rows :to-be-greater-than 3)
      (expect (<= (+ (* 15 columns) (* 2 (1- columns))) 154) :to-be t)
      (let ((fewer (ceiling 42 (1- rows))))
        (expect (< 154 (+ (* 15 fewer) (* 2 (1- fewer)))) :to-be t))))
  (it "takes one row per label when there are fewer than three"
    (expect (mail-label-rows-for '(10 10) 154) :to-equal 2)))

(describe "mail-label-cycle"
  (it "cycles a label some messages have through on, off and back to some"
    (let ((scope (mail-label-scope-make :articles '(1 2) :carriers '(("g" 1))
                                        :want (list (cons "g" 'some))))
          seen)
      (dotimes (_ 3)
        (mail-label-cycle scope "g")
        (push (alist-get "g" (mail-label-scope-want scope) nil nil #'equal) seen))
      (expect (nreverse seen) :to-equal '(all none some))))
  (it "switches a label all or none have between on and off"
    (dolist (start '((all 1 2) (none)))
      (let ((scope (mail-label-scope-make :articles '(1 2) :carriers (list (cons "g" (cdr start)))
                                          :want (list (cons "g" (car start)))))
            seen)
        (dotimes (_ 2)
          (mail-label-cycle scope "g")
          (push (alist-get "g" (mail-label-scope-want scope) nil nil #'equal) seen))
        (expect (nreverse seen)
                :to-equal (if (eq (car start) 'all) '(none all) '(all none)))))))

(describe "mail-label-items"
  (it "adds where a label is missing and removes where it is, additions first"
    (let ((scope (mail-label-scope-make
                  :articles '(1 2 3)
                  :carriers '(("a" 1) ("b" 1 2 3) ("c" 2))
                  :want '(("a" . all) ("b" . none) ("c" . some)))))
      (expect (mail-label-items scope)
              :to-equal '((add "a" 2) (add "a" 3)
                          (remove "b" 1) (remove "b" 2) (remove "b" 3))))))

(describe "mail-label-copies"
  (it "reads each message's files from notmuch, every id in one run"
    (let (args)
      (cl-letf (((symbol-function 'call-process)
                 (lambda (_program _infile _buffer _display &rest rest)
                   (setq args rest)
                   (insert "((((:id \"a@x\" :match t :filename (\"/m/gmail/job/cur/1\" \"/m/gmail/inbox/cur/2\")"
                           " :headers (:Subject \"s\")) ()))"
                           " (((:id \"b@x\" :match t :filename \"/m/gmail/kids/cur/3\") ())))")
                   0)))
        (expect (sort (mail-label-copies '("<a@x>" "<b@x>"))
                      (lambda (x y) (string< (car x) (car y))))
                :to-equal '(("<a@x>" "/m/gmail/job/cur/1" "/m/gmail/inbox/cur/2")
                            ("<b@x>" "/m/gmail/kids/cur/3")))
        (expect (car (last args)) :to-equal "id:\"a@x\" or id:\"b@x\""))))
  (it "finds nothing in output that is not notmuch's"
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _) (insert "/fixture/archive/cur/1\n") 0)))
      (expect (mail-label-copies '("<a@x>")) :to-be nil)))
  (it "finds nothing when notmuch fails"
    (cl-letf (((symbol-function 'call-process) (lambda (&rest _) 1)))
      (expect (mail-label-copies '("<a@x>")) :to-be nil))))

(describe "mail-label-carriers"
  (it "joins the summary's own copy, nnmaildir's copies and notmuch's"
    (labels-tests-with-articles '(1 2)
     (let (asked)
      (cl-letf (((symbol-function 'mail-article-group)
                 (lambda (_) (labels-tests-group "inbox")))
                ((symbol-function 'mail-group-loaded-p)
                 (lambda (group) (not (equal group (labels-tests-group "unread")))))
                ((symbol-function 'group-articles-by-id)
                 (lambda (group ids)
                   (push group asked)
                   (when (equal group (labels-tests-group "lists"))
                     (list (cons (car ids) 1)))))
                ;; mbsync renamed the copy in labelled since the group was read
                ((symbol-function 'mail-label-copies)
                 (lambda (_) '(("<2@x>" "/m/gmail/labelled/cur/old:2,S"
                                "/m/gmail/gone/cur/away:2,"))))
                ((symbol-function 'current-mail-file)
                 (lambda (file _)
                   (unless (string-match-p "gone" file)
                     (replace-regexp-in-string "old" "old,U=4" file))))
                ((symbol-function 'mail-file-group)
                 (lambda (file)
                   (labels-tests-group (nth 3 (split-string file "/"))))))
        (expect (mail-label-carriers
                 (mapcar #'labels-tests-group '("inbox" "lists" "labelled" "gone" "unread"))
                 '(1 2))
                :to-equal `((,(labels-tests-group "inbox") 1 2)
                            (,(labels-tests-group "lists") 1)
                            (,(labels-tests-group "labelled") 2)
                            (,(labels-tests-group "gone"))
                            (,(labels-tests-group "unread"))))
        ;; nnmaildir would read a group it lacks whole, in the main thread
        (expect (member (labels-tests-group "unread") asked) :to-be nil))))))

(describe "mail-label-select"
  (it "takes the marked messages, or their threads"
    (with-temp-buffer
      (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                ((symbol-function 'use-region-p) #'ignore)
                ((symbol-function 'mail-process-marked) (lambda () '(3 5)))
                ((symbol-function 'mail-real-articles) #'identity)
                ((symbol-function 'mail-whole-threads) (lambda (articles) (cons 4 articles))))
        (let ((scope (mail-label-select nil)))
          (expect (mail-label-scope-articles scope) :to-equal '(3 5))
          (expect (mail-label-scope-marked scope) :to-be t)
          (expect (mail-label-scope-summary scope) :to-be (current-buffer)))
        (expect (mail-label-scope-articles (mail-label-select t)) :to-equal '(4 3 5)))))
  (it "takes the region over the marks"
    (with-temp-buffer
      (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                ((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'mail-process-marked) (lambda () '(3 5)))
                ((symbol-function 'mail-articles-at-point-or-region) (lambda () '(1 2)))
                ((symbol-function 'mail-real-articles) #'identity))
        (let ((scope (mail-label-select nil)))
          (expect (mail-label-scope-articles scope) :to-equal '(1 2))
          (expect (mail-label-scope-marked scope) :to-be nil)))))
  (it "takes the message on screen outside the summary, whatever the summary marked"
    (let ((summary (generate-new-buffer " *labels-tests-summary*")))
      (unwind-protect
          (with-temp-buffer
            (cl-letf (((symbol-function 'mail-on-screen) (lambda () (cons summary 7)))
                      ((symbol-function 'mail-process-marked) (lambda () '(3 5))))
              (let ((scope (mail-label-select nil)))
                (expect (mail-label-scope-summary scope) :to-be summary)
                (expect (mail-label-scope-articles scope) :to-equal '(7)))))
        (kill-buffer summary)))))

(describe "mail-label-description"
  (it "checks a label every message has, dashes one some have, and leaves the rest blank"
    (let ((scope (mail-label-scope-make
                  :want `((,(labels-tests-group "job") . all)
                          (,(labels-tests-group "kids") . some)
                          (,(labels-tests-group "money") . none)
                          (,mail-inbox-group . all)))))
      (cl-letf (((symbol-function 'transient-scope) (lambda (&rest _) scope)))
        (expect (mapcar (lambda (group)
                          (substring-no-properties
                           (mail-label-description (mail-label-suffix :group group))))
                        (list (labels-tests-group "job") (labels-tests-group "kids")
                              (labels-tests-group "money") mail-inbox-group))
                :to-equal '("✓ job" "◐ kids" "  money" "✓ Inbox"))
        (expect (get-text-property 0 'face (mail-label-description
                                            (mail-label-suffix :group (labels-tests-group "kids"))))
                :to-be 'shadow)))))

(describe "mail-label-heading"
  (it "names what the menu acts on"
    (dolist (case '((t (1 2 3) "Labels: 3 marked messages")
                    (nil (4) "Labels: the message on screen")
                    (nil (4 5) "Labels: 2 messages")))
      (let ((scope (mail-label-scope-make :marked (nth 0 case) :articles (nth 1 case))))
        (cl-letf (((symbol-function 'transient-scope) (lambda (&rest _) scope)))
          (expect (mail-label-heading) :to-equal (nth 2 case)))))))

(describe "mail-label-children"
  (it "lays the labels out in three rows, the actions in a column after them"
    (labels-tests-with-keys-file
      (let* ((groups (cons mail-inbox-group
                           (mapcar #'labels-tests-group (cdr labels-tests-today))))
             (scope (mail-label-scope-make :groups groups)))
        (cl-letf (((symbol-function 'transient-scope) (lambda (&rest _) scope))
                  ((symbol-function 'window-body-width) (lambda (&rest _) 168)))
          (let* ((columns (mail-label-children nil))
                 (labels (butlast columns)))
            (expect (length labels) :to-equal 5)
            (dolist (column labels)
              (expect (length (aref column 2)) :to-be-less-than 4))
            (expect (mapcar (lambda (suffix) (plist-get (cdr suffix) :key))
                            (aref (car (last columns)) 2))
                    :to-equal '("RET" "+"))
            (expect (mapcar (lambda (suffix) (plist-get (cdr suffix) :key))
                            (aref (car columns) 2))
                    :to-equal '("i" "c" "e"))))))))

(describe "mail-label-menu"
  (it "applies on RET and on the Return key, which evil binds in the summary"
    (expect (plist-get (cdr (transient-get-suffix 'mail-label-menu "<return>")) :command)
            :to-be 'mail-label-apply)))

(describe "mail-label-copy-number"
  (it "answers nnmaildir's number for the copy"
    (cl-letf (((symbol-function 'group-articles-by-id) (lambda (&rest _) '(("<1@x>" . 12)))))
      (expect (mail-label-copy-number (labels-tests-group "job") "<1@x>") :to-equal 12)))
  (it "maps a copy mbsync renamed to its number by its name"
    (let (asked)
      (cl-letf (((symbol-function 'group-articles-by-id) #'ignore)
                ((symbol-function 'mail-label-copies)
                 (lambda (_) '(("<1@x>" "/m/gmail/inbox/cur/a:2,S" "/m/gmail/job/cur/b:2,S"))))
                ((symbol-function 'current-mail-file)
                 (lambda (file _) (replace-regexp-in-string "/b:" "/b,U=7:" file)))
                ((symbol-function 'mail-file-group)
                 (lambda (file) (labels-tests-group (nth 3 (split-string file "/")))))
                ((symbol-function 'gnus-find-method-for-group)
                 (lambda (_) '(nnmaildir "gmail")))
                ((symbol-function 'nnmaildir-base-name-to-article-number)
                 (lambda (&rest args) (setq asked args) 30)))
        (expect (mail-label-copy-number (labels-tests-group "job") "<1@x>") :to-equal 30)
        (expect asked :to-equal '("b,U=7" "job" "gmail"))))))

(describe "mail-label-run"
  :var (calls)
  (before-each (setq calls nil))
  (it "copies the message into the label, rescanning nothing"
    (cl-letf (((symbol-function 'gnus-summary-move-article)
               (lambda (n group method action)
                 (push (list n group method action gnus-newsgroup-processable
                             (eq (symbol-function 'gnus-group-get-new-news-this-group)
                                 #'ignore))
                       calls))))
      (let ((gnus-newsgroup-processable '(1 2 3)))
        (expect (mail-label-run 'add (labels-tests-group "job") 2) :to-be t)
        (expect gnus-newsgroup-processable :to-equal '(1 2 3)))
      (expect calls :to-equal `((nil ,(labels-tests-group "job") nil copy (2) t)))))
  (it "deletes the summary's own copy through Gnus"
    (cl-letf (((symbol-function 'mail-article-group) (lambda (_) (labels-tests-group "job")))
              ((symbol-function 'gnus-summary-delete-article)
               (lambda (&rest _) (push gnus-newsgroup-processable calls))))
      (expect (mail-label-run 'remove (labels-tests-group "job") 5) :to-be 'shown)
      (expect calls :to-equal '((5)))))
  (it "deletes another group's copy, found by its Message-ID"
    (labels-tests-with-articles '(5)
     (cl-letf (((symbol-function 'mail-article-group) (lambda (_) mail-inbox-group))
              ((symbol-function 'mail-label-copy-number)
               (lambda (group id) (push (list 'number group id) calls) 40))
              ((symbol-function 'gnus-request-expire-articles)
               (lambda (&rest args) (push (cons 'expire args) calls) nil))
              ((symbol-function 'drop-gone-articles)
               (lambda (&rest args) (push (cons 'drop args) calls))))
      (expect (mail-label-run 'remove (labels-tests-group "job") 5) :to-be t)
      (expect (nreverse calls)
              :to-equal `((number ,(labels-tests-group "job") "<5@x>")
                          (expire (40) ,(labels-tests-group "job") t)
                          (drop ,(labels-tests-group "job") (40)))))))
  (it "changes nothing when the label holds no copy"
    (labels-tests-with-articles '(5)
     (cl-letf (((symbol-function 'mail-article-group) (lambda (_) mail-inbox-group))
               ((symbol-function 'mail-label-copy-number) #'ignore)
               ((symbol-function 'gnus-request-expire-articles)
                (lambda (&rest _) (push 'expire calls))))
       (expect (mail-label-run 'remove (labels-tests-group "job") 5) :to-be nil)
       (expect calls :to-be nil)))))

(describe "mail-label-report"
  (it "counts each label's changes, in the order they ran"
    (expect (mail-label-report `((remove ,(labels-tests-group "kids") 3 t)
                                 (add ,(labels-tests-group "job") 2 t)
                                 (add ,(labels-tests-group "job") 1 t)
                                 (add ,mail-inbox-group 1 t)))
            :to-equal "added Inbox to 1, added job to 2, removed kids from 1")))

(describe "running label changes"
  :var (ran said limited summary)
  (before-each
    (setq ran nil said nil limited nil
          summary (generate-new-buffer " *labels-tests-summary*")
          mail-label-jobs nil
          mail-label-timer nil)
    (spy-on 'mail-label-run :and-call-fake
            (lambda (verb group article)
              (push (list verb group article) ran)
              (if (eq article 9) 'shown t)))
    (spy-on 'gnus-summary-limit-to-marks :and-call-fake
            (lambda (&rest args) (push args limited)))
    (spy-on 'gnus-activate-group)
    (spy-on 'message :and-call-fake (lambda (&rest args) (setq said (apply #'format args)))))
  (after-each
    (when (timerp mail-label-timer)
      (cancel-timer mail-label-timer))
    (setq mail-label-jobs nil mail-label-timer nil)
    (when (buffer-live-p summary)
      (kill-buffer summary)))
  (it "runs the changes and says what changed"
    (let ((mail-label-slice 10))
      (mail-label-start (mail-label-job-make
                         :summary summary
                         :items `((add ,(labels-tests-group "job") 1)
                                  (add ,(labels-tests-group "job") 2)
                                  (remove ,(labels-tests-group "kids") 1))))
      (expect (length ran) :to-equal 3)
      (expect said :to-equal "Labels: added job to 2, removed kids from 1")
      (expect limited :to-be nil)
      (expect mail-label-jobs :to-be nil)
      ;; Gnus refuses a group whose range ends below its copies
      (expect (mapcar #'car (spy-calls-all-args 'gnus-activate-group))
              :to-have-same-items-as (list (labels-tests-group "job")
                                           (labels-tests-group "kids")))))
  (it "yields after a slice and goes on from a timer"
    (let ((mail-label-slice 0))
      (mail-label-start (mail-label-job-make
                         :summary summary
                         :items `((add ,(labels-tests-group "job") 1)
                                  (add ,(labels-tests-group "job") 2))))
      (expect ran :to-be nil)
      (expect (timerp mail-label-timer) :to-be t)
      (let ((mail-label-slice 10))
        (cancel-timer mail-label-timer)
        (mail-label-step))
      (expect (length ran) :to-equal 2)
      (expect mail-label-timer :to-be nil)))
  (it "takes the lines out of view whose own copy went"
    (let ((mail-label-slice 10))
      (mail-label-start (mail-label-job-make
                         :summary summary
                         :items `((remove ,(labels-tests-group "job") 9))))
      (expect limited :to-equal (list (list (list gnus-canceled-mark) 'reverse)))))
  (it "drops the rest when the summary is gone, and says so"
    (let ((mail-label-slice 10))
      (kill-buffer summary)
      (mail-label-start (mail-label-job-make
                         :summary summary
                         :items `((add ,(labels-tests-group "job") 1))))
      (expect ran :to-be nil)
      (expect said :to-equal "Labels: nothing changed, the rest dropped with its summary"))))

(describe "mail-label-apply"
  :var (started summary)
  (before-each
    (setq started nil
          summary (generate-new-buffer " *labels-tests-summary*"))
    (spy-on 'mail-label-start :and-call-fake (lambda (job) (push job started)))
    (spy-on 'gnus-summary-remove-process-mark :and-call-fake
            (lambda (&rest articles)
              (setq gnus-newsgroup-processable
                    (seq-difference gnus-newsgroup-processable articles)))))
  (after-each
    (kill-buffer summary))
  (it "applies at once and uses up the marks of the messages it labels"
    (with-current-buffer summary
      (setq-local gnus-newsgroup-processable '(1 2 9)))
    (let ((scope (mail-label-scope-make
                  :summary summary :articles '(1 2) :marked t
                  :carriers `((,(labels-tests-group "job") 1))
                  :want `((,(labels-tests-group "job") . all)))))
      (cl-letf (((symbol-function 'transient-scope) (lambda (&rest _) scope))
                ((symbol-function 'mail-group-loaded-p) (lambda (_) t)))
        (mail-label-apply)))
    (expect (mail-label-job-items (car started))
            :to-equal `((add ,(labels-tests-group "job") 2)))
    (expect (buffer-local-value 'gnus-newsgroup-processable summary) :to-equal '(9)))
  (it "waits for a label nnmaildir does not hold yet"
    (let ((scope (mail-label-scope-make
                  :summary summary :articles '(1)
                  :want `((,(labels-tests-group "travel") . all))))
          loaded waiting)
      (cl-letf (((symbol-function 'transient-scope) (lambda (&rest _) scope))
                ((symbol-function 'mail-group-loaded-p) (lambda (_) loaded))
                ((symbol-function 'load-mail-groups) (lambda (groups &rest _) (push groups waiting)))
                ((symbol-function 'wait-for-mail-load)
                 (lambda (_key _what predicate action) (push (cons predicate action) waiting))))
        (mail-label-apply)
        (expect started :to-be nil)
        (expect (cadr waiting) :to-equal (list (labels-tests-group "travel")))
        (pcase-let ((`(,predicate . ,action) (car waiting)))
          (expect (funcall predicate) :to-be nil)
          (setq loaded t)
          (expect (funcall predicate) :to-be t)
          (funcall action)))
      (expect (length started) :to-equal 1)))
  (it "says so when nothing changes"
    (let ((scope (mail-label-scope-make :summary summary :articles '(1)
                                        :carriers `((,(labels-tests-group "job") 1))
                                        :want `((,(labels-tests-group "job") . all))))
          said)
      (cl-letf (((symbol-function 'transient-scope) (lambda (&rest _) scope))
                ((symbol-function 'message)
                 (lambda (&rest args) (setq said (apply #'format args)))))
        (mail-label-apply))
      (expect started :to-be nil)
      (expect said :to-equal "Labels: nothing changed"))))

(describe "mail-label-create"
  :var (root reopened subscribed)
  (before-each
    (setq root (file-name-as-directory (make-temp-file "labels-tests-store" t))
          reopened nil subscribed nil)
    (make-directory (expand-file-name "inbox" root))
    (make-directory (expand-file-name "job" root)))
  (after-each
    (delete-directory root t))
  (it "makes the label's folder and group, and turns it on"
    (let ((gmail-maildir root)
          (gnus-message-archive-group "nnmaildir+gmail:sent")
          (scope (mail-label-scope-make :articles '(1) :want nil :carriers nil)))
      (cl-letf (((symbol-function 'transient-scope) (lambda (&rest _) scope))
                ((symbol-function 'read-string) (lambda (&rest _) " travel "))
                ((symbol-function 'subscribe-mail-group) (lambda (group) (push group subscribed)))
                ((symbol-function 'load-mail-groups) #'ignore)
                ((symbol-function 'transient-setup)
                 (lambda (&rest args) (setq reopened args))))
        (mail-label-create))
      (dolist (sub '("cur" "new" "tmp"))
        (expect (file-directory-p (expand-file-name (concat "travel/" sub) root)) :to-be t))
      (expect subscribed :to-equal (list (labels-tests-group "travel")))
      (expect (alist-get (labels-tests-group "travel") (mail-label-scope-want scope)
                         nil nil #'equal)
              :to-be 'all)
      (expect (member (labels-tests-group "travel") (mail-label-scope-groups scope))
              :to-be-truthy)
      (expect reopened :to-equal (list 'mail-label-menu nil nil :scope scope))))
  (it "refuses a name Gmail and Gnus would read apart, and one that exists"
    (let ((gmail-maildir root)
          (gnus-message-archive-group "nnmaildir+gmail:sent"))
      (dolist (name '("two words" "a/b" "" "Job"))
        (cl-letf (((symbol-function 'transient-scope)
                   (lambda (&rest _) (mail-label-scope-make)))
                  ((symbol-function 'read-string) (lambda (&rest _) name)))
          (expect (mail-label-create) :to-throw 'user-error)))
      (expect (directory-files root nil "\\`[^.]") :to-equal '("inbox" "job")))))

(describe "accept-mail-without-nul-a"
  (it "files the article without its NUL bytes, written instead of linked"
    (with-temp-buffer
      (insert "Subject: x\n\nbyte\0code\0\n")
      (let ((nnmaildir--file "/store/inbox/cur/1:2,S")
            seen)
        (accept-mail-without-nul-a
         (lambda (&rest args) (setq seen (list args nnmaildir--file (buffer-string))))
         "job" "gmail")
        (expect seen :to-equal '(("job" "gmail") nil "Subject: x\n\nbytecode\n")))))
  (it "leaves an article without NUL bytes alone, linked as before"
    (with-temp-buffer
      (insert "Subject: x\n\nplain\n")
      (let ((nnmaildir--file "/store/inbox/cur/1:2,S")
            seen)
        (accept-mail-without-nul-a
         (lambda (&rest _) (setq seen (list nnmaildir--file (buffer-string))))
         "job")
        (expect seen :to-equal '("/store/inbox/cur/1:2,S" "Subject: x\n\nplain\n"))))))

;;; labels-tests.el ends here
