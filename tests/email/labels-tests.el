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
(defvar mail-label-colors nil)

;; labels.el's top-level (require 'consult) must no-op in the batch tier;
;; the specs stub consult--read
(provide 'consult)

(load-module-file "scripts/mail-load.el")
(load-module-file "modules/email/autoload/load.el")
(load-module-file "modules/email/autoload/mail.el")
(load-module-file "modules/email/autoload/news.el")
(load-module-file "modules/email/autoload/marks.el")
(load-module-file "modules/email/autoload/labels.el")
(load-module-file "modules/email/autoload/sync.el")

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
    (expect (length (mail-label-key-candidates "ab")) :to-equal (+ 26 26 10 -2)))
  (it "never offers q, which quits every menu, or l, which picks by name"
    (expect (member "q" (mail-label-key-candidates "quotes")) :to-be nil)
    (expect (car (mail-label-key-candidates "quotes")) :to-equal "Q")
    (expect (member "l" (mail-label-key-candidates "legal")) :to-be nil)
    (expect (car (mail-label-key-candidates "legal")) :to-equal "L")))

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
              :to-equal '("y" "f" "F" "m"))))
  (it "drops a kept key whose label is gone"
    (expect (mail-label-assign-keys '("job") '(("gone" . "g") ("job" . "j")))
            :to-equal '(("job" . "j")))))

(describe "mail-label-keys-for"
  (it "remembers the keys, so a new label moves none"
    (labels-tests-with-keys-file
      (mail-label-keys-for labels-tests-today)
      (let ((keys (mail-label-keys-for (append labels-tests-today '("family")))))
        (expect (cdr (assoc "forwarded" keys)) :to-equal "f")
        (expect (cdr (assoc "family" keys)) :to-equal "y")))
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
                    :to-equal '("RET" "l" "+"))
            (expect (mapcar (lambda (suffix) (plist-get (cdr suffix) :key))
                            (aref (car columns) 2))
                    :to-equal '("i" "c" "e"))))))))

(describe "mail-label-menu"
  (it "applies on RET and on the Return key, which evil binds in the summary"
    (expect (plist-get (cdr (transient-get-suffix 'mail-label-menu "<return>")) :command)
            :to-be 'mail-label-apply))
  (it "lays its labels out again after every key, so a label + makes shows at once"
    (expect (oref (get 'mail-label-menu 'transient--prefix) refresh-suffixes) :to-be t)))

(describe "mail-label-pick"
  :var (scope offered)
  (before-each
    (setq offered nil
          scope (mail-label-scope-make
                 :articles '(1)
                 :groups (list mail-inbox-group (labels-tests-group "job")
                               (labels-tests-group "kids"))
                 :carriers `((,(labels-tests-group "kids") 1))
                 :want `((,mail-inbox-group . none)
                         (,(labels-tests-group "job") . none)
                         (,(labels-tests-group "kids") . all))))
    (spy-on 'transient-scope :and-return-value scope))
  (cl-flet ((pick (answer)
              (cl-letf (((symbol-function 'consult--read)
                         (lambda (candidates &rest props)
                           (setq offered
                                 (mapcar (lambda (name)
                                           (concat name (funcall (plist-get props :annotate)
                                                                 name)))
                                         candidates))
                           answer)))
                (mail-label-pick)))
            (want (name)
              (alist-get (labels-tests-group name) (mail-label-scope-want scope)
                         nil nil #'equal)))
    (it "offers the labels by their menu names, marked as the menu marks them"
      (pick "job")
      (expect (mapcar #'substring-no-properties offered)
              :to-equal '("Inbox  " "job  " "kids ✓")))
    (it "turns the picked label on or off, as its key does"
      (pick "job")
      (expect (want "job") :to-be 'all)
      (pick "kids")
      (expect (want "kids") :to-be 'none)
      (pick "Inbox")
      (expect (alist-get mail-inbox-group (mail-label-scope-want scope) nil nil #'equal)
              :to-be 'all))
    (it "picks nothing for a name no label has"
      (pick "jbo")
      (expect (mapcar #'cdr (mail-label-scope-want scope)) :to-equal '(none none all)))))

(describe "closing the label menu"
  :var (scope summary)
  (before-each
    (setq summary (generate-new-buffer " *labels-tests-summary*")
          scope (mail-label-scope-make :summary summary :articles '(1)))
    ;; each apply asks once what changed
    (spy-on 'mail-label-items :and-return-value nil)
    (spy-on 'message))
  (after-each
    (remove-hook 'transient-exit-hook #'mail-label-apply-pending)
    (setq mail-label-pending nil)
    (kill-buffer summary))
  (it "applies what the menu was set to, as RET does"
    (cl-letf (((symbol-function 'mail-label-groups) (lambda () nil))
              ((symbol-function 'mail-label-carriers) #'ignore)
              ((symbol-function 'transient-setup) #'ignore))
      (mail-label-open scope))
    (run-hooks 'transient-exit-hook)
    (expect 'mail-label-items :to-have-been-called-times 1)
    (expect 'mail-label-items :to-have-been-called-with scope)
    (expect (memq #'mail-label-apply-pending transient-exit-hook) :to-be nil))
  (it "applies nothing more once RET applied"
    (setq mail-label-pending scope)
    (add-hook 'transient-exit-hook #'mail-label-apply-pending)
    (mail-label-apply scope)
    (run-hooks 'transient-exit-hook)
    (expect 'mail-label-items :to-have-been-called-times 1))
  (it "leaves another menu's closing alone"
    (run-hooks 'transient-exit-hook)
    (expect 'mail-label-items :not :to-have-been-called)))

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
      (cl-letf (((symbol-function 'mail-group-loaded-p) (lambda (_) t)))
        (mail-label-apply scope)))
    (expect (mail-label-job-items (car started))
            :to-equal `((add ,(labels-tests-group "job") 2)))
    (expect (buffer-local-value 'gnus-newsgroup-processable summary) :to-equal '(9)))
  (it "waits for a label nnmaildir does not hold yet"
    (let ((scope (mail-label-scope-make
                  :summary summary :articles '(1)
                  :want `((,(labels-tests-group "travel") . all))))
          loaded waiting)
      (cl-letf (((symbol-function 'mail-group-loaded-p) (lambda (_) loaded))
                ((symbol-function 'load-mail-groups) (lambda (groups &rest _) (push groups waiting)))
                ((symbol-function 'wait-for-mail-load)
                 (lambda (_key _what predicate action) (push (cons predicate action) waiting))))
        (mail-label-apply scope)
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
      (cl-letf (((symbol-function 'message)
                 (lambda (&rest args) (setq said (apply #'format args)))))
        (mail-label-apply scope))
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
          (scope (mail-label-scope-make :articles '(1) :want nil :carriers nil))
          (read nil))
      (cl-letf (((symbol-function 'transient-scope) (lambda (&rest _) scope))
                ((symbol-function 'read-string) (lambda (&rest _) " travel "))
                ;; every folder Gnus lacks, the new one's topic included
                ((symbol-function 'subscribe-mail-groups)
                 (lambda ()
                   (setq subscribed (seq-filter (lambda (group) (string-suffix-p ":travel" group))
                                                (maildir-groups)))))
                ((symbol-function 'load-mail-groups)
                 (lambda (groups &optional first) (setq read (list groups first))))
                ((symbol-function 'transient-setup)
                 (lambda (&rest args) (setq reopened args))))
        (mail-label-create))
      (dolist (sub '("cur" "new" "tmp"))
        (expect (file-directory-p (expand-file-name (concat "travel/" sub) root)) :to-be t))
      (expect subscribed :to-equal (list (labels-tests-group "travel")))
      ;; ahead of every other read, since the menu fills it next
      (expect read :to-equal (list (list (labels-tests-group "travel")) t))
      (expect (alist-get (labels-tests-group "travel") (mail-label-scope-want scope)
                         nil nil #'equal)
              :to-be 'all)
      (expect (member (labels-tests-group "travel") (mail-label-scope-groups scope))
              :to-be-truthy)
      ;; the menu stays open, so closing it later applies the label
      (expect reopened :to-be nil)))
  (it "refuses a name Gmail and Gnus would read apart, one mbsync skips, and one that exists"
    (let ((gmail-maildir root)
          (gnus-message-archive-group "nnmaildir+gmail:sent"))
      (dolist (name '("two words" "a/b" "" "Job" "new"))
        (cl-letf (((symbol-function 'transient-scope)
                   (lambda (&rest _) (mail-label-scope-make)))
                  ((symbol-function 'read-string) (lambda (&rest _) name)))
          (expect (mail-label-create) :to-throw 'user-error)))
      (expect (directory-files root nil "\\`[^.]") :to-equal '("inbox" "job")))))

(describe "mail-label-name-problem"
  :var (root)
  (before-each
    (setq root (file-name-as-directory (make-temp-file "labels-tests-store" t)))
    (dolist (folder '("inbox" "sent" "archive" "job"))
      (make-directory (expand-file-name folder root))))
  (after-each
    (delete-directory root t))
  (it "takes ASCII letters, digits, - and _"
    (let ((gmail-maildir root))
      (dolist (name '("travel" "Q4-2026_x"))
        (expect (mail-label-name-problem name) :to-be nil))))
  (it "refuses what a label change would send Gmail apart from the folder's name"
    ;; . is the folder of a nested label, and IMAP takes no raw UTF-8
    (let ((gmail-maildir root))
      (dolist (name '("two words" "a/b" "a.b" "" "caf\u00e9" "say\"hi"))
        (expect (mail-label-name-problem name)
                :to-equal "A label name takes ASCII letters, digits, - and _"))))
  (it "refuses cur, new and tmp, which mbsync's maildir listing skips"
    (let ((gmail-maildir root))
      (dolist (name '("cur" "new" "tmp"))
        (expect (mail-label-name-problem name)
                :to-equal "mbsync syncs no label named cur, new or tmp"))))
  (it "refuses the name of any folder in any case, the inbox, Sent and All Mail's too"
    (let ((gmail-maildir root))
      (dolist (name '("INBOX" "Sent" "archive" "JOB"))
        (expect (mail-label-name-problem name) :to-equal (format "Label %s exists" name))))))

(describe "new-mail-label"
  (it "makes the label and shows its line"
    (let (calls)
      (cl-letf (((symbol-function 'make-mail-label)
                 (lambda (name) (push (list 'make name) calls) (labels-tests-group name)))
                ((symbol-function 'gnus-group-jump-to-group)
                 (lambda (group &rest _) (push (list 'jump group) calls)))
                ((symbol-function 'message) #'ignore))
        (new-mail-label "travel"))
      (expect (nreverse calls)
              :to-equal (list '(make "travel") (list 'jump (labels-tests-group "travel")))))))

(describe "mail-label-change-problem"
  (it "takes a label, a nested one too"
    (let ((gnus-message-archive-group "nnmaildir+gmail:sent"))
      (dolist (group '("travel" "work.qlik" "Health"))
        (expect (mail-label-change-problem (labels-tests-group group)) :to-be nil))))
  (it "refuses the inbox, Sent, Trash and All Mail, which are no labels"
    (let ((gnus-message-archive-group "nnmaildir+gmail:sent"))
      (dolist (group (list mail-inbox-group mail-trash-group mail-archive-group
                           gnus-message-archive-group))
        (expect (mail-label-change-problem group) :to-match "is not a label\\'"))))
  (it "refuses no group, and a group of another server"
    (expect (mail-label-change-problem nil) :to-equal "No group here")
    (expect (mail-label-change-problem "nnmaildir+news:gmane.emacs.devel")
            :to-match "is not a Gmail label\\'"))
  (it "refuses a label whose name a label change could not send"
    (let ((gnus-message-archive-group "nnmaildir+gmail:sent"))
      (expect (mail-label-change-problem (labels-tests-group "caf\u00e9"))
              :to-match "\\`Change caf\u00e9 in Gmail")))
  (it "takes a label while a mail sync runs: the change waits for it"
    (let ((gnus-message-archive-group "nnmaildir+gmail:sent"))
      (cl-letf (((symbol-function 'mail-sync-running-p) (lambda () t)))
        (expect (mail-label-change-problem (labels-tests-group "travel")) :to-be nil)))))

(defmacro labels-tests-changing (started &rest body)
  "Run BODY with the label change stubbed, which starts now when STARTED.
The stub keeps (CHANGE DONE) in `change'; the folders' follow and the
callback go to `calls', and the echo area to `said'."
  (declare (indent 1))
  `(let (change calls said)
     (cl-letf (((symbol-function 'start-gmail-label-change)
                (lambda (&rest args) (setq change args) ,started))
               ((symbol-function 'follow-mail-folders) (lambda () (push 'follow calls)))
               ((symbol-function 'message)
                (lambda (format &rest args) (setq said (apply #'format format args)))))
       ,@body)))

(describe "run-mail-label-change"
  (it "says what it does, then follows the folders and calls back once the change is made"
    (labels-tests-changing t
      (run-mail-label-change '(delete "travel") "Deleting label travel"
                             (lambda () (push 'done calls)))
      (expect (car change) :to-equal '(delete "travel"))
      (expect said :to-equal "Deleting label travel...")
      (expect calls :to-be nil)
      (funcall (cadr change) nil)
      (expect (nreverse calls) :to-equal '(follow done))))
  (it "says the change waits for the mail sync that runs"
    (labels-tests-changing nil
      (run-mail-label-change '(delete "travel") "Deleting label travel" #'ignore)
      (expect said :to-equal "Deleting label travel once the mail sync is done...")))
  (it "says why the change failed, and calls nothing back"
    (labels-tests-changing t
      (run-mail-label-change '(delete "travel") "Deleting label travel"
                             (lambda () (push 'done calls)))
      (funcall (cadr change) "Gmail kept label travel")
      (expect calls :to-be nil)
      (expect said :to-equal "Labels: delete failed: Gmail kept label travel"))))

(describe "delete-mail-label"
  (it "refuses a group that is no label, asking nothing and running nothing"
    (let (asked ran)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) (setq asked prompt) t))
                ((symbol-function 'run-mail-label-change) (lambda (&rest args) (setq ran args))))
        (expect (delete-mail-label mail-inbox-group) :to-throw 'user-error))
      (expect (list asked ran) :to-equal '(nil nil))))
  (it "asks first, by the label's name, and runs nothing on no"
    (let (asked ran)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) (setq asked prompt) nil))
                ((symbol-function 'run-mail-label-change) (lambda (&rest args) (setq ran args))))
        (delete-mail-label (labels-tests-group "travel")))
      (expect asked :to-match "\\`Delete label travel in Gmail")
      (expect asked :to-match "keep their other labels")
      (expect ran :to-be nil)))
  (it "deletes the label in Gmail, then says it is gone"
    (let (ran said)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                ((symbol-function 'run-mail-label-change)
                 (lambda (change doing done) (setq ran (list change doing)) (funcall done)))
                ((symbol-function 'message)
                 (lambda (format &rest args) (setq said (apply #'format format args)))))
        (delete-mail-label (labels-tests-group "work.qlik")))
      (expect ran :to-equal '((delete "work.qlik") "Deleting label work.qlik"))
      (expect said :to-equal "Deleted label work.qlik"))))

(defmacro labels-tests-renaming (topic level &rest body)
  "Run BODY with travel in TOPIC at LEVEL, the label change stubbed.
The stub keeps (CHANGE DONE DOING) in `ran', and Gnus's calls go to `calls'."
  (declare (indent 2))
  `(let* ((gnus-newsrc-hashtb (make-hash-table :test #'equal))
          (gnus-topic-alist (list (list "Gnus") (list ,topic (labels-tests-group "travel"))))
          (gmail-maildir "/nonexistent-labels-tests/")
          (ran nil)
          (calls nil)
          (said nil))
     (puthash (labels-tests-group "travel")
              (list nil (gnus-info-make (labels-tests-group "travel") ,level nil nil nil))
              gnus-newsrc-hashtb)
     (cl-letf (((symbol-function 'run-mail-label-change)
                (lambda (change doing done) (setq ran (list change done doing))))
               ((symbol-function 'gnus-group-change-level)
                (lambda (&rest args) (push (cons 'level args) calls)))
               ((symbol-function 'put-group-in-topic)
                (lambda (&rest args) (push (cons 'topic args) calls)))
               ((symbol-function 'apply-mail-topics) (lambda () (push '(arranged) calls)))
               ((symbol-function 'message)
                (lambda (format &rest args) (setq said (apply #'format format args)))))
       ,@body)))

(describe "rename-mail-label"
  (it "renames the label in Gmail from the old name to the new"
    (labels-tests-renaming "Labels" 3
      (rename-mail-label (labels-tests-group "travel") "trips")
      (expect (car ran) :to-equal '(rename "travel" "trips"))
      (expect (nth 2 ran) :to-equal "Renaming label travel to trips")))
  (it "refuses a new name that is taken or that mbsync skips, and runs nothing"
    (labels-tests-renaming "Labels" 3
      (cl-letf (((symbol-function 'maildir-groups)
                 (lambda () (list (labels-tests-group "travel") (labels-tests-group "job")))))
        (dolist (name '("Job" "new" "two words"))
          (expect (rename-mail-label (labels-tests-group "travel") name) :to-throw 'user-error)))
      (expect ran :to-be nil)))
  (it "gives the new group the old one's topic and level once the change is made"
    (labels-tests-renaming "Lists" 4
      (rename-mail-label (labels-tests-group "travel") "trips")
      ;; the folders' new group, subscribed at the default level
      (puthash (labels-tests-group "trips")
               (list nil (gnus-info-make (labels-tests-group "trips") 3 nil nil nil))
               gnus-newsrc-hashtb)
      (funcall (cadr ran))
      (expect (nreverse calls)
              :to-equal (list (list 'level (labels-tests-group "trips") 4 3)
                              (list 'topic (labels-tests-group "trips") "Lists")
                              '(arranged)))
      (expect said :to-equal "Renamed label travel to trips")))
  (it "says the next sync fetches the label when its folder went aside"
    ;; Gmail gave the renamed label new UIDs
    (labels-tests-renaming "Labels" 3
      (rename-mail-label (labels-tests-group "travel") "trips")
      (funcall (cadr ran))
      (expect calls :to-be nil)
      (expect said :to-equal "Renamed label travel to trips; the next sync fetches it"))))

(describe "rename-mail-label-a"
  (it "renames a Gmail group in Gmail too, by its bare new name"
    (let (renamed handed)
      (cl-letf (((symbol-function 'rename-mail-label) (lambda (&rest args) (setq renamed args))))
        (rename-mail-label-a (lambda (&rest args) (setq handed args))
                             (labels-tests-group "travel") (labels-tests-group "trips")))
      (expect renamed :to-equal (list (labels-tests-group "travel") "trips"))
      (expect handed :to-be nil)))
  (it "hands any other group to Gnus, and no group too"
    (let (renamed handed)
      (cl-letf (((symbol-function 'rename-mail-label) (lambda (&rest args) (setq renamed args))))
        (rename-mail-label-a (lambda (&rest args) (push args handed))
                             "nnmaildir+news:gmane.emacs.devel" "nnmaildir+news:devel")
        (rename-mail-label-a (lambda (&rest args) (push args handed)) nil "x"))
      (expect renamed :to-be nil)
      (expect (nreverse handed)
              :to-equal '(("nnmaildir+news:gmane.emacs.devel" "nnmaildir+news:devel")
                          (nil "x"))))))

(describe "delete-mail-label-a"
  (it "deletes a Gmail group in Gmail too, whatever Gnus was asked"
    (let (deleted handed)
      (cl-letf (((symbol-function 'delete-mail-label) (lambda (group) (setq deleted group))))
        (delete-mail-label-a (lambda (&rest args) (setq handed args))
                             (labels-tests-group "travel") t))
      (expect deleted :to-equal (labels-tests-group "travel"))
      (expect handed :to-be nil)))
  (it "hands any other group to Gnus with its arguments"
    (let (deleted handed)
      (cl-letf (((symbol-function 'delete-mail-label) (lambda (group) (setq deleted group))))
        (delete-mail-label-a (lambda (&rest args) (setq handed args))
                             "nnmaildir+news:gmane.emacs.devel" t))
      (expect deleted :to-be nil)
      (expect handed :to-equal '("nnmaildir+news:gmane.emacs.devel" t)))))

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

(describe "mail-label-groups"
  (it "offers the inbox first, then the labels by name, without Sent, Trash and All Mail"
    (let ((gnus-message-archive-group "nnmaildir+gmail:sent"))
      (cl-letf (((symbol-function 'maildir-groups)
                 (lambda ()
                   (mapcar #'labels-tests-group
                           '("sent" "job" "archive" "inbox" "Health" "trash" "clojure")))))
        (expect (mapcar #'mail-label-name (mail-label-groups))
                :to-equal '("Inbox" "clojure" "Health" "job"))))))

;;; On the summary line

(defun labels-tests-tables (&rest labels)
  "Tables like `mail-label-tables' answers, from LABELS as (NAME ARTICLE...).
A message is <ARTICLE@x>, as `labels-tests-header' makes it."
  (mapcar (pcase-lambda (`(,name . ,articles))
            (let ((ids (make-hash-table :test #'equal)))
              (dolist (article articles)
                (puthash (format "<%d@x>" article) t ids))
              (cons (if (equal name "Inbox") mail-inbox-group (labels-tests-group name)) ids)))
          labels))

(defun labels-tests-summary (group lines)
  "Fill the current buffer the way GROUP's summary format draws LINES.
Each of LINES is (ARTICLE LEVEL): the thread's indent, the slot, the subject."
  (setq-local gnus-newsgroup-name group)
  (setq-local gnus-newsgroup-data nil)
  (pcase-dolist (`(,article ,level) lines)
    (push (gnus-data-make article gnus-read-mark (1+ (point)) (labels-tests-header article) level)
          gnus-newsgroup-data)
    (insert (propertize (concat (make-string (* 2 level) ?\s)
                                (propertize " " 'mail-labels t)
                                (format "subject %d\n" article))
                        'gnus-number article)))
  (setq gnus-newsgroup-data (nreverse gnus-newsgroup-data)))

(defun labels-tests-shown (article)
  "Label names the slot of ARTICLE's line shows."
  (save-excursion
    (goto-char (point-min))
    (while (not (eql (get-text-property (point) 'gnus-number) article))
      (forward-line 1))
    (let ((slot (text-property-not-all (point) (line-end-position) 'mail-labels nil)))
      (split-string (substring-no-properties (or (get-text-property slot 'display)
                                                 "undrawn"))))))

(describe "mail-label-tables"
  (it "lists every label nnmaildir holds, the inbox first, Sent, Trash and All Mail left out"
    (let* ((server (make-nnmaildir--srv :address "gmail" :groups (make-hash-table :test #'equal)))
           (nnmaildir--servers (list (cons "gmail" server)))
           (gnus-message-archive-group "nnmaildir+gmail:sent"))
      (dolist (name '("job" "sent" "inbox" "trash" "Health" "archive" "clojure"))
        (let ((ids (make-hash-table :test #'equal)))
          (puthash (concat "<" name "@x>") t ids)
          (puthash name (make-nnmaildir--grp :name name :mlist ids)
                   (nnmaildir--srv-groups server))))
      (let ((tables (mail-label-tables)))
        (expect (mapcar #'car tables)
                :to-equal (mapcar #'labels-tests-group '("inbox" "clojure" "Health" "job")))
        ;; nnmaildir's own tables, not copies
        (expect (gethash "<job@x>" (cdr (assoc (labels-tests-group "job") tables))) :to-be t))))
  (it "has nothing while the Gmail server is closed"
    (let ((nnmaildir--servers nil))
      (expect (mail-label-tables) :to-be nil))))

(describe "mail-message-labels"
  (it "names each label holding one of the messages, in the tables' order"
    (let ((tables (labels-tests-tables '("Inbox" 1) '("job" 2 3) '("kids" 3) '("money" 9))))
      (expect (mail-message-labels '("<3@x>") tables)
              :to-equal (mapcar #'labels-tests-group '("job" "kids")))
      (expect (mail-message-labels '("<1@x>" "<3@x>") tables)
              :to-equal (list mail-inbox-group (labels-tests-group "job")
                              (labels-tests-group "kids")))
      (expect (mail-message-labels '("<4@x>") tables) :to-be nil))))

(describe "mail-label-chips"
  (it "draws each label as a chip in its Gmail colors, a space between chips"
    (let* ((mail-label-colors '(("job" "#9b811e" "white")))
           (chips (mail-label-chips (list (labels-tests-group "job") mail-inbox-group))))
      (expect (substring-no-properties chips) :to-equal " job   Inbox ")
      (expect (get-text-property 1 'face chips)
              :to-equal '(:background "#9b811e" :foreground "white"))
      (expect (get-text-property 5 'face chips) :to-be nil)
      ;; a label Gmail gives no colors
      (expect (get-text-property 7 'face chips) :to-be 'mail-label))))

(describe "mail-label-chips-of"
  (it "chips every label a message of the thread carries"
    (cl-letf (((symbol-function 'mail-label-tables)
               (lambda () (labels-tests-tables '("Inbox" 1) '("job" 2) '("kids" 9)))))
      (expect (substring-no-properties (mail-label-chips-of '("<1@x>" "<2@x>")))
              :to-equal " Inbox   job ")
      (expect (mail-label-chips-of '("<5@x>")) :to-be nil)))
  (it "reads the tables it is given, so a caller drawing many lines reads them once"
    (let ((tables (labels-tests-tables '("kids" 9))))
      (spy-on 'mail-label-tables)
      (expect (substring-no-properties (mail-label-chips-of '("<9@x>") tables))
              :to-equal " kids ")
      (expect 'mail-label-tables :not :to-have-been-called))))

(describe "mail-labels-shown-p"
  (it "shows labels in a Gmail folder and in search results only"
    (expect (mail-labels-shown-p mail-inbox-group) :to-be-truthy)
    (expect (mail-labels-shown-p (labels-tests-group "archive")) :to-be-truthy)
    (expect (mail-labels-shown-p "nnselect:nnselect-87a5d8") :to-be-truthy)
    (expect (mail-labels-shown-p "nnmaildir+news:gmane.emacs.devel") :to-be nil)
    (expect (mail-labels-shown-p "nnatom+www.reddit.com/r/emacs/new/.rss:r/emacs") :to-be nil)
    (expect (mail-labels-shown-p nil) :to-be nil)))

(describe "gnus-user-format-function-L"
  (it "leaves a one-space slot in a Gmail folder or a search"
    (dolist (group (list mail-inbox-group "nnselect:nnselect-87a5d8"))
      (let* ((gnus-newsgroup-name group)
             (slot (gnus-user-format-function-L (labels-tests-header 1))))
        (expect (substring-no-properties slot) :to-equal " ")
        (expect (get-text-property 0 'mail-labels slot) :to-be t))))
  (it "leaves nothing in a news group or a feed, whose lines stay as they were"
    (let ((gnus-newsgroup-name "nnmaildir+news:gmane.emacs.devel"))
      (expect (gnus-user-format-function-L (labels-tests-header 1)) :to-equal ""))))

(describe "draw-mail-labels"
  :var (tables)
  (before-each
    (setq tables (labels-tests-tables '("Inbox" 1 2 5) '("job" 1 2 3 4 5) '("kids" 3 4)
                                      '("lists" 1 2 3 4 5)))
    (spy-on 'mail-label-tables :and-call-fake (lambda () tables)))
  (it "shows every label of a line in any group, the group read and Inbox among them"
    (with-temp-buffer
      (labels-tests-summary (labels-tests-group "lists") '((1 0) (6 0)))
      (draw-mail-labels (point-min) (point-max))
      (expect (labels-tests-shown 1) :to-equal '("Inbox" "job" "lists"))
      ;; a line with no labels takes no room
      (expect (labels-tests-shown 6) :to-be nil))
    (with-temp-buffer
      (labels-tests-summary mail-inbox-group '((1 0)))
      (draw-mail-labels (point-min) (point-max))
      (expect (labels-tests-shown 1) :to-equal '("Inbox" "job" "lists"))))
  (it "shows each line its own labels, a reply with the labels of the line above too"
    (with-temp-buffer
      (labels-tests-summary (labels-tests-group "lists") '((1 0) (2 1) (3 1) (4 0) (5 1)))
      (draw-mail-labels (point-min) (point-max))
      (expect (mapcar #'labels-tests-shown '(1 2 3 4 5))
              :to-equal '(("Inbox" "job" "lists") ("Inbox" "job" "lists")
                          ("job" "kids" "lists") ("job" "kids" "lists")
                          ("Inbox" "job" "lists")))))
  (it "starts from the top of the line jit-lock starts in"
    (with-temp-buffer
      (labels-tests-summary (labels-tests-group "lists") '((1 0) (2 1)))
      (goto-char (point-min))
      (forward-line 1)
      (draw-mail-labels (+ 3 (point)) (point-max))
      (expect (labels-tests-shown 2) :to-equal '("Inbox" "job" "lists"))
      (expect (get-text-property 1 'display) :to-be nil)))
  (it "reads no table for a summary with no slots, a news group's"
    (with-temp-buffer
      (insert (propertize "news line\n" 'gnus-number 1))
      (setq-local gnus-newsgroup-name "nnmaildir+news:gmane.emacs.devel")
      (draw-mail-labels (point-min) (point-max))
      (expect 'mail-label-tables :not :to-have-been-called))))

(defvar mail-live-collect-mode)

(describe "refresh-mail-labels"
  (it "draws the labels again in summaries showing them, thread views and collected lines"
    (let ((gmail (generate-new-buffer " *labels-tests-gmail*"))
          (news (generate-new-buffer " *labels-tests-news*"))
          (thread (generate-new-buffer " *labels-tests-thread*"))
          (collect (generate-new-buffer " *labels-tests-collect*"))
          (other (generate-new-buffer " *labels-tests-other*"))
          (drawn nil))
      (unwind-protect
          (cl-letf (((symbol-function 'mail-thread-update-header-line)
                     (lambda () (push (list 'header (current-buffer)) drawn)))
                    ((symbol-function 'mail-thread-draw-labels)
                     (lambda () (push (list 'lines (current-buffer)) drawn)))
                    ((symbol-function 'mail-live-draw-labels)
                     (lambda () (push (list 'collect (current-buffer)) drawn))))
            (pcase-dolist (`(,buffer ,mode ,group) `((,gmail gnus-summary-mode ,mail-inbox-group)
                                                     (,news gnus-summary-mode
                                                            "nnmaildir+news:gmane.emacs.devel")
                                                     (,thread mail-thread-mode nil)
                                                     (,collect embark-collect-mode nil)
                                                     (,other embark-collect-mode nil)))
              (with-current-buffer buffer
                (setq-local major-mode mode)
                (setq-local gnus-newsgroup-name group)
                (insert (propertize "line\n" 'fontified t))))
            (with-current-buffer collect
              (setq-local mail-live-collect-mode t))
            (let ((mail-label-refresh-timer 'due))
              (refresh-mail-labels)
              (expect mail-label-refresh-timer :to-be nil))
            (expect (get-text-property 1 'fontified gmail) :to-be nil)
            (expect (get-text-property 1 'fontified news) :to-be t)
            (expect (nreverse drawn)
                    :to-equal (list (list 'lines thread) (list 'header thread)
                                    (list 'collect collect))))
        (mapc #'kill-buffer (list gmail news thread collect other))))))

(describe "refresh-mail-labels-soon"
  (it "draws once after the command, however many folders it changed"
    (let ((mail-label-refresh-timer nil))
      (unwind-protect
          (progn
            (refresh-mail-labels-soon "job" "gmail")
            (let ((timer mail-label-refresh-timer))
              (expect (timerp timer) :to-be t)
              (expect (timer--function timer) :to-be 'refresh-mail-labels)
              (refresh-mail-labels-soon '(1 2) "kids" "gmail" t)
              (expect mail-label-refresh-timer :to-be timer)))
        (when (timerp mail-label-refresh-timer)
          (cancel-timer mail-label-refresh-timer))))))

;;; labels-tests.el ends here
