;;; tests/scripts/reddit-fetch-tests.el --- Reddit fetch specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "scripts/reddit-fetch.el")

(defvar reddit-fetch-tests--script
  (expand-file-name "scripts/reddit-fetch.el" test-config-root)
  "The script a child Emacs runs.")

(defconst reddit-fetch-tests--base "http://reddit.test"
  "Base URL the stand-in for the browser answers.")

;;; Reddit's JSON

(defun reddit-fetch-tests--time (days-ago)
  "Reddit's created_utc DAYS-AGO days before now."
  (float-time (time-subtract nil (* days-ago 86400))))

(defun reddit-fetch-tests--listing (things)
  "A listing of THINGS."
  (list :kind "Listing" :data (list :children (vconcat things))))

(defun reddit-fetch-tests--post (id author title days &optional comments link)
  "Post ID by AUTHOR titled TITLE, DAYS old, holding COMMENTS; LINK makes it a link post."
  (list :kind "t3"
        :data (list :id id :name (concat "t3_" id) :author author :title title
                    :created_utc (reddit-fetch-tests--time days)
                    :permalink (format "/r/test/comments/%s/slug/" id)
                    :url (or link (format "https://www.reddit.com/r/test/comments/%s/slug/" id))
                    :is_self (if link :false t)
                    :selftext_html (if link :null (format "<p>%s says %s</p>" author id))
                    :num_comments (or comments 0))))

(defun reddit-fetch-tests--comment (id post author days &rest replies)
  "Comment ID on POST by AUTHOR, DAYS old, answered by REPLIES."
  (list :kind "t1"
        :data (list :id id :name (concat "t1_" id) :link_id (concat "t3_" post)
                    :author author
                    :created_utc (reddit-fetch-tests--time days)
                    :permalink (format "/r/test/comments/%s/slug/%s/" post id)
                    :body_html (format "<p>%s says %s</p>" author id)
                    :replies (if replies (reddit-fetch-tests--listing replies) ""))))

(defun reddit-fetch-tests--thread (post &rest comments)
  "The thread of POST holding the top-level COMMENTS."
  (vector (reddit-fetch-tests--listing (list post))
          (reddit-fetch-tests--listing comments)))

(defun reddit-fetch-tests--parse (data)
  "DATA as a fetch hands it over: serialized, then parsed."
  (json-parse-string (json-serialize data)
                     :object-type 'hash-table :null-object nil :false-object nil))

(defun reddit-fetch-tests--url (kind &optional post)
  "URL of the posts listing, the comments listing or POST's thread, by KIND."
  (pcase kind
    ('posts (format "%s/r/test/new.json?limit=100&raw_json=1" reddit-fetch-tests--base))
    ('comments (format "%s/r/test/comments.json?limit=100&raw_json=1" reddit-fetch-tests--base))
    ('thread (format "%s/r/test/comments/%s.json?limit=500&raw_json=1"
                     reddit-fetch-tests--base post))))

;;; A stand-in for the browser, answering from files named after the URL

(defun reddit-fetch-tests--file (fixtures url)
  "File under FIXTURES the stand-in answers URL from."
  (expand-file-name (replace-regexp-in-string "[^A-Za-z0-9]" "_" url) fixtures))

(defun reddit-fetch-tests--write-browser (program fixtures log)
  "Write PROGRAM, which dumps the page of a URL served under FIXTURES.
It logs each URL to LOG and each profile to LOG.profiles, answers no JSON
to a HeadlessChrome agent, a profile without cookies or a URL not served,
and hangs on a URL whose file has a .hang sibling."
  (with-temp-file program
    (insert "#!/bin/sh\n"
            "for arg; do url=$arg; done\n"
            "for arg; do case $arg in --user-data-dir=*) profile=${arg#--user-data-dir=};; esac; done\n"
            "printf '%s\\n' \"$url\" >> '" log "'\n"
            "printf '%s\\n' \"$profile\" >> '" log ".profiles'\n"
            "file='" fixtures "'$(printf '%s' \"$url\" | tr -c 'A-Za-z0-9' '_')\n"
            "if [ -f \"$file.hang\" ]; then exec sleep 30; fi\n"
            "printf '<html><head></head><body>'\n"
            "case \"$*\" in *HeadlessChrome*) file=/nonexistent;; esac\n"
            "if [ -f \"$profile/Default/Cookies\" ] && [ -f \"$file\" ]; then\n"
            "  printf '<pre style=\"word-wrap: break-word; white-space: pre-wrap;\">'\n"
            "  sed -e 's/&/\\&amp;/g' -e 's/</\\&lt;/g' -e 's/>/\\&gt;/g' \"$file\"\n"
            "  printf '</pre>'\n"
            "fi\n"
            "printf '</body></html>\\n'\n"))
  (set-file-modes program #o755))

(defun reddit-fetch-tests--write-cookies (file &rest rows)
  "Write FILE, a cookie database of ROWS, each (HOST NAME VALUE); return it open."
  (let ((db (sqlite-open file)))
    (sqlite-execute db "PRAGMA journal_mode=WAL")
    (sqlite-execute db "CREATE TABLE cookies (host_key TEXT, name TEXT, value TEXT)")
    (dolist (row rows)
      (sqlite-execute db "INSERT INTO cookies VALUES (?, ?, ?)" row))
    db))

(defun reddit-fetch-tests--cookie-rows (file)
  "Hosts and names of the cookies in the database FILE, sorted."
  (let ((db (sqlite-open file)))
    (unwind-protect
        (sqlite-select db "SELECT host_key, name FROM cookies ORDER BY host_key, name")
      (sqlite-close db))))

(defun reddit-fetch-tests--no-json (url)
  "What a fetch of URL says when the page it got holds no JSON."
  (format "%s: no JSON in the page; a rate limit, a block or a lost login" url))

(defun reddit-fetch-tests--serve (fixtures url data)
  "Have the stand-in under FIXTURES answer URL with DATA as JSON."
  (let ((coding-system-for-write 'no-conversion))
    (write-region (json-serialize data) nil (reddit-fetch-tests--file fixtures url) nil 'silent)))

(defmacro reddit-fetch-tests--with-store (&rest body)
  "Run BODY with a store holding r.test, the browser's stand-in and a login.
BODY sees `store', `fixtures' and `log'."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "reddit-fetch-tests" t)))
          (store (expand-file-name "store/" root))
          (fixtures (expand-file-name "fixtures/" root))
          (log (expand-file-name "log" root))
          (reddit-fetch-browser (expand-file-name "browser" root))
          (reddit-fetch-cookies (expand-file-name "Cookies" root))
          (reddit-fetch-profile nil))
     (make-directory (expand-file-name "r.test" store) t)
     (make-directory fixtures t)
     (reddit-fetch-tests--write-browser reddit-fetch-browser fixtures log)
     (sqlite-close (reddit-fetch-tests--write-cookies
                    reddit-fetch-cookies '(".reddit.com" "token_v2" "t")))
     (unwind-protect (progn ,@body)
       (when reddit-fetch-profile
         (delete-directory reddit-fetch-profile t))
       (delete-directory root t))))

(defun reddit-fetch-tests--run (store)
  "Fetch every folder under STORE from the stand-in."
  (reddit-fetch-run store reddit-fetch-tests--base))

(defun reddit-fetch-tests--messages (store)
  "Messages of r.test under STORE as (MESSAGE-ID REFERENCES READ), by Message-ID."
  (let ((cur (expand-file-name "r.test/cur" store)))
    (sort (mapcar (lambda (file)
                    (with-temp-buffer
                      (insert-file-contents (expand-file-name file cur))
                      (narrow-to-region (point-min) (or (search-forward "\n\n" nil t) (point-max)))
                      (list (mail-fetch-field "Message-ID")
                            (mail-fetch-field "References")
                            (and (string-match-p "S" (or (cadr (split-string file ":2,")) "")) t))))
                  (directory-files cur nil "\\`[^.]"))
          (lambda (a b) (string< (car a) (car b))))))

(defun reddit-fetch-tests--requests (log)
  "URLs the stand-in was asked for, in order, from LOG."
  (when (file-exists-p log)
    (with-temp-buffer
      (insert-file-contents log)
      (split-string (buffer-string) "\n" t))))

(defun reddit-fetch-tests--serve-subreddit (fixtures)
  "Serve r/test under FIXTURES: an old post, and two threads with comments.
p2 holds c1, its reply c2 and c2's reply c3, the last answered by
nobody; c4 is older than a week.  p3 holds c5."
  (reddit-fetch-tests--serve
   fixtures (reddit-fetch-tests--url 'posts)
   (reddit-fetch-tests--listing
    (list (reddit-fetch-tests--post "p3" "eve" "Post three" 0.5 1)
          (reddit-fetch-tests--post "p2" "bob" "Post two" 9 4)
          (reddit-fetch-tests--post "p1" "ann" "Post one" 10))))
  (reddit-fetch-tests--serve
   fixtures (reddit-fetch-tests--url 'comments)
   (reddit-fetch-tests--listing
    (list (reddit-fetch-tests--comment "c5" "p3" "dan" 0.1)
          (reddit-fetch-tests--comment "c3" "p2" "cat" 0.2))))
  (reddit-fetch-tests--serve
   fixtures (reddit-fetch-tests--url 'thread "p2")
   (reddit-fetch-tests--thread
    (reddit-fetch-tests--post "p2" "bob" "Post two" 9 4)
    (reddit-fetch-tests--comment "c4" "p2" "dan" 8)
    (reddit-fetch-tests--comment
     "c1" "p2" "cat" 1
     (reddit-fetch-tests--comment
      "c2" "p2" "ann" 0.5
      (reddit-fetch-tests--comment "c3" "p2" "cat" 0.2)))))
  (reddit-fetch-tests--serve
   fixtures (reddit-fetch-tests--url 'thread "p3")
   (reddit-fetch-tests--thread
    (reddit-fetch-tests--post "p3" "eve" "Post three" 0.5 1)
    (reddit-fetch-tests--comment "c5" "p3" "dan" 0.1))))

(defconst reddit-fetch-tests--delivered
  '(("<c1.p2@reddit.com>" "<p2@reddit.com>" nil)
    ("<c2.p2@reddit.com>" "<p2@reddit.com> <c1.p2@reddit.com>" nil)
    ("<c3.p2@reddit.com>" "<p2@reddit.com> <c1.p2@reddit.com> <c2.p2@reddit.com>" nil)
    ("<c4.p2@reddit.com>" "<p2@reddit.com>" t)
    ("<c5.p3@reddit.com>" "<p3@reddit.com>" nil)
    ("<p1@reddit.com>" nil t)
    ("<p2@reddit.com>" nil t)
    ("<p3@reddit.com>" nil nil))
  "What a first run over `reddit-fetch-tests--serve-subreddit' delivers.")

;;; Specs

(describe "reddit-fetch-copy-cookies"
  (it "copies the Reddit cookies of a database the browser holds open, unsynced rows too"
    (let* ((root (make-temp-file "reddit-fetch-cookies" t))
           (source (expand-file-name "Cookies" root))
           (dest (expand-file-name "profile/Default/Cookies" root))
           (db (reddit-fetch-tests--write-cookies
                source '(".reddit.com" "token_v2" "t") '(".example.com" "id" "e"))))
      (unwind-protect
          (progn
            ;; a row the open database keeps in its WAL file
            (sqlite-execute db "INSERT INTO cookies VALUES ('www.reddit.com', 'session', 's')")
            (reddit-fetch-copy-cookies source dest)
            (expect (reddit-fetch-tests--cookie-rows dest)
                    :to-equal '((".reddit.com" "token_v2") ("www.reddit.com" "session")))
            (sqlite-close db)
            (setq db nil)
            (expect (length (reddit-fetch-tests--cookie-rows source)) :to-equal 3))
        (when db
          (sqlite-close db))
        (delete-directory root t))))
  (it "says so when the browser has no cookie database"
    (expect (reddit-fetch-copy-cookies nil "/tmp/never")
            :to-throw 'error '("No browser cookie database at nil"))
    (expect (reddit-fetch-copy-cookies "/nonexistent/Cookies" "/tmp/never")
            :to-throw 'error '("No browser cookie database at /nonexistent/Cookies"))))

(describe "reddit-fetch-browser-command"
  (it "runs the browser headless over the profile, under an agent Reddit takes"
    (let* ((reddit-fetch-browser "/bin/brave")
           (command (reddit-fetch-browser-command "http://reddit.test/x.json" "/tmp/profile")))
      (expect (car command) :to-equal "/bin/brave")
      (expect (car (last command)) :to-equal "http://reddit.test/x.json")
      (expect (cdr command) :to-contain "--headless=new")
      (expect (cdr command) :to-contain "--dump-dom")
      (expect (cdr command) :to-contain "--user-data-dir=/tmp/profile")
      (expect (cdr command) :to-contain (concat "--user-agent=" reddit-fetch-user-agent))
      (expect reddit-fetch-user-agent :not :to-match "Headless")))
  (it "says so when there is no browser"
    (let ((reddit-fetch-browser nil))
      (expect (reddit-fetch-browser-command "http://reddit.test/x.json" "/tmp/profile")
              :to-throw 'error '("No Chromium-family browser to fetch Reddit with")))))

(describe "reddit-fetch-page-json"
  (it "reads the JSON a browser's dump holds, unescaping it once"
    (let ((json (reddit-fetch-page-json
                 (concat "<html><head></head><body><pre style=\"word-wrap: break-word;\">"
                         "{\"html\": \"&lt;p&gt;1 &amp;lt; 2 &amp; 3&nbsp;&lt;/p&gt;\", \"n\": [1]}"
                         "</pre></body></html>"))))
      ;; the entity the HTML itself holds survives
      (expect (gethash "html" json) :to-equal "<p>1 &lt; 2 & 3\u00a0</p>")
      (expect (gethash "n" json) :to-equal [1])))
  (it "finds none in a page without a <pre>"
    (expect (reddit-fetch-page-json "<html><head></head><body></body></html>") :to-be nil))
  (it "reads an answer past a megabyte"
    (let ((text (make-string 2000000 ?x)))
      (expect (gethash "a" (reddit-fetch-page-json
                            (format "<html><body><pre>{\"a\": \"%s\"}</pre></body></html>" text)))
              :to-equal text))))

(describe "reddit-fetch-json"
  (it "fetches through the headless browser, over one profile a run holding the login"
    (reddit-fetch-tests--with-store
      (sqlite-close (reddit-fetch-tests--write-cookies
                     (progn (delete-file reddit-fetch-cookies) reddit-fetch-cookies)
                     '(".reddit.com" "token_v2" "t") '(".example.com" "id" "e")))
      (reddit-fetch-tests--serve fixtures "http://reddit.test/a.json" '(:a 1))
      (reddit-fetch-tests--serve fixtures "http://reddit.test/b.json" '(:b "ü"))
      (expect (gethash "a" (reddit-fetch-json "http://reddit.test/a.json")) :to-equal 1)
      (expect (gethash "b" (reddit-fetch-json "http://reddit.test/b.json")) :to-equal "ü")
      (let ((profiles (reddit-fetch-tests--requests (concat log ".profiles"))))
        (expect profiles :to-equal (list reddit-fetch-profile reddit-fetch-profile))
        (expect (reddit-fetch-tests--cookie-rows
                 (expand-file-name "Default/Cookies" reddit-fetch-profile))
                :to-equal '((".reddit.com" "token_v2"))))))
  (it "says which URL answered a page with no JSON"
    (reddit-fetch-tests--with-store
      (expect (reddit-fetch-json "http://reddit.test/gone.json")
              :to-throw 'error (list (reddit-fetch-tests--no-json "http://reddit.test/gone.json")))))
  (it "gives up on a browser that hangs, and stops it"
    (reddit-fetch-tests--with-store
      (let ((reddit-fetch-timeout 1)
            (url "http://reddit.test/slow.json"))
        (write-region "" nil (concat (reddit-fetch-tests--file fixtures url) ".hang"))
        (expect (reddit-fetch-json url) :to-throw 'error (list (concat url ": no answer in 1 s")))
        (expect (seq-filter (lambda (process)
                              (string-prefix-p "reddit-fetch-browser" (process-name process)))
                            (process-list))
                :to-equal nil)))))

(describe "reddit-fetch-entry"
  (it "reads a post's ids, author, title, time, link, text and comment count"
    (let ((entry (reddit-fetch-entry
                  (reddit-fetch-tests--parse (reddit-fetch-tests--post "p2" "bob" "Post <two>" 1 3))
                  reddit-fetch-tests--base)))
      (expect (list (plist-get entry :id) (plist-get entry :post) (plist-get entry :comment)
                    (plist-get entry :author) (plist-get entry :title) (plist-get entry :comments))
              :to-equal '("t3_p2" "p2" nil "u/bob" "Post <two>" 3))
      (expect (plist-get entry :link) :to-equal "http://reddit.test/r/test/comments/p2/slug/")
      (expect (plist-get entry :html) :to-equal "<p>bob says p2</p>")
      (expect (- (float-time) (plist-get entry :time)) :to-be-close-to 86400 -1)))
  (it "links a link post's target"
    (expect (plist-get (reddit-fetch-entry
                        (reddit-fetch-tests--parse
                         (reddit-fetch-tests--post "p4" "bob" "Look" 1 0 "https://example.com/?a=1&b=2"))
                        reddit-fetch-tests--base)
                       :html)
            :to-equal (concat "<p><a href=\"https://example.com/?a=1&amp;b=2\">"
                              "https://example.com/?a=1&amp;b=2</a></p>")))
  (it "reads a comment's post, its ancestors and the title it is given"
    (let ((entry (reddit-fetch-entry
                  (reddit-fetch-tests--parse (reddit-fetch-tests--comment "c2" "p2" "[deleted]" 0.5))
                  reddit-fetch-tests--base "Post two" '("c1"))))
      (expect (list (plist-get entry :id) (plist-get entry :post) (plist-get entry :comment)
                    (plist-get entry :ancestors) (plist-get entry :author) (plist-get entry :title))
              :to-equal '("t1_c2" "p2" "c2" ("c1") "u/[deleted]" "Post two"))
      (expect (plist-get entry :link) :to-equal "http://reddit.test/r/test/comments/p2/slug/c2/")
      (expect (plist-get entry :html) :to-equal "<p>[deleted] says c2</p>"))))

(describe "reddit-fetch-thread"
  (it "lists the post, then each comment after its parent, with its ancestors"
    (let ((thread (reddit-fetch-thread
                   (reddit-fetch-tests--parse
                    (vector (reddit-fetch-tests--listing
                             (list (reddit-fetch-tests--post "p2" "bob" "Post two" 1 5)))
                            (reddit-fetch-tests--listing
                             (list (reddit-fetch-tests--comment
                                    "c1" "p2" "cat" 0.9
                                    (reddit-fetch-tests--comment
                                     "c2" "p2" "ann" 0.8
                                     (reddit-fetch-tests--comment "c3" "p2" "cat" 0.7))
                                    (reddit-fetch-tests--comment "c4" "p2" "dan" 0.6))
                                   (reddit-fetch-tests--comment "c5" "p2" "eve" 0.5)
                                   ;; what a thread too long to send whole holds back
                                   (list :kind "more" :data (list :count 7 :children ["c9"]))))))
                   reddit-fetch-tests--base)))
      (expect (mapcar (lambda (entry) (list (plist-get entry :id) (plist-get entry :ancestors)))
                      thread)
              :to-equal '(("t3_p2" nil) ("t1_c1" nil) ("t1_c2" ("c1")) ("t1_c3" ("c1" "c2"))
                          ("t1_c4" ("c1")) ("t1_c5" nil)))
      (expect (seq-uniq (mapcar (lambda (entry) (plist-get entry :title)) thread))
              :to-equal '("Post two")))))

(describe "reddit-fetch-message"
  (let ((post '(:id "t3_p2" :post "p2" :author "u/bob" :title "Ünicode post"
                    :time 0 :link "https://www.reddit.com/r/test/comments/p2/slug/"
                    :html "<p>hi</p>"))
        (top '(:id "t1_c1" :post "p2" :comment "c1" :author "u/cat" :title "Ünicode post"
                   :time 0 :link "https://www.reddit.com/r/test/comments/p2/slug/c1/"
                   :html "<p>one</p>"))
        (reply '(:id "t1_c3" :post "p2" :comment "c3" :ancestors ("c1" "c2")
                     :author "u/[deleted]" :title "Ünicode post"
                     :time 0 :link "https://www.reddit.com/r/test/comments/p2/slug/c3/"
                     :html "<p>three</p>")))
    (it "gives a post its own Message-ID, an encoded subject and its link"
      (with-temp-buffer
        (insert (reddit-fetch-message post))
        (expect (mail-fetch-field "Message-ID") :to-equal "<p2@reddit.com>")
        (expect (mail-fetch-field "References") :to-be nil)
        (expect (rfc2047-decode-string (mail-fetch-field "Subject")) :to-equal "Ünicode post")
        (expect (mail-fetch-field "From") :to-equal "\"u/bob\" <bob@reddit.invalid>")
        (expect (mail-fetch-field "Date") :to-equal "Thu, 01 Jan 1970 00:00:00 +0000")
        (expect (mail-fetch-field "Archived-At")
                :to-equal "<https://www.reddit.com/r/test/comments/p2/slug/>")
        (expect (mail-fetch-field "Content-Type") :to-equal "text/html; charset=utf-8")
        (expect (buffer-string) :to-match "\n\n<p>hi</p>\n\\'")))
    (it "threads a top-level comment under its post"
      (with-temp-buffer
        (insert (reddit-fetch-message top))
        (expect (mail-fetch-field "Message-ID") :to-equal "<c1.p2@reddit.com>")
        (expect (mail-fetch-field "References") :to-equal "<p2@reddit.com>")
        (expect (mail-fetch-field "In-Reply-To") :to-equal "<p2@reddit.com>")
        (expect (rfc2047-decode-string (mail-fetch-field "Subject")) :to-equal "Re: Ünicode post")))
    (it "threads a reply under its parent, its References naming every ancestor"
      (with-temp-buffer
        (insert (reddit-fetch-message reply))
        (expect (mail-fetch-field "References")
                :to-equal "<p2@reddit.com> <c1.p2@reddit.com> <c2.p2@reddit.com>")
        (expect (mail-fetch-field "In-Reply-To") :to-equal "<c2.p2@reddit.com>")
        (expect (mail-fetch-field "From") :to-equal "\"u/[deleted]\" <_deleted_@reddit.invalid>")))))

(describe "reddit-fetch-run"
  (it "delivers new posts, then every comment of each thread that has any, oldest first"
    (reddit-fetch-tests--with-store
      (reddit-fetch-tests--serve-subreddit fixtures)
      (expect (reddit-fetch-tests--run store) :to-equal '(("r.test" 3 5 0 nil)))
      ;; posts and comments older than a week arrive read
      (expect (reddit-fetch-tests--messages store) :to-equal reddit-fetch-tests--delivered)
      ;; p1 holds no comment, so its thread is never asked for
      (expect (reddit-fetch-tests--requests log)
              :to-equal (list (reddit-fetch-tests--url 'posts)
                              (reddit-fetch-tests--url 'comments)
                              (reddit-fetch-tests--url 'thread "p2")
                              (reddit-fetch-tests--url 'thread "p3")))))

  (it "fetches a thread past the listing before every listed one"
    (reddit-fetch-tests--with-store
      (reddit-fetch-tests--serve-subreddit fixtures)
      (reddit-fetch-tests--serve
       fixtures (reddit-fetch-tests--url 'comments)
       (reddit-fetch-tests--listing
        (list (reddit-fetch-tests--comment "c5" "p3" "dan" 0.1)
              (reddit-fetch-tests--comment "c8" "p0" "ann" 0.1))))
      (reddit-fetch-tests--serve
       fixtures (reddit-fetch-tests--url 'thread "p0")
       (reddit-fetch-tests--thread
        (reddit-fetch-tests--post "p0" "eve" "Post zero" 30 1)
        (reddit-fetch-tests--comment "c8" "p0" "ann" 0.1)))
      (expect (reddit-fetch-tests--run store) :to-equal '(("r.test" 3 6 0 nil)))
      (expect (cddr (reddit-fetch-tests--requests log))
              :to-equal (list (reddit-fetch-tests--url 'thread "p0")
                              (reddit-fetch-tests--url 'thread "p2")
                              (reddit-fetch-tests--url 'thread "p3")))))

  (it "asks for no thread while none changed"
    (reddit-fetch-tests--with-store
      (reddit-fetch-tests--serve-subreddit fixtures)
      (reddit-fetch-tests--run store)
      (delete-file log)
      (expect (reddit-fetch-tests--run store) :to-equal '(("r.test" 0 0 0 nil)))
      (expect (reddit-fetch-tests--requests log)
              :to-equal (list (reddit-fetch-tests--url 'posts)
                              (reddit-fetch-tests--url 'comments)))))

  (it "fetches a thread again once its count changes, and delivers what is new"
    (reddit-fetch-tests--with-store
      (reddit-fetch-tests--serve-subreddit fixtures)
      (reddit-fetch-tests--run store)
      (reddit-fetch-tests--serve
       fixtures (reddit-fetch-tests--url 'posts)
       (reddit-fetch-tests--listing
        (list (reddit-fetch-tests--post "p3" "eve" "Post three" 0.5 2)
              (reddit-fetch-tests--post "p2" "bob" "Post two" 9 4)
              (reddit-fetch-tests--post "p1" "ann" "Post one" 10))))
      (reddit-fetch-tests--serve
       fixtures (reddit-fetch-tests--url 'thread "p3")
       (reddit-fetch-tests--thread
        (reddit-fetch-tests--post "p3" "eve" "Post three" 0.5 2)
        (reddit-fetch-tests--comment
         "c5" "p3" "dan" 0.1
         (reddit-fetch-tests--comment "c6" "p3" "eve" 0.05))))
      (delete-file log)
      (expect (reddit-fetch-tests--run store) :to-equal '(("r.test" 0 1 0 nil)))
      (expect (assoc "<c6.p3@reddit.com>" (reddit-fetch-tests--messages store))
              :to-equal '("<c6.p3@reddit.com>" "<p3@reddit.com> <c5.p3@reddit.com>" nil))
      (expect (reddit-fetch-tests--requests log)
              :to-equal (list (reddit-fetch-tests--url 'posts)
                              (reddit-fetch-tests--url 'comments)
                              (reddit-fetch-tests--url 'thread "p3")))))

  (it "fetches a thread past the listing once a new comment shows there, its post read"
    (reddit-fetch-tests--with-store
      (reddit-fetch-tests--serve fixtures (reddit-fetch-tests--url 'posts)
                                 (reddit-fetch-tests--listing nil))
      (reddit-fetch-tests--serve
       fixtures (reddit-fetch-tests--url 'comments)
       (reddit-fetch-tests--listing (list (reddit-fetch-tests--comment "c8" "p0" "ann" 0.1))))
      (reddit-fetch-tests--serve
       fixtures (reddit-fetch-tests--url 'thread "p0")
       (reddit-fetch-tests--thread
        (reddit-fetch-tests--post "p0" "eve" "Post zero" 3 2)
        (reddit-fetch-tests--comment
         "c7" "p0" "cat" 2
         (reddit-fetch-tests--comment "c8" "p0" "ann" 0.1))))
      (expect (reddit-fetch-tests--run store) :to-equal '(("r.test" 0 2 0 nil)))
      (expect (reddit-fetch-tests--messages store)
              :to-equal '(("<c7.p0@reddit.com>" "<p0@reddit.com>" nil)
                          ("<c8.p0@reddit.com>" "<p0@reddit.com> <c7.p0@reddit.com>" nil)
                          ("<p0@reddit.com>" nil t)))
      ;; the thread's count is kept only while the listing holds it
      (expect (reddit-fetch-tests--run store) :to-equal '(("r.test" 0 0 0 nil)))
      (expect (reddit-fetch-read-state store "r.test" reddit-fetch-threads-file) :to-be nil)))

  (it "never delivers an id twice, even one deleted from the folder or delivered before"
    (reddit-fetch-tests--with-store
      (reddit-fetch-tests--serve-subreddit fixtures)
      ;; an earlier fetch delivered c5, and the user deleted it since
      (reddit-fetch-save-state store "r.test" reddit-fetch-known-file '("t1_c5"))
      (expect (reddit-fetch-tests--run store) :to-equal '(("r.test" 3 4 0 nil)))
      (let ((cur (expand-file-name "r.test/cur" store)))
        (dolist (file (directory-files cur t "\\`[^.]"))
          (with-temp-buffer
            (insert-file-contents file)
            (when (string-match-p "<p2@reddit.com>" (or (mail-fetch-field "Message-ID") ""))
              (delete-file file)))))
      ;; with no count kept, every thread is fetched again
      (reddit-fetch-save-state store "r.test" reddit-fetch-threads-file nil)
      (expect (reddit-fetch-tests--run store) :to-equal '(("r.test" 0 0 0 nil)))
      (expect (mapcar #'car (reddit-fetch-tests--messages store))
              :to-equal '("<c1.p2@reddit.com>" "<c2.p2@reddit.com>" "<c3.p2@reddit.com>"
                          "<c4.p2@reddit.com>" "<p1@reddit.com>" "<p3@reddit.com>"))))

  (it "takes a message the folder holds as delivered, though no state names it"
    (reddit-fetch-tests--with-store
      (reddit-fetch-tests--serve-subreddit fixtures)
      (reddit-fetch-tests--run store)
      (delete-file (reddit-fetch-state-file store "r.test" reddit-fetch-known-file))
      (delete-file (reddit-fetch-state-file store "r.test" reddit-fetch-threads-file))
      (expect (reddit-fetch-tests--run store) :to-equal '(("r.test" 0 0 0 nil)))
      (expect (reddit-fetch-tests--messages store) :to-equal reddit-fetch-tests--delivered)))

  (it "stops at a thread that fails, reports it, and fetches what waits next run"
    (reddit-fetch-tests--with-store
      (reddit-fetch-tests--serve-subreddit fixtures)
      (let ((p2 (reddit-fetch-tests--url 'thread "p2"))
            (failed (expand-file-name "failed" fixtures)))
        (rename-file (expand-file-name (replace-regexp-in-string "[^A-Za-z0-9]" "_" p2) fixtures)
                     failed)
        (pcase-let ((`((,group ,posts ,comments ,waiting ,problem))
                     (reddit-fetch-tests--run store)))
          (expect (list group posts comments waiting) :to-equal '("r.test" 3 0 2))
          (expect problem :to-equal (reddit-fetch-tests--no-json p2)))
        (expect (reddit-fetch-tests--requests log)
                :to-equal (list (reddit-fetch-tests--url 'posts)
                                (reddit-fetch-tests--url 'comments)
                                p2))
        (rename-file failed (expand-file-name (replace-regexp-in-string "[^A-Za-z0-9]" "_" p2)
                                              fixtures))
        (expect (reddit-fetch-tests--run store) :to-equal '(("r.test" 0 5 0 nil)))
        (expect (reddit-fetch-tests--messages store) :to-equal reddit-fetch-tests--delivered))))

  (it "fetches as many threads a run as Reddit's limit leaves room for, the rest next run"
    (reddit-fetch-tests--with-store
      (reddit-fetch-tests--serve-subreddit fixtures)
      (let ((reddit-fetch-max-threads 1))
        (expect (reddit-fetch-tests--run store) :to-equal '(("r.test" 3 4 1 nil)))
        (expect (reddit-fetch-tests--run store) :to-equal '(("r.test" 0 1 0 nil))))
      (expect (reddit-fetch-tests--messages store) :to-equal reddit-fetch-tests--delivered)))

  (it "reports a folder whose listing fails, and still fetches the others"
    (reddit-fetch-tests--with-store
      (reddit-fetch-tests--serve-subreddit fixtures)
      (make-directory (expand-file-name "r.gone" store))
      (expect (reddit-fetch-tests--run store)
              :to-equal `(("r.gone" error
                           ,(reddit-fetch-tests--no-json
                             (format "%s/r/gone/new.json?limit=100&raw_json=1"
                                     reddit-fetch-tests--base)))
                          ("r.test" 3 5 0 nil)))))

  (it "deletes the browser profile of the run, which holds the user's login"
    (reddit-fetch-tests--with-store
      (reddit-fetch-tests--serve-subreddit fixtures)
      (reddit-fetch-tests--run store)
      (let ((profiles (seq-uniq (reddit-fetch-tests--requests (concat log ".profiles")))))
        (expect (length profiles) :to-equal 1)
        (expect (file-exists-p (car profiles)) :to-be nil)
        (expect reddit-fetch-profile :to-be nil)))))

(describe "reddit-fetch-main"
  (cl-flet ((main (store)
              (let ((sandbox (make-temp-file "reddit-fetch-init" t))
                    (output (generate-new-buffer " *reddit-fetch-main*")))
                (unwind-protect
                    (list (call-process
                           (expand-file-name invocation-name invocation-directory) nil output nil
                           "-Q" "--batch" "--init-directory" sandbox
                           "-l" reddit-fetch-tests--script
                           "--eval" (format "(setq reddit-fetch-browser %S reddit-fetch-cookies %S)"
                                            reddit-fetch-browser reddit-fetch-cookies)
                           "--eval" (format "(reddit-fetch-main %S %S)"
                                            store reddit-fetch-tests--base))
                          (with-current-buffer output (buffer-string)))
                  (kill-buffer output)
                  (delete-directory sandbox t)))))
    (it "runs as a program, reports each folder and exits 0"
      (reddit-fetch-tests--with-store
        (reddit-fetch-tests--serve-subreddit fixtures)
        (pcase-let ((`(,status ,output) (main store)))
          (expect status :to-equal 0)
          (expect output :to-match "^r.test: 3 posts, 5 comments, 0 threads waiting$"))
        (expect (reddit-fetch-tests--messages store) :to-equal reddit-fetch-tests--delivered)
        (expect (seq-filter #'file-exists-p
                            (reddit-fetch-tests--requests (concat log ".profiles")))
                :to-equal nil)))
    (it "exits 1 when a listing fails"
      (reddit-fetch-tests--with-store
        (pcase-let ((`(,status ,output) (main store)))
          (expect status :to-equal 1)
          (expect output :to-match "^r.test: .*new.json.*: no JSON in the page"))))))

;;; reddit-fetch-tests.el ends here
