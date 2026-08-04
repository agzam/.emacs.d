;;; tests/web-browsing/reddit-meetup-tests.el --- web-browsing/autoload/reddit-meetup.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/web-browsing/autoload/reddit-meetup.el")

(defun reddit-meetup-tests--time (year month day)
  "Noon of YEAR-MONTH-DAY, the hour the date math normalises to."
  (encode-time (list 0 0 12 day month year nil -1 nil)))

(defun reddit-meetup-tests--ht (&rest pairs)
  "Hash table of PAIRS, the shape reddit JSON parses into."
  (let ((table (make-hash-table :test 'equal)))
    (while pairs
      (puthash (pop pairs) (pop pairs) table))
    table))

(defun reddit-meetup-tests--post-data (&rest pairs)
  "Post of PAIRS, as it sits under a listing child's data key."
  (apply #'reddit-meetup-tests--ht pairs))

(defun reddit-meetup-tests--listing (&rest posts)
  "Submitted listing wrapping POSTS, each a plain list of key/value pairs."
  (reddit-meetup-tests--ht
   "data" (reddit-meetup-tests--ht
           "children"
           (vconcat (mapcar (lambda (post)
                              (reddit-meetup-tests--ht
                               "data" (apply #'reddit-meetup-tests--post-data post)))
                            posts)))))

;; Verbatim bodies of two real announcements: the July one carries a notes
;; link fenced by its own rule, the May one a notes link without it.
(defconst reddit-meetup-tests--july-body
  "This is a monthly reminder. Every first Thursday of the month we meet without a specific agenda to blabber about Emacs stuff and anyone is welcome to join. This time is on July 2nd.

*I understand that it's not the most convenient time for many (6 PM CST - UTC−6; 01:00 CET; 4PM PST), please don't sacrifice your sleep.*

https://www.meetup.com/emacsatx/events/

**DM me if you'd like to join without going through meetup.com curse and I'll send you a direct link to the meeting.**

---

[Notes + video](https://www.reddit.com/r/emacs/comments/1ukwx8l/emacs_atx_meetup_tomorrow_july_2nd/ov8nv8b/).

----
Previous meetings and notes:

https://www.reddit.com/r/emacs/comments/1tvcwd8/emacs_monthly_thursday_meetup/

https://www.reddit.com/r/emacs/comments/1t4mpn3/emacs_atx_meetup_this_thursday/")

(defconst reddit-meetup-tests--may-body
  "Just a monthly reminder. This time is on May 7.

https://www.meetup.com/emacsatx/events/314341747/

----

[Notes and Video](https://www.reddit.com/r/emacs/comments/1t4mpn3/emacs_atx_meetup_this_thursday/okkd3ez/)

Previous meeting notes:

https://www.reddit.com/r/emacs/comments/1sa0jmd/emacs_atx_meetup_tomorrow_thursday/")

(describe "reddit-meetup--first-thursday"
  (it "matches every meeting of the 2026 season"
    (expect (mapcar (lambda (month) (reddit-meetup--first-thursday 2026 month))
                    (number-sequence 1 12))
            :to-equal '(1 5 5 2 7 4 2 6 3 1 5 3)))

  (it "carries into the next year"
    (expect (reddit-meetup--first-thursday 2027 1) :to-equal 7)))

(describe "reddit-meetup--next-meeting"
  (it "keeps this month's meeting while it is still ahead"
    (expect (format-time-string
             "%Y-%m-%d"
             (reddit-meetup--next-meeting (reddit-meetup-tests--time 2026 8 4)))
            :to-equal "2026-08-06"))

  (it "keeps the meeting on the day itself"
    (expect (format-time-string
             "%Y-%m-%d"
             (reddit-meetup--next-meeting (reddit-meetup-tests--time 2026 8 6)))
            :to-equal "2026-08-06"))

  (it "moves on once the meeting has passed"
    (expect (format-time-string
             "%Y-%m-%d"
             (reddit-meetup--next-meeting (reddit-meetup-tests--time 2026 8 7)))
            :to-equal "2026-09-03"))

  (it "rolls over into the next year"
    (expect (format-time-string
             "%Y-%m-%d"
             (reddit-meetup--next-meeting (reddit-meetup-tests--time 2026 12 4)))
            :to-equal "2027-01-07")))

(describe "reddit-meetup--ordinal"
  (it "suffixes by the last digit"
    (expect (mapcar #'reddit-meetup--ordinal '(1 2 3 4 6 21 22 23))
            :to-equal '("1st" "2nd" "3rd" "4th" "6th" "21st" "22nd" "23rd")))

  (it "keeps the teens on th"
    (expect (mapcar #'reddit-meetup--ordinal '(11 12 13))
            :to-equal '("11th" "12th" "13th"))))

(describe "reddit-meetup--title"
  (let ((meeting (reddit-meetup-tests--time 2026 8 6)))
    (it "says today on the day"
      (expect (reddit-meetup--title meeting (reddit-meetup-tests--time 2026 8 6))
              :to-equal "Emacs ATX meetup today, in a few hours"))

    (it "says tomorrow on the eve"
      (expect (reddit-meetup--title meeting (reddit-meetup-tests--time 2026 8 5))
              :to-equal "Emacs ATX meetup tomorrow, August 6th"))

    (it "says this Thursday within the week"
      (expect (reddit-meetup--title meeting (reddit-meetup-tests--time 2026 8 4))
              :to-equal "Emacs ATX meetup this Thursday, August 6th"))

    (it "names the date when it is further out"
      (expect (reddit-meetup--title meeting (reddit-meetup-tests--time 2026 7 25))
              :to-equal "Emacs ATX meetup on August 6th"))))

(describe "reddit-meetup--set-date"
  (it "rewrites the ordinal phrasing"
    (expect (reddit-meetup--set-date "This time is on July 2nd. Bring tea."
                                     "August 6th")
            :to-equal "This time is on August 6th. Bring tea."))

  (it "rewrites the bare phrasing"
    (expect (reddit-meetup--set-date "This time is on May 7." "August 6th")
            :to-equal "This time is on August 6th."))

  (it "leaves a body without the sentence alone"
    (expect (reddit-meetup--set-date "No date here." "August 6th")
            :to-equal "No date here.")))

(describe "reddit-meetup--strip-notes-link"
  (it "drops the link and the rule that fenced it"
    (let ((body (reddit-meetup--strip-notes-link reddit-meetup-tests--july-body)))
      (expect body :not :to-match "\\[Notes \\+ video\\]")
      (expect body :not :to-match "ov8nv8b")
      ;; the two adjacent rules collapse into the one the list still needs
      (expect body :not :to-match "^---[ \t]*\n+----")
      (expect body :to-match "^----\nPrevious meetings and notes:")))

  (it "keeps a rule that still separates sections"
    (let ((body (reddit-meetup--strip-notes-link reddit-meetup-tests--may-body)))
      (expect body :not :to-match "Notes and Video")
      (expect body :to-match "----\n\nPrevious meeting notes:")))

  (it "leaves a body with no notes link alone"
    (expect (reddit-meetup--strip-notes-link "Plain body.\n")
            :to-equal "Plain body.\n")))

(describe "reddit-meetup--add-previous"
  (it "puts the url at the head of the list"
    (expect (reddit-meetup--add-previous
             "Previous meetings and notes:\n\nhttps://old/one\n"
             "https://new/two")
            :to-equal
            "Previous meetings and notes:\n\nhttps://new/two\n\nhttps://old/one\n"))

  (it "does not list the same url twice"
    (let ((body "Previous meetings and notes:\n\nhttps://new/two\n"))
      (expect (reddit-meetup--add-previous body "https://new/two")
              :to-equal body)))

  (it "starts a list when the body has none"
    (expect (reddit-meetup--add-previous "Body.\n" "https://new/two")
            :to-equal
            "Body.\n\n----\nPrevious meetings and notes:\n\nhttps://new/two\n")))

(describe "reddit-meetup--compose-body"
  (let ((body (reddit-meetup--compose-body
               reddit-meetup-tests--july-body
               "August 6th"
               "https://www.reddit.com/r/emacs/comments/1ukwx8l/emacs_atx_meetup_tomorrow_july_2nd/")))
    (it "announces the new date"
      (expect body :to-match "This time is on August 6th\\."))

    (it "carries the boilerplate over untouched"
      (expect body :to-match "https://www\\.meetup\\.com/emacsatx/events/")
      (expect body :to-match "DM me if you'd like to join"))

    (it "promotes the previous announcement to the top of the list"
      (expect body :to-match
              (concat "Previous meetings and notes:\n\n"
                      "https://www\\.reddit\\.com/r/emacs/comments/1ukwx8l/[^\n]*\n\n"
                      "https://www\\.reddit\\.com/r/emacs/comments/1tvcwd8/")))

    (it "drops the notes link of the meeting that already happened"
      (expect body :not :to-match "Notes \\+ video"))))

(describe "reddit-meetup--stated-date"
  (it "reads the ordinal phrasing"
    (expect (reddit-meetup--stated-date "This time is on July 2nd. See you.")
            :to-equal '(7 . 2)))

  (it "reads the bare phrasing"
    (expect (reddit-meetup--stated-date "This time is on May 7.")
            :to-equal '(5 . 7)))

  (it "reads an abbreviated month"
    (expect (reddit-meetup--stated-date "This time is on Aug 6.")
            :to-equal '(8 . 6)))

  (it "returns nil without the sentence"
    (expect (reddit-meetup--stated-date "We meet every first Thursday.")
            :to-be nil))

  (it "returns nil on a month it cannot resolve"
    (expect (reddit-meetup--stated-date "This time is on Smarch 4.")
            :to-be nil)))

(describe "reddit-meetup--announcement-p"
  (it "takes a body carrying the meetup link"
    (expect (reddit-meetup--announcement-p
             (reddit-meetup-tests--post-data
              "subreddit" "emacs"
              "title" "Anything at all"
              "selftext" "https://www.meetup.com/emacsatx/events/"))
            :to-be t))

  (it "takes a body that only names the date"
    (expect (reddit-meetup--announcement-p
             (reddit-meetup-tests--post-data
              "subreddit" "emacs"
              "title" "Anything at all"
              "selftext" "This time is on August 6th."))
            :to-be t))

  (it "passes over an unrelated post"
    (expect (reddit-meetup--announcement-p
             (reddit-meetup-tests--post-data
              "subreddit" "emacs"
              "title" "Emacs meetup videos I liked"
              "selftext" "A list of talks."))
            :to-be nil))

  (it "passes over another subreddit"
    (expect (reddit-meetup--announcement-p
             (reddit-meetup-tests--post-data
              "subreddit" "clojure"
              "title" "Meetup"
              "selftext" "https://www.meetup.com/emacsatx/events/"))
            :to-be nil)))

(describe "reddit-meetup--find-announcement"
  (it "takes the newest announcement, whatever the newer posts are titled"
    (expect (gethash "id"
                     (reddit-meetup--find-announcement
                      (reddit-meetup-tests--listing
                       '("id" "package" "subreddit" "emacs"
                         "title" "Tiny package, may come handy"
                         "selftext" "Nothing to do with meetups.")
                       '("id" "wanted" "subreddit" "emacs"
                         "title" "Something completely different"
                         "selftext" "This time is on July 2nd.\nhttps://www.meetup.com/emacsatx/events/")
                       '("id" "older" "subreddit" "emacs"
                         "title" "Emacs ATX meetup"
                         "selftext" "This time is on June 4."))))
            :to-equal "wanted"))

  (it "returns nil when there is no announcement to work from"
    (expect (reddit-meetup--find-announcement
             (reddit-meetup-tests--listing
              '("subreddit" "emacs" "title" "Something else"
                "selftext" "A post about nothing.")))
            :to-be nil)))

(describe "reddit-meetup--announced-for"
  (it "finds the announcement already covering the date"
    (expect (gethash "id"
                     (reddit-meetup--announced-for
                      (reddit-meetup-tests--listing
                       '("id" "august" "subreddit" "emacs" "title" "Whatever"
                         "selftext" "This time is on August 6th.")
                       '("id" "july" "subreddit" "emacs" "title" "Whatever"
                         "selftext" "This time is on July 2nd."))
                      '(8 . 6)))
            :to-equal "august"))

  (it "clears the date when every announcement covers another one"
    (expect (reddit-meetup--announced-for
             (reddit-meetup-tests--listing
              '("id" "july" "subreddit" "emacs" "title" "Whatever"
                "selftext" "This time is on July 2nd.\nhttps://www.meetup.com/emacsatx/events/"))
             '(8 . 6))
            :to-be nil))

  (it "clears a date no announcement at all has been posted for"
    (expect (reddit-meetup--announced-for (reddit-meetup-tests--listing) '(8 . 6))
            :to-be nil))

  (it "refuses to vouch for a newest announcement whose date it cannot read"
    (expect (gethash "id"
                     (reddit-meetup--announced-for
                      (reddit-meetup-tests--listing
                       '("id" "unreadable" "subreddit" "emacs" "title" "Whatever"
                         "selftext" "We meet on Thursday, https://www.meetup.com/emacsatx/events/"))
                      '(8 . 6)))
            :to-equal "unreadable"))

  (it "still clears the date when only an older announcement is unreadable"
    (expect (reddit-meetup--announced-for
             (reddit-meetup-tests--listing
              '("id" "july" "subreddit" "emacs" "title" "Whatever"
                "selftext" "This time is on July 2nd.")
              '("id" "unreadable" "subreddit" "emacs" "title" "Whatever"
                "selftext" "We meet on Thursday, https://www.meetup.com/emacsatx/events/"))
             '(8 . 6))
            :to-be nil)))

(describe "reddit-meetup--split-draft"
  (it "takes the first line as the title and the rest as the body"
    (expect (reddit-meetup--split-draft "The title\n\nFirst line.\n\nLast line.\n")
            :to-equal '("The title" . "First line.\n\nLast line.")))

  (it "reports an empty title when the draft starts blank"
    (expect (car (reddit-meetup--split-draft "\nbody\n")) :to-equal "")))

(describe "reddit-meetup--submit-params"
  (it "builds a self post carrying the modhash"
    (let ((params (reddit-meetup--submit-params
                   "Title" "Body" "mh" '(:subreddit "emacs"))))
      (expect (cdr (assoc "kind" params)) :to-equal "self")
      (expect (cdr (assoc "sr" params)) :to-equal "emacs")
      (expect (cdr (assoc "title" params)) :to-equal "Title")
      (expect (cdr (assoc "text" params)) :to-equal "Body")
      (expect (cdr (assoc "uh" params)) :to-equal "mh")
      (expect (assoc "flair_id" params) :to-be nil)))

  (it "carries the flair of the previous announcement over"
    (let ((params (reddit-meetup--submit-params
                   "Title" "Body" "mh"
                   '(:subreddit "emacs" :flair-id "e6cc" :flair-text "Announcement"))))
      (expect (cdr (assoc "flair_id" params)) :to-equal "e6cc")
      (expect (cdr (assoc "flair_text" params)) :to-equal "Announcement"))))

(defun reddit-meetup-tests--submit (body listing)
  "Submit BODY against LISTING, reporting what reached reddit.
The promise layer is stubbed synchronous - neither reddigg nor promise
live in the test sandbox, and what matters here is that the guard runs
before the post does."
  (let ((posted nil)
        (refused nil)
        (account (reddit-meetup-tests--ht "name" "someone" "modhash" "mh"))
        (response (reddit-meetup-tests--ht
                   "json" (reddit-meetup-tests--ht
                           "errors" []
                           "data" (reddit-meetup-tests--ht "url" "https://reddit/new"))))
        kill-ring kill-ring-yank-pointer)
    (with-fake-feature 'reddigg
      (cl-letf (((symbol-function 'promise-then)
                 (lambda (value resolve &rest _) (funcall resolve value)))
                ((symbol-function 'promise-catch) (lambda (value _) value))
                ((symbol-function 'reddigg-fetch-json)
                 (lambda (url)
                   (if (string-match-p "me\\.json" url)
                       (reddit-meetup-tests--ht "data" account)
                     listing)))
                ((symbol-function 'reddigg-post-form)
                 (lambda (_url params) (setq posted params) response)))
        (with-temp-buffer
          (insert body)
          (setq reddit-meetup--source
                (list :subreddit "emacs" :meeting (reddit-meetup-tests--time 2026 8 6)))
          (condition-case err
              (reddit-meetup-submit)
            (user-error (setq refused (error-message-string err)))))))
    (list :posted posted :refused refused :copied (car kill-ring))))

(describe "reddit-meetup-submit"
  (it "refuses a meeting that already has an announcement"
    (let ((result (reddit-meetup-tests--submit
                   "A title\n\nThis time is on August 6th.\n"
                   (reddit-meetup-tests--listing
                    '("subreddit" "emacs" "title" "Emacs ATX meetup, August 6th"
                      "url" "https://reddit/august"
                      "selftext" "This time is on August 6th.")))))
      (expect (plist-get result :posted) :to-be nil)
      (expect (plist-get result :refused) :to-match "Already announced")
      (expect (plist-get result :refused) :to-match "https://reddit/august")))

  (it "goes ahead when no announcement covers the meeting yet"
    (let* ((result (reddit-meetup-tests--submit
                    "A title\n\nThis time is on August 6th.\n"
                    (reddit-meetup-tests--listing
                     '("subreddit" "emacs" "title" "Emacs ATX meetup, July 2nd"
                       "url" "https://reddit/july"
                       "selftext" "This time is on July 2nd."))))
           (posted (plist-get result :posted)))
      (expect (plist-get result :refused) :to-be nil)
      (expect (cdr (assoc "title" posted)) :to-equal "A title")
      (expect (cdr (assoc "text" posted)) :to-match "This time is on August 6th\\.")
      (expect (plist-get result :copied) :to-equal "https://reddit/new")))

  (it "reads the date off the draft, not off the meeting it was opened for"
    ;; the draft was hand-edited to September, which nothing announces yet
    (let ((result (reddit-meetup-tests--submit
                   "A title\n\nThis time is on September 3rd.\n"
                   (reddit-meetup-tests--listing
                    '("subreddit" "emacs" "title" "Emacs ATX meetup, August 6th"
                      "url" "https://reddit/august"
                      "selftext" "This time is on August 6th.")))))
      (expect (plist-get result :refused) :to-be nil)
      (expect (plist-get result :posted) :not :to-be nil))))

(describe "reddit-meetup--report-failure"
  ;; a refusal signalled inside a promise handler comes back as an error object
  (it "renders the error object a rejected promise carries"
    (expect (reddit-meetup--report-failure '(user-error "Reddit refused the post: RATELIMIT"))
            :to-equal "reddit: Reddit refused the post: RATELIMIT"))

  (it "passes a transport failure string through"
    (expect (reddit-meetup--report-failure "reddit request failed (HTTP 403)")
            :to-equal "reddit: reddit request failed (HTTP 403)")))

(describe "reddit-meetup--posted-url"
  (it "returns the url of the new post"
    (expect (reddit-meetup--posted-url
             (reddit-meetup-tests--ht
              "json" (reddit-meetup-tests--ht
                      "errors" []
                      "data" (reddit-meetup-tests--ht "url" "https://reddit/new"))))
            :to-equal "https://reddit/new"))

  (it "surfaces the refusal reddit hides in a 200"
    (expect (reddit-meetup--posted-url
             (reddit-meetup-tests--ht
              "json" (reddit-meetup-tests--ht
                      "errors" (vector ["RATELIMIT" "you are doing that too much" "ratelimit"]))))
            :to-throw 'user-error)))
