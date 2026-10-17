;;; modules/email/config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Gnus over a maildir that mbsync mirrors from Gmail: every label is an
;; nnmaildir group, notmuch is only the search index behind gnus-search,
;; and batch Emacs runs fetch gmane's lists and subreddits into maildirs.
;; Outgoing mail always leaves as agzam.ibragimov - sending it through the
;; inbox account with that address as an alias stamps "sent on
;; behalf" - and a Gcc copy lands in the synced sent folder so threads in
;; the mirrored store keep their replies.
;;; Code:

(defvar gmail-maildir (expand-file-name "~/.mail/gmail/")
  "Maildir root mbsync keeps in sync with Gmail; each label is a subdir.")

(defvar news-maildir (expand-file-name "~/.mail/news/")
  "Maildir root the news fetch fills; each news group is a subdir of its name.")

(defvar news-server "news.gmane.io"
  "NNTP server the news groups are fetched from.")

(defvar reddit-maildir (expand-file-name "~/.mail/reddit/")
  "Maildir root the Reddit fetch fills; subreddit NAME is the subdir r.NAME.")

(defvar mail-from-address "agzam.ibragimov@gmail.com"
  "Address every outgoing message is sent from.")

(defvar mail-inbox-address "inbox@example.com"
  "Address whose mailbox mbsync mirrors; never a sender.")

(defvar mail-sync-program "mail-sync"
  "Program that syncs the maildir with Gmail and refreshes the notmuch index.")

(defvar mail-inbox-group "nnmaildir+gmail:inbox"
  "Gnus group `open-mail-inbox' enters.")

(defvar mail-trash-group "nnmaildir+gmail:trash"
  "Group a queued deletion is moved into; mbsync mirrors it from Gmail's Trash.")

(defvar mail-archive-group "nnmaildir+gmail:archive"
  "Group mbsync mirrors from Gmail's All Mail, where archived mail lives.")

(defvar mail-groups (list mail-inbox-group "nnmaildir+news:gmane.emacs.devel"
                          "nnmaildir+news:gmane.emacs.orgmode" "nnmaildir+reddit:r.emacs")
  "Groups Gnus subscribes to on startup, on top of every maildir group.")

(defvar mail-remote-backends '(nntp nnatom)
  "Back ends whose groups cost a round trip to a server to read.")

(defvar mail-bulk-groups
  '("nnmaildir+gmail:archive" "nnmaildir+gmail:emacs" "nnmaildir+gmail:org-mode"
    "nnmaildir+news:gmane.emacs.devel" "nnmaildir+news:gmane.emacs.orgmode"
    "nnmaildir+reddit:r.emacs")
  "Groups no sync rescans: tens of thousands of files each.")

(defvar mail-topics
  '(("Gmail"
     ("nnmaildir+gmail:inbox" . "Inbox")
     ("nnmaildir+gmail:sent" . "Sent Mail")
     ("nnmaildir+gmail:archive" . "All Mail, archived mail included")
     ("nnmaildir+gmail:trash" . "Trash, emptied by Gmail after 30 days"))
    ("Labels"
     ("nnmaildir+gmail:kids" . "School: Frisco ISD, Panther Creek")
     ("nnmaildir+gmail:job" . "Recruiters and interviews")
     ("nnmaildir+gmail:money" . "Receipts, banks, bills")
     ("nnmaildir+gmail:github" . "GitHub notifications")
     ("nnmaildir+gmail:friends" . "Friends and family")
     ("nnmaildir+gmail:Health" . "Doctors, pharmacy")
     ("nnmaildir+gmail:forwarded")
     ("nnmaildir+gmail:subscriptions"))
    ("Lists"
     ("nnmaildir+news:gmane.emacs.devel" . "emacs-devel, fetched from gmane")
     ("nnmaildir+news:gmane.emacs.orgmode" . "emacs-orgmode, fetched from gmane")
     ("nnmaildir+gmail:emacs" . "emacs-devel, delivered to Gmail")
     ("nnmaildir+reddit:r.emacs" . "Reddit, posts and comments")
     ("nnmaildir+gmail:org-mode" . "emacs-orgmode, delivered to Gmail")
     ("nnmaildir+gmail:clojure" . "Clojure groups, Clojurists Together")))
  "Topics under Gnus's root topic, in order: (TOPIC (GROUP . DESCRIPTION)...).
Every start puts these groups back into their topic, in this order, and
lists them even with nothing unread.  Other groups and topics stay put,
but a label in the root topic goes under `mail-label-topic'.")

(defvar mail-label-topic "Labels"
  "Topic of `mail-topics' that a Gmail label Gnus subscribed moves under.")

(defvar mail-label-colors
  '(("clojure" "#3e9a5e" "white")
    ("emacs" "#5279df" "white")
    ("friends" "#225930" "white")
    ("Health" "#e78ca8" "#222222")
    ("job" "#9b811e" "white")
    ("kids" "#af91fb" "#222222")
    ("money" "#3e9a5e" "white")
    ("org-mode" "#734856" "white"))
  "Chip colors of the labels Gmail colors, as (LABEL BACKGROUND TEXT).
Any other label's chip takes the face `mail-label'.")

(defvar mail-treat-quotes t
  "Treatment condition for `highlight-mail-quotes', like the gnus-treat ones.")

;; the news and Reddit stores fill while Emacs runs, whether Gnus runs or not
(add-hook 'emacs-startup-hook #'start-news-fetch-timer)
(add-hook 'emacs-startup-hook #'start-reddit-fetch-timer)

(use-package gnus
  :ensure nil
  :defer t
  :init
  (setq gnus-select-method '(nnnil "")
        gnus-secondary-select-methods
        `((nnmaildir "gmail" (directory ,gmail-maildir) (get-new-mail nil))
          ;; a search names a hit's group by its path under remove-prefix
          (nnmaildir "news" (directory ,news-maildir) (get-new-mail nil)
                     (gnus-search-engine gnus-search-notmuch (remove-prefix ,news-maildir)))
          (nnmaildir "reddit" (directory ,reddit-maildir) (get-new-mail nil)
                     (gnus-search-engine gnus-search-notmuch (remove-prefix ,reddit-maildir))))
        ;; asked for new groups, nnmaildir registers every label it has
        ;; not read, and such a label then opens empty
        gnus-check-new-newsgroups nil
        gnus-save-killed-list nil
        gnus-read-active-file 'some
        gnus-agent nil
        ;; a scan reads every message it has no overview for, so the
        ;; mailing-list labels sit above this level (`mail-bulk-groups')
        ;; and are read on entry instead.  3 is where a subscription
        ;; lands (`gnus-level-default-subscribed'), which gnus.el has not
        ;; defined yet here
        gnus-activate-level 3
        gnus-inhibit-startup-message t
        ;; a dribble left by a session that never saved otherwise asks
        ;; a yes-or-no question before the group buffer appears
        gnus-always-read-dribble-file t
        ;; the inbox alone holds a thousand articles; prompting for a
        ;; count on every entry helps only for the mailing-list groups
        gnus-large-newsgroup 5000
        ;; set before gnus-msg loads, so gnus-confirm-mail-reply-to-news
        ;; derives nil and answering a gmane thread takes no confirmation
        gnus-novice-user nil
        gnus-use-full-window nil
        ;; the bare name, then what `mail-topics' says the group holds
        gnus-group-line-format "%M%S%p%P%5y %B%(%-20G%) %uC\n"
        gnus-suppress-duplicates t
        gnus-refer-thread-use-search t
        ;; entering a group must not open (and mark read) its first article
        gnus-auto-select-first nil
        ;; the Gcc copy is marked read so mbsync uploads it as seen
        gnus-message-archive-group "nnmaildir+gmail:sent"
        gnus-gcc-mark-as-read t
        gnus-posting-styles '((".*" (name user-full-name) (address mail-from-address)))
        ;; the last matching entry wins.  `gnus-group-find-parameter'
        ;; drops an entry whose cdr is a list, so a parameter is a dotted
        ;; pair, and a two-element entry sets a variable buffer-locally.
        ;; nnmaildir evaluates its own parameters, hence the quote.
        gnus-parameters
        `(;; nnmaildir deletes expired files, and mbsync would push that
          ;; to Gmail as an archive or an unlabel
          ("\\`nnmaildir\\+gmail:" (expire-age . 'never) (display . all))
          ;; the archive holds everything ever received; show the newest
          ;; slice instead of prompting for a count
          ("\\`nnmaildir\\+gmail:archive\\'" (display . 200))
          ;; a year of a list; a followup posts to the server it came from
          ("\\`nnmaildir\\+news:" (display . 500) (gnus-use-scoring t)
           (post-method nntp ,news-server))
          ;; a thread's post can sit below the 500 its comments are in, and
          ;; heads the thread once its header is fetched too; `some' would
          ;; have nnmaildir hand over every header of the group each time
          ("\\`nnmaildir\\+reddit:" (display . 500) (gnus-fetch-old-headers 2000)))
        ;; the function already puts the newest thread first; (not ...)
        ;; would sort oldest first
        gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
        gnus-summary-thread-gathering-function #'gnus-gather-threads-by-references
        ;; %uD is the queued delete or archive and %uS the star, both
        ;; drawn by marks.el; %uL holds the labels, drawn by labels.el
        gnus-summary-line-format "%uD%U%R%uS %-16,16&user-date; %-24,24f %B%uL%s\n"
        gnus-sum-thread-tree-root ""
        gnus-sum-thread-tree-false-root ""
        gnus-sum-thread-tree-single-indent ""
        gnus-sum-thread-tree-indent "  "
        gnus-sum-thread-tree-vertical "│ "
        gnus-sum-thread-tree-leaf-with-other "├─▶ "
        gnus-sum-thread-tree-single-leaf "└─▶ ")
  :config
  (add-hook 'gnus-group-mode-hook #'gnus-topic-mode)
  ;; a label dropped while Gnus was down leaves its group without a folder
  (add-hook 'gnus-started-hook #'drop-vanished-mail-groups -10)
  (add-hook 'gnus-started-hook #'subscribe-mail-groups)
  ;; Gnus would rename or delete a label's folder alone, and mbsync's
  ;; sync state would point at a label Gmail lacks
  (advice-add 'gnus-group-rename-group :around #'rename-mail-label-a)
  (advice-add 'gnus-group-delete-group :around #'delete-mail-label-a)
  ;; topics are newsrc state, so config applies over it at every start,
  ;; once the subscriptions exist
  (add-hook 'gnus-started-hook #'apply-mail-topics 50)
  ;; the start reads no maildir (`defer-mail-server-scan-a'); a batch
  ;; Emacs reads every group, and the session builds each from its table
  ;; a slice at a time
  (add-hook 'gnus-started-hook #'load-all-mail-groups 90)
  (add-hook 'gnus-exit-gnus-hook #'stop-mail-load)
  ;; what launchd's syncs change reaches the routine groups, as after , u
  (add-hook 'gnus-started-hook #'start-mail-sync-check 90)
  (add-hook 'gnus-exit-gnus-hook #'stop-mail-sync-check)
  ;; queued mail leaves on Gnus's demon, and what is left before Gnus quits
  (add-hook 'gnus-started-hook #'start-mail-queue 90)
  (add-hook 'gnus-exit-gnus-hook #'stop-mail-queue)
  (advice-add 'gnus-draft-send :around #'send-queued-article-a)
  ;; entering a group or searching needs the groups nnmaildir holds
  (advice-add 'gnus-group-read-ephemeral-search-group :around #'wait-for-mail-loads-a)
  ;; a feed is read from its downloaded copy, so a group never waits on
  ;; the network; the download itself runs in the background
  (add-hook 'gnus-started-hook #'start-feed-fetches 95)
  ;; the news and Reddit stores are filled the same way, by a batch Emacs
  (add-hook 'gnus-started-hook #'fetch-stale-news 95)
  (add-hook 'gnus-started-hook #'fetch-stale-reddit 95)
  ;; a fetch and a batch read of one folder would number its messages
  ;; twice, so each waits for the other
  (add-hook 'mail-load-parked-functions #'news-group-parked-p)
  (add-hook 'mail-load-parked-functions #'reddit-group-parked-p)
  (add-hook 'mail-load-finished-hook #'fetch-pending-news)
  (add-hook 'mail-load-finished-hook #'fetch-pending-reddit)
  (add-hook 'mail-sync-hook #'fetch-news)
  (add-hook 'mail-sync-hook #'fetch-feeds)
  (add-hook 'mail-sync-hook #'fetch-reddit)
  ;; Gnus gives up on a server whose directory is missing
  (add-hook 'gnus-before-startup-hook #'ensure-news-folders)
  (add-hook 'gnus-before-startup-hook #'ensure-reddit-folders)
  (add-hook 'gnus-subscribe-newsgroup-functions #'defer-news-group-h)

  (defun bind-mail-keys (mode &rest _)
    "Bind this module's keys on the Gnus maps when MODE is gnus.
evil-collection evilifies gnus from an after-load hook of its own, and
`elpaca-after-init' registers that hook after this one, so its RET (which
only scrolls the article), its `gR' and its r and R win unless the keys
are applied again from `evil-collection-setup-hook'."
    (when (eq mode 'gnus)
      ;; a key reads the same wherever a message is read; the thread
      ;; view's map exists once its file loads, and general waits for it
      (map! :map (gnus-summary-mode-map gnus-article-mode-map mail-thread-mode-map)
            ;; both quote the message; the capital answers everyone
            :n "r" #'reply-to-sender
            :n "R" #'reply-to-everyone
            (:localleader
             :desc "sync"             "u" #'sync-mail
             :desc "search all mail"  "/" #'search-mail-live
             :desc "search like this" "?" #'search-mail-like-this
             :desc "new message"      "c" #'compose-new-mail
             :desc "forward"          "f" #'forward-mail
             :desc "labels"           "l" #'label-mail
             :desc "thread labels"    "L" #'label-mail-threads
             :desc "quit mail"        "q" #'quit-mail
             (:prefix ("r" . "reply")
              :desc "to the list only" "l" #'reply-to-list
              :desc "on the newsgroup" "n" #'follow-up-on-newsgroup)
             (:prefix ("o" . "open")
              :desc "in Gmail"        "g" #'open-message-in-gmail
              :desc "in list archive" "l" #'open-message-in-list-archive)))
      (map! :map gnus-summary-mode-map
            :n "RET" #'open-mail-thread
            :n "<return>" #'open-mail-thread
            ;; dired's d, u and x; the capitals take the thread at point.
            ;; u, U and x displace evil-collection's process-mark and
            ;; limit-to-unread keys.  Every mark key takes the visual
            ;; selection, and visual state needs the keys of its own:
            ;; evil's visual and motion maps bind a, A, u, U and ! there
            :nv "d" #'mail-mark-for-deletion
            :nv "D" #'mail-mark-thread-for-deletion
            :nv "a" #'mail-mark-for-archive
            :nv "A" #'mail-mark-thread-for-archive
            :nv "u" #'mail-unmark
            :nv "U" #'mail-unmark-thread
            :nv "!" #'mail-toggle-read
            :nv "=" #'mail-toggle-star
            :n "x" #'mail-execute-marks
            :n "M" nil
            :n "%" nil
            ;; leaving would drop the queue unasked
            :n "q" #'quit-mail-summary
            :n "ZZ" #'quit-mail-summary
            :n "J" #'gnus-summary-scroll-up
            :n "K" #'gnus-summary-scroll-down
            ;; vim's folds, over threads
            :n "TAB" #'toggle-mail-thread-fold
            :n "<tab>" #'toggle-mail-thread-fold
            :n "za" #'toggle-mail-thread-fold
            :n "zM" #'gnus-summary-hide-all-threads
            :n "zR" #'gnus-summary-show-all-threads
            (:localleader
             ;; Gmail's Move to: a label is a group
             :desc "move to label" "m" #'gnus-summary-move-article
             :desc "narrow"        "n" #'gnus-summary-limit-map
             (:prefix ("t" . "thread")
              :desc "fetch from every group" "f" #'gnus-summary-refer-thread
              :desc "mark read"              "r" #'mail-mark-thread-read)
             (:prefix ("s" . "sort")
              :desc "date"    "d" #'sort-mail-by-date
              :desc "author"  "a" #'sort-mail-by-author
              :desc "subject" "s" #'sort-mail-by-subject)
             :desc "marks"         "*" #'mail-selection-menu))
      ;; gR reads the routine groups again in the background, as a sync
      ;; does; gr stays Gnus's own per-group rescan
      (map! :map gnus-group-mode-map
            :n "gR" #'refresh-mail-groups
            (:localleader
             :desc "sync"            "u" #'sync-mail
             :desc "search all mail" "/" #'search-mail-live
             :desc "new message"     "c" #'compose-new-mail
             :desc "inbox"           "i" #'open-mail-inbox
             :desc "quit mail"       "q" #'quit-mail
             :desc "add group"       "a" #'add-mail-group
             :desc "unsubscribe"     "d" #'gnus-group-unsubscribe
             :desc "move to topic"   "m" #'gnus-topic-move-group
             ;; in Gmail too; r renames a label the same way
             (:prefix ("l" . "label")
              :desc "new"    "n" #'new-mail-label
              :desc "rename" "r" #'rename-mail-label
              :desc "delete" "d" #'delete-mail-label)
             (:prefix ("t" . "topic")
              :desc "new"    "n" #'gnus-topic-create-topic
              :desc "rename" "r" #'gnus-topic-rename
              :desc "delete" "d" #'gnus-topic-delete)))))

  (bind-mail-keys 'gnus)
  (add-hook 'evil-collection-setup-hook #'bind-mail-keys)

  ;; the thread view loads with its first use, so its map exists only
  ;; then; C-j and C-k move by section the way rfc-mode binds them
  (map! :after mail-thread
        :map mail-thread-mode-map
        :n "TAB" #'mail-thread-toggle-message
        :n "<tab>" #'mail-thread-toggle-message
        :n "RET" #'mail-thread-open-article
        :n "<return>" #'mail-thread-open-article
        :n "C-j" #'mail-thread-next-message
        :n "C-k" #'mail-thread-previous-message
        :n "]]" #'mail-thread-next-message
        :n "[[" #'mail-thread-previous-message
        :n "q" #'mail-thread-quit
        ;; the summary's , n narrows as well
        (:localleader :desc "narrow to a query" "n" #'mail-thread-filter)
        ;; the summary's mark keys, on the message at point or its
        ;; thread; the summary beside the view draws the marks
        :n "d" (cmd! (run-in-mail-summary #'mail-mark-for-deletion t))
        :n "D" (cmd! (run-in-mail-summary #'mail-mark-thread-for-deletion t))
        :n "a" (cmd! (run-in-mail-summary #'mail-mark-for-archive t))
        :n "A" (cmd! (run-in-mail-summary #'mail-mark-thread-for-archive t))
        :n "u" (cmd! (run-in-mail-summary #'mail-unmark t))
        :n "U" (cmd! (run-in-mail-summary #'mail-unmark-thread t))
        :n "!" (cmd! (run-in-mail-summary #'mail-toggle-read t))
        :n "=" (cmd! (run-in-mail-summary #'mail-toggle-star t))))

(use-package gnus-win
  :ensure nil
  :defer t
  :config
  ;; the display is ultra-wide, so the article and the thread view go
  ;; beside the summary
  (gnus-add-configuration
   '(article (horizontal 1.0 (summary 0.33 point) (article 1.0))))
  (add-to-list 'gnus-window-to-buffer '(mail-thread . mail-thread-buffer-name))
  (gnus-add-configuration
   '(mail-thread (horizontal 1.0 (summary 0.33) (mail-thread 1.0 point))))

  (defadvice! nest-gnus-windows-a (fn &rest args)
    "Call FN with ARGS, splitting Gnus windows under a parent of their own.
A re-layout deletes the summary's window, and without the parent its
columns would go to the window left of Gnus."
    :around #'gnus-configure-windows
    (let ((window-combination-limit t))
      (apply fn args))))

(use-package gnus-sum
  :ensure nil
  :defer t
  :config
  ;; the mark letters show as symbols (`mail-mark-symbols')
  (add-hook 'gnus-summary-mode-hook #'draw-mail-mark-symbols-h)
  (add-hook 'gnus-summary-mode-hook #'draw-mail-labels-h)
  ;; ahead of Gnus's own function, which marks a starred unread message
  ;; read as it is displayed and drops the star with it
  (add-hook 'gnus-mark-article-hook #'mail-keep-star-on-read-h)
  ;; a summary's exit writes its whole view of read and star; what it
  ;; held on entry tells its own changes from those made elsewhere
  (add-hook 'gnus-select-group-hook #'note-entry-marks-h)
  ;; a summary open under a search learns what the search changed, or
  ;; its own exit would write the older state back
  (add-hook 'gnus-summary-prepare-exit-hook #'carry-search-marks-h)
  ;; mail a search found may lie above its group's active range, where
  ;; nnselect saves no read mark
  (add-hook 'gnus-summary-prepare-exit-hook #'activate-search-hit-groups-h)
  ;; a summary's exit would unmark the mail that arrived after it opened
  (advice-add 'gnus-update-read-articles :around #'keep-newer-read-marks-a)
  ;; and it would undo what a refresh brought from the phone meanwhile;
  ;; a label saves before the prepare-exit hook runs, a search after it
  (add-hook 'gnus-exit-group-hook #'keep-group-changes-h)
  (add-hook 'gnus-summary-prepare-exit-hook #'keep-search-group-changes-h)
  ;; a group still loading opens once its table is applied, and a thread
  ;; search waits for every group, like any search
  (advice-add 'gnus-summary-read-group :around #'wait-for-mail-group-a)
  (advice-add 'gnus-summary-refer-thread :around #'wait-for-mail-loads-a)

  ;; a summary rebuild or a menu over the window leaves point on the
  ;; bottom row; label changes can finish in a timer, after the command
  (defadvice! recenter-mail-summary-a (&rest _)
    "Recenter the summary window after an action on its messages."
    :after '(mail-act-on-selection mail-execute-marks
             mail-label-apply mail-label-finish)
    (with-current-buffer (window-buffer)
      (when (derived-mode-p 'gnus-summary-mode)
        (recenter)))))

(use-package gnus-art
  :ensure nil
  :defer t
  :init
  ;; one painter for quoted lines: gnus-cite's overlays would cover the
  ;; depth faces, and its reply-buffer mode puts gnus-cite faces in front
  ;; of the message-cited-text ones message-mode paints
  (setq gnus-treat-highlight-citation nil
        gnus-message-highlight-citation nil
        ;; HTML paragraphs come unfilled and wrap at the window edge.
        ;; Gnus applies this after the mode hooks on every article
        gnus-article-truncate-lines nil)
  (add-hook 'gnus-article-mode-hook #'visual-line-mode)
  (add-hook 'gnus-article-mode-hook #'visual-wrap-prefix-mode)
  :config
  (add-to-list 'gnus-treatment-function-alist
               '(mail-treat-quotes highlight-mail-quotes) t))

(use-package nnmaildir
  :ensure nil
  :defer t
  :config
  ;; a scan of the whole server reads every label in the main thread, so
  ;; each group is read on its own when something needs it
  (advice-add 'nnmaildir-request-scan :around #'defer-mail-server-scan-a)
  ;; until a group's table is applied, nnmaildir refuses it
  (advice-add 'nnmaildir-request-group :around #'refuse-unloaded-mail-group-a)
  (advice-add 'nnmaildir-request-accept-article :around #'refuse-unloaded-mail-group-a)
  (advice-add 'nnmaildir-base-name-to-article-number :around #'scan-mail-group-on-miss-a)
  ;; a flag Gnus saves would drop the flags the phone changed on that file
  (advice-add 'nnmaildir--article-set-flags :around #'keep-flags-set-elsewhere-a)
  ;; nnmaildir drops a message mbsync renamed at its next read or change
  (dolist (fn '(nnmaildir-retrieve-headers nnmaildir-request-article
                nnmaildir-request-move-article nnmaildir-request-expire-articles))
    (advice-add fn :before #'follow-renamed-mail-a))
  (dolist (fn '(nnmaildir-request-set-mark nnmaildir-request-update-info))
    (advice-add fn :before #'follow-renamed-mail-marks-a))
  ;; a copy or move is an upload, and Gmail refuses a NUL byte in one
  (advice-add 'nnmaildir-request-accept-article :around #'accept-mail-without-nul-a)
  ;; a label added, removed or archived away shows on every summary
  (advice-add 'nnmaildir-request-accept-article :after #'refresh-mail-labels-soon)
  (advice-add 'nnmaildir-request-expire-articles :after #'refresh-mail-labels-soon))

(use-package gnus-search
  :ensure nil
  :defer t
  :init
  (setq gnus-search-default-engines '((nnmaildir . gnus-search-notmuch)
                                      (nnimap . gnus-search-imap))
        gnus-search-notmuch-config-file
        (expand-file-name "notmuch/default/config"
                          (or (getenv "XDG_CONFIG_HOME") "~/.config"))
        gnus-search-notmuch-remove-prefix gmail-maildir)
  :config
  ;; a hit is the copy in the inbox when there is one, so marks and
  ;; moves in search results reach what the inbox shows
  (advice-add 'gnus-search-indexed-parse-output :around #'search-likeliest-copies-a))

;; a line of the live search, in its minibuffer or a collect buffer, acts
;; as its thread, and an export reads the threads whole in one summary
(after! embark
  (defvar-keymap mail-thread-embark-map
    :doc "Embark actions on a thread the live search lists."
    :parent embark-general-map
    "RET" #'open-mail-live-candidate
    "s" #'open-mail-live-summary
    "g" #'open-mail-live-in-gmail)
  (setf (alist-get 'mail-thread embark-keymap-alist) 'mail-thread-embark-map
        (alist-get 'mail-thread embark-default-action-overrides) #'open-mail-live-candidate
        (alist-get 'mail-thread embark-exporters-alist) #'export-mail-live-search)
  ;; s on several lines reads them in one summary
  (add-to-list 'embark-multitarget-actions #'open-mail-live-summary)
  (add-hook 'embark-after-export-hook #'open-mail-live-export-h)
  ;; checked here, so collecting anything else loads no mail code
  (add-hook! 'embark-collect-mode-hook
    (defun prepare-mail-collect-h ()
      "Turn on `mail-live-collect-mode' where Embark collects the live search's lines."
      (when (eq embark--type 'mail-thread)
        (mail-live-collect-mode))))
  ;; the mode turns on in a buffer already in normal state, and evil reads
  ;; a minor mode's state keys only when it recomputes its maps
  (add-hook 'mail-live-collect-mode-hook #'evil-normalize-keymaps)
  ;; the mail localleader; a key on a message reads the line's thread
  ;; first, as RET does
  (map! :map mail-live-collect-mode-map
        (:localleader
         :desc "sync"             "u" #'sync-mail
         :desc "search all mail"  "/" #'search-mail-live
         :desc "search like this" "?" (cmd! (run-on-mail-live-line #'search-mail-like-this))
         :desc "new message"      "c" #'compose-new-mail
         :desc "forward"          "f" (cmd! (run-on-mail-live-line #'forward-mail))
         :desc "labels"           "l" (cmd! (run-on-mail-live-line #'label-mail))
         :desc "thread labels"    "L" (cmd! (run-on-mail-live-line #'label-mail-threads))
         :desc "quit mail"        "q" #'quit-mail
         (:prefix ("r" . "reply")
          :desc "to the list only" "l" (cmd! (run-on-mail-live-line #'reply-to-list))
          :desc "on the newsgroup" "n" (cmd! (run-on-mail-live-line #'follow-up-on-newsgroup)))
         (:prefix ("o" . "open")
          ;; no thread view first: reading it would mark the message read
          :desc "in Gmail"        "g" (cmd! (open-mail-live-in-gmail (mail-live-line-at-point)))
          :desc "in list archive" "l" (cmd! (run-on-mail-live-line
                                               #'open-message-in-list-archive))))))

(use-package message
  :ensure nil
  :defer t
  :init
  (setq message-send-mail-function #'smtpmail-send-it
        message-kill-buffer-on-exit t
        message-confirm-send t
        message-fill-column nil
        ;; not message-alternative-emails: a match there overrides the
        ;; posting style and would answer as whichever address received
        ;; the mail
        message-dont-reply-to-names (regexp-opt (list mail-from-address mail-inbox-address))
        message-citation-line-function #'message-insert-formatted-citation-line
        message-citation-line-format "On %a, %b %d, %Y at %R, %N wrote:\n"
        message-signature "Best regards,\nAg")
  :config
  ;; ahead of message.el's own entry, which covers From as well
  (add-to-list 'message-completion-alist
               '("^\\([^ :]*-\\)?\\(To\\|B?Cc\\|Reply-To\\|Mail-Followup-To\\|Mail-Copies-To\\):"
                 . complete-mail-address))
  (add-hook 'message-mode-hook #'prepare-mail-addresses-h))

(use-package gnus-msg
  :ensure nil
  :defer t
  :config
  ;; the body is Org; what Gnus composes leaves as HTML or plain text
  (add-hook 'gnus-message-setup-hook #'prepare-mail-format-h)
  (advice-add 'message-send :around #'send-converted-mail-a)
  (map! :map message-mode-map
        ;; C-c C-c and evil-collection's ZZ queue the message instead
        [remap message-send-and-exit] #'send-later
        (:localleader
         :desc "plain text or HTML" "p" #'toggle-mail-format
         :desc "edit body in Org"   "o" #'org-mime-edit-mail-in-org-mode)))

(use-package gnus-delay
  :ensure nil
  :defer t
  :init
  ;; C-c C-c holds a message this long; C-c C-j still asks for any delay
  (setq gnus-delay-default-delay "1m"
        ;; the demon checks the queue every 2 steps, after 1 idle step
        gnus-demon-timestep 5))

(use-package gnus-draft
  :ensure nil
  :defer t
  :config
  ;; evil-collection's e edits the raw article in every summary; in a
  ;; draft group, the delayed one included, e opens the message itself
  (map! :map gnus-draft-mode-map :n "e" #'gnus-draft-edit-message)
  ;; the mode turns on in a summary already in normal state, and evil
  ;; reads a minor mode's state keys only when it recomputes its maps
  (add-hook 'gnus-draft-mode-hook #'evil-normalize-keymaps))

(use-package org-mime
  :defer t
  :commands org-mime-edit-mail-in-org-mode
  :init
  ;; the Org buffer opens as any other buffer would, not over the frame
  (setq org-mime-obey-display-buffer-p t))

(use-package smtpmail
  :ensure nil
  :defer t
  :init
  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls
        smtpmail-smtp-user mail-from-address))

(use-package mm-decode
  :ensure nil
  :defer t
  :init
  (setq mm-text-html-renderer #'render-mail-html))

;;; config.el ends here
