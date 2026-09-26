;;; modules/email/config.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Gnus over a maildir that mbsync mirrors from Gmail: every label is an
;; nnmaildir group, notmuch is only the search index behind gnus-search,
;; and gmane over NNTP serves the mailing lists.  Outgoing mail always
;; leaves as agzam.ibragimov - sending it through the to.plotnick account
;; with that address as an alias stamps "sent on behalf" - and a Gcc copy
;; lands in the synced sent folder so threads in the mirrored store keep
;; their replies.
;;; Code:

(defvar gmail-maildir (expand-file-name "~/.mail/gmail/")
  "Maildir root mbsync keeps in sync with Gmail; each label is a subdir.")

(defvar mail-from-address "agzam.ibragimov@gmail.com"
  "Address every outgoing message is sent from.")

(defvar mail-inbox-address "to.plotnick@gmail.com"
  "Address whose mailbox mbsync mirrors; never a sender.")

(defvar mail-sync-program "mail-sync"
  "Program that syncs the maildir with Gmail and refreshes the notmuch index.")

(defvar mail-inbox-group "nnmaildir+gmail:inbox"
  "Gnus group `open-mail-inbox' enters.")

(defvar mail-trash-group "nnmaildir+gmail:trash"
  "Group a queued deletion is moved into; mbsync mirrors it from Gmail's Trash.")

(defvar mail-archive-group "nnmaildir+gmail:archive"
  "Group mbsync mirrors from Gmail's All Mail, where archived mail lives.")

(defvar mail-groups (list mail-inbox-group "nntp+news.gmane.io:gmane.emacs.devel")
  "Groups Gnus subscribes to on startup, on top of every maildir group.")

(defvar mail-bulk-groups
  '("nnmaildir+gmail:archive" "nnmaildir+gmail:emacs" "nnmaildir+gmail:org-mode"
    "nnmaildir+gmail:new" "nntp+news.gmane.io:gmane.emacs.devel")
  "Groups no sync rescans: tens of thousands of files, or an NNTP round trip.")

(defvar mail-treat-quotes t
  "Treatment condition for `highlight-mail-quotes', like the gnus-treat ones.")

(use-package gnus
  :ensure nil
  :defer t
  :init
  (setq gnus-select-method '(nnnil "")
        gnus-secondary-select-methods
        `((nnmaildir "gmail" (directory ,gmail-maildir) (get-new-mail nil))
          (nntp "news.gmane.io"))
        ;; gmane carries tens of thousands of groups; scanning for new
        ;; ones or saving the killed list makes every startup crawl
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
        gnus-suppress-duplicates t
        gnus-refer-thread-use-search t
        ;; entering a group must not open (and mark read) its first article
        gnus-auto-select-first nil
        ;; the Gcc copy is marked read so mbsync uploads it as seen
        gnus-message-archive-group "nnmaildir+gmail:sent"
        gnus-gcc-mark-as-read t
        gnus-posting-styles '((".*" (name user-full-name) (address mail-from-address)))
        ;; the last matching entry wins for each parameter.  A real
        ;; parameter is a dotted pair - `gnus-group-find-parameter'
        ;; drops an entry whose cdr is a list.  A two-element entry
        ;; instead sets that variable buffer-locally and evaluates the
        ;; value, hence the quote.  nnmaildir evaluates its own
        ;; parameters too, so expire-age needs one as well.
        gnus-parameters
        '(;; nnmaildir deletes expired files, and mbsync would push that
          ;; to Gmail as an archive or an unlabel
          ("\\`nnmaildir\\+gmail:" (expire-age . 'never) (display . all)
           (gnus-fetch-old-headers 'some))
          ;; the archive holds everything ever received; show the newest
          ;; slice instead of prompting for a count
          ("\\`nnmaildir\\+gmail:archive\\'" (display . 200))
          ("\\`nntp\\+news\\.gmane\\.io:" (gnus-use-scoring t)))
        ;; the function already puts the newest thread first; (not ...)
        ;; would sort oldest first
        gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
        gnus-summary-thread-gathering-function #'gnus-gather-threads-by-references
        ;; %uD is the queued delete or archive and %uS the star, both
        ;; drawn by marks.el
        gnus-summary-line-format "%uD%U%R%uS %-16,16&user-date; %-24,24f %B%s\n"
        gnus-sum-thread-tree-root ""
        gnus-sum-thread-tree-false-root ""
        gnus-sum-thread-tree-single-indent ""
        gnus-sum-thread-tree-indent "  "
        gnus-sum-thread-tree-vertical "│ "
        gnus-sum-thread-tree-leaf-with-other "├─▶ "
        gnus-sum-thread-tree-single-leaf "└─▶ ")
  :config
  (add-hook 'gnus-group-mode-hook #'gnus-topic-mode)
  (add-hook 'gnus-started-hook #'subscribe-mail-groups)
  ;; the start reads no maildir (`defer-mail-server-scan-a'); the
  ;; routine groups follow on timer turns, a label subscribed just now
  ;; among them
  (add-hook 'gnus-started-hook #'queue-mail-refresh 90)

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
             :desc "sync"            "u" #'sync-mail
             :desc "search all mail" "/" #'search-mail
             :desc "new message"     "c" #'compose-new-mail
             :desc "forward"         "f" #'forward-mail
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
            :n "J" #'gnus-summary-scroll-up
            :n "K" #'gnus-summary-scroll-down
            ;; vim's folds, over threads
            :n "TAB" #'toggle-mail-thread-fold
            :n "<tab>" #'toggle-mail-thread-fold
            :n "za" #'toggle-mail-thread-fold
            :n "zM" #'gnus-summary-hide-all-threads
            :n "zR" #'gnus-summary-show-all-threads
            (:localleader
             ;; Gmail's Move to and Label as: a label is a group
             :desc "move to label" "m" #'gnus-summary-move-article
             :desc "add label"     "l" #'gnus-summary-copy-article
             :desc "narrow"        "n" #'gnus-summary-limit-map
             (:prefix ("t" . "thread")
              :desc "fetch from every group" "f" #'gnus-summary-refer-thread
              :desc "mark read"              "r" #'mail-mark-thread-read)
             (:prefix ("s" . "sort")
              :desc "date"    "d" #'sort-mail-by-date
              :desc "author"  "a" #'sort-mail-by-author
              :desc "subject" "s" #'sort-mail-by-subject)))
      ;; gR reads the routine groups before it returns, where
      ;; evil-collection's gnus-group-get-new-news leaves them to timer
      ;; turns; gr stays Gnus's own per-group rescan
      (map! :map gnus-group-mode-map
            :n "gR" #'refresh-mail-groups
            (:localleader
             :desc "sync"            "u" #'sync-mail
             :desc "search all mail" "/" #'search-mail
             :desc "new message"     "c" #'compose-new-mail
             :desc "inbox"           "i" #'open-mail-inbox))))

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
  ;; ahead of Gnus's own function, which marks a starred unread message
  ;; read as it is displayed and drops the star with it
  (add-hook 'gnus-mark-article-hook #'mail-keep-star-on-read-h))

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
  (advice-add 'nnmaildir-request-group :around #'scan-unknown-mail-group-a)
  (advice-add 'nnmaildir-request-accept-article :around #'scan-unknown-mail-group-a)
  (advice-add 'nnmaildir-base-name-to-article-number :around #'scan-mail-group-on-miss-a))

(use-package gnus-search
  :ensure nil
  :defer t
  :init
  (setq gnus-search-default-engines '((nnmaildir . gnus-search-notmuch)
                                      (nnimap . gnus-search-imap))
        gnus-search-notmuch-config-file
        (expand-file-name "notmuch/default/config"
                          (or (getenv "XDG_CONFIG_HOME") "~/.config"))
        gnus-search-notmuch-remove-prefix gmail-maildir))

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
        message-citation-line-format "On %a, %b %d, %Y at %R, %N wrote:\n"))

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
