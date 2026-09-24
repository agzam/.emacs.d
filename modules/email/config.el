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

(defvar mail-groups (list mail-inbox-group "nntp+news.gmane.io:gmane.emacs.devel")
  "Groups Gnus subscribes to on startup, on top of every maildir group.")

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
        gnus-summary-line-format "%U%R %-16,16&user-date; %-24,24f %B%s\n"
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

  (map! :map gnus-group-mode-map
        (:localleader
         :desc "sync"   "u" #'sync-mail
         :desc "search" "s" #'search-mail
         :desc "inbox"  "i" #'open-mail-inbox))

  (defun bind-mail-summary-keys (mode &rest _)
    "Bind the summary keys of this module when MODE is gnus.
evil-collection evilifies gnus from an after-load hook of its own, and
`elpaca-after-init' registers that hook after this one, so its RET (which
only scrolls the article) wins unless the keys are applied again from
`evil-collection-setup-hook'."
    (when (eq mode 'gnus)
      (map! :map gnus-summary-mode-map
            :n "RET" #'open-mail-thread
            :n "<return>" #'open-mail-thread
            (:localleader
             :desc "sync"          "u" #'sync-mail
             :desc "search"        "s" #'search-mail
             :desc "open in Gmail" "g" #'open-message-in-gmail
             :desc "list archive"  "l" #'open-message-in-list-archive))))

  (bind-mail-summary-keys 'gnus)
  (add-hook 'evil-collection-setup-hook #'bind-mail-summary-keys)

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
        :n "q" #'mail-thread-quit))

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
  (setq mm-text-html-renderer 'shr))

;;; config.el ends here
