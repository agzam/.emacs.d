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
        gnus-use-full-window nil
        gnus-suppress-duplicates t
        gnus-refer-thread-use-search t
        ;; entering a group must not open (and mark read) its first article
        gnus-auto-select-first nil
        ;; the Gcc copy is marked read so mbsync uploads it as seen
        gnus-message-archive-group "nnmaildir+gmail:sent"
        gnus-gcc-mark-as-read t
        gnus-posting-styles '((".*" (name user-full-name) (address mail-from-address)))
        ;; the last matching entry wins for each parameter
        gnus-parameters
        '(;; nnmaildir deletes expired files, and mbsync would push that
          ;; to Gmail as an archive or an unlabel
          ("\\`nnmaildir\\+gmail:" (expire-age never) (display all)
           (gnus-fetch-old-headers some))
          ;; the archive holds everything ever received; show the newest
          ;; slice instead of prompting for a count
          ("\\`nnmaildir\\+gmail:archive\\'" (display 200))
          ("\\`nntp\\+news\\.gmane\\.io:" (gnus-use-scoring t)))
        gnus-thread-sort-functions '((not gnus-thread-sort-by-most-recent-date))
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

  (map! :map gnus-group-mode-map
        (:localleader
         :desc "sync"   "u" #'sync-mail
         :desc "search" "s" #'search-mail
         :desc "inbox"  "i" #'open-mail-inbox))

  (map! :map gnus-summary-mode-map
        (:localleader
         :desc "sync"          "u" #'sync-mail
         :desc "search"        "s" #'search-mail
         :desc "open in Gmail" "g" #'open-message-in-gmail
         :desc "list archive"  "l" #'open-message-in-list-archive)))

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
