;;; tests/email/config-tests.el --- email module config specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defun email-tests--load ()
  "Load the module config with `use-package' reduced to its :init forms.
Emacs ships use-package, so the real macro is always there; left alone it
would defer the :config block (map! calls) to the first `require' of gnus
in this process, which the mail suite performs."
  (cl-letf (((symbol-function 'use-package)
             (cons 'macro (lambda (_name &rest args)
                            `(progn ,@(use-package-body-forms args :init))))))
    (load-module-file "modules/email/config.el")))

(email-tests--load)

(describe "email module identity"
  (it "sends every message as agzam.ibragimov"
    (let ((style (cdr (assoc ".*" gnus-posting-styles))))
      (expect (symbol-value (cadr (assq 'address style)))
              :to-equal "agzam.ibragimov@gmail.com")
      (expect (cadr (assq 'name style)) :to-be 'user-full-name)))
  (it "authenticates SMTP as the sender, not the mirrored inbox"
    (expect smtpmail-smtp-user :to-equal "agzam.ibragimov@gmail.com")
    (expect smtpmail-smtp-server :to-equal "smtp.gmail.com")
    (expect smtpmail-stream-type :to-be 'starttls))
  (it "keeps both own addresses out of reply recipients"
    (expect "agzam.ibragimov@gmail.com" :to-match message-dont-reply-to-names)
    (expect "to.plotnick@gmail.com" :to-match message-dont-reply-to-names))
  (it "leaves message-alternative-emails alone (it would override the posting style)"
    (expect (bound-and-true-p message-alternative-emails) :to-be nil))
  (it "files a read copy of outgoing mail into the synced sent folder"
    (expect gnus-message-archive-group :to-equal "nnmaildir+gmail:sent")
    (expect gnus-gcc-mark-as-read :to-be t)))

(describe "email module servers"
  (it "reads the mbsync maildir through nnmaildir without fetching mail itself"
    (let ((server (assq 'nnmaildir gnus-secondary-select-methods)))
      (expect (cadr server) :to-equal "gmail")
      (expect (cadr (assq 'directory (cddr server))) :to-equal gmail-maildir)
      (expect (assq 'get-new-mail (cddr server)) :to-equal '(get-new-mail nil))))
  (it "reads the lists from gmane"
    (expect (assq 'nntp gnus-secondary-select-methods) :to-equal '(nntp "news.gmane.io")))
  (it "never scans gmane for new groups or saves its killed list"
    (expect gnus-check-new-newsgroups :to-be nil)
    (expect gnus-save-killed-list :to-be nil)
    (expect gnus-agent :to-be nil))
  (it "searches nnmaildir groups through notmuch, mapping paths back to groups"
    (expect (alist-get 'nnmaildir gnus-search-default-engines) :to-be 'gnus-search-notmuch)
    (expect gnus-search-notmuch-remove-prefix :to-equal gmail-maildir)
    (expect gnus-refer-thread-use-search :to-be t)))

(defmacro email-tests--with-empty-newsrc (&rest body)
  "Run BODY over an empty newsrc, so lookups fall through to `gnus-parameters'."
  (declare (indent 0))
  `(let ((gnus-newsrc-hashtb (make-hash-table :test #'equal)))
     ,@body))

(describe "email module group parameters"
  ;; assert what Gnus resolves, never the shape of the entry: a
  ;; parameter written as a list instead of a dotted pair reads back as
  ;; nil, and nnmaildir evaluates its own parameter values
  (before-all
    (require 'gnus)
    (require 'nnmaildir))
  (it "never expires nnmaildir articles (mbsync would push the deletion)"
    (email-tests--with-empty-newsrc
      (expect (nnmaildir--param "nnmaildir+gmail:inbox" 'expire-age) :to-be 'never)
      (expect (nnmaildir--param "nnmaildir+gmail:archive" 'expire-age) :to-be 'never)))
  (it "shows read mail in every nnmaildir group"
    (email-tests--with-empty-newsrc
      (expect (gnus-group-find-parameter "nnmaildir+gmail:inbox" 'display) :to-be 'all)))
  (it "threads with old headers in every nnmaildir group"
    (let ((general (assoc "\\`nnmaildir\\+gmail:" gnus-parameters)))
      ;; a two-element entry sets the variable buffer-locally and
      ;; evaluates the value, so the value has to be quoted
      (expect (eval (nth 1 (assq 'gnus-fetch-old-headers general)) t) :to-be 'some)))
  (it "shows the archive as a newest slice, overriding the general entry"
    (email-tests--with-empty-newsrc
      (expect (gnus-group-find-parameter "nnmaildir+gmail:archive" 'display) :to-equal 200)))
  (it "scores gmane groups"
    (let ((gmane (assoc "\\`nntp\\+news\\.gmane\\.io:" gnus-parameters)))
      (expect (eval (nth 1 (assq 'gnus-use-scoring gmane)) t) :to-be t))))

(describe "email module prompts"
  (it "reads a leftover dribble instead of asking about it on startup"
    (expect gnus-always-read-dribble-file :to-be t))
  (it "enters the inbox without asking for an article count"
    (expect (< 1002 gnus-large-newsgroup) :to-be t))
  (it "answers a gmane thread by mail without a confirmation"
    ;; gnus-confirm-mail-reply-to-news derives from this when gnus-msg loads
    (expect gnus-novice-user :to-be nil)
    (require 'gnus-msg)
    (expect gnus-confirm-mail-reply-to-news :to-be nil))
  (it "never opens the first article on group entry"
    (expect gnus-auto-select-first :to-be nil)))

(describe "email module subscriptions"
  (it "subscribes the inbox and emacs-devel on startup"
    (expect mail-groups :to-equal
            '("nnmaildir+gmail:inbox" "nntp+news.gmane.io:gmane.emacs.devel"))
    (expect (member mail-inbox-group mail-groups) :to-be-truthy)))

(describe "email module quarantine"
  (before-all
    (require 'gnus)
    (require 'gnus-start)
    (require 'gnus-agent)
    (require 'gnus-dup)
    (require 'nndraft)
    (require 'nnmail)
    (require 'mail-source)
    (require 'smtpmail))
  (it "keeps every gnus path under the sandbox"
    (dolist (var '(gnus-home-directory gnus-directory gnus-startup-file
                   gnus-init-file gnus-kill-files-directory gnus-cache-directory
                   gnus-agent-directory gnus-article-save-directory
                   gnus-duplicate-file))
      (expect (expand-file-name (symbol-value var)) :to-match
              (concat "\\`" (regexp-quote (expand-file-name test-sandbox-dir))))))
  (it "keeps every message, draft, queue and cache path under the sandbox"
    (dolist (var '(message-directory message-auto-save-directory nndraft-directory
                   nnmail-message-id-cache-file mail-source-directory
                   smtpmail-queue-dir))
      (expect (expand-file-name (symbol-value var)) :to-match
              (concat "\\`" (regexp-quote (expand-file-name test-sandbox-dir)))))))
