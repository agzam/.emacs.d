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

(describe "email module group parameters"
  (it "never expires nnmaildir articles (mbsync would push the deletion)"
    (let ((general (assoc "\\`nnmaildir\\+gmail:" gnus-parameters)))
      (expect (assq 'expire-age general) :to-equal '(expire-age never))
      (expect (assq 'display general) :to-equal '(display all))))
  (it "shows the archive as a newest slice, overriding the general entry"
    (let* ((names (mapcar #'car gnus-parameters))
           (general (cl-position "\\`nnmaildir\\+gmail:" names :test #'equal))
           (archive (cl-position "\\`nnmaildir\\+gmail:archive\\'" names :test #'equal)))
      ;; gnus-group-fast-parameter keeps the last matching entry
      (expect (< general archive) :to-be t)
      (expect (assq 'display (assoc "\\`nnmaildir\\+gmail:archive\\'" gnus-parameters))
              :to-equal '(display 200)))))

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
