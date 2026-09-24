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

(describe "email module thread order"
  (before-all (require 'gnus-sum))
  (it "puts the newest thread at the top"
    ;; gnus-thread-sort-by-most-recent-date already sorts newest first,
    ;; so a (not ...) wrapper would silently invert the summary
    (let ((old (list (make-full-mail-header
                      1 "old" "a@x" "Mon, 01 Jan 2024 00:00:00 +0000" "<1@x>" "" 0 0 nil)))
          (new (list (make-full-mail-header
                      2 "new" "b@x" "Thu, 01 Jan 2026 00:00:00 +0000" "<2@x>" "" 0 0 nil))))
      (expect (mapcar (lambda (thread) (mail-header-subject (car thread)))
                      (gnus-sort-threads (list old new)))
              :to-equal '("new" "old")))))

(defun email-tests--config-forms (package)
  "The :config forms of the `use-package' block for PACKAGE in the module config.
The suite loads the config with :config skipped, so these are read back
from the source."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "modules/email/config.el" test-config-root))
    (goto-char (point-min))
    (let (form)
      (while (and (setq form (ignore-errors (read (current-buffer))))
                  (not (and (eq (car-safe form) 'use-package)
                            (eq (cadr form) package)))))
      (expect form :to-be-truthy)
      (use-package-body-forms (cddr form) :config))))

(describe "email module bindings"
  :var* ((config (email-tests--config-forms 'gnus)))

  (it "opens the whole thread from the summary instead of one article"
    (let ((pairs (mapcan #'map-form-key-pairs
                         (map-form-groups config 'gnus-summary-mode-map))))
      (expect (cdr (assoc "RET" pairs)) :to-equal '(function open-mail-thread))
      (expect (cdr (assoc "<return>" pairs)) :to-equal '(function open-mail-thread))))

  (it "queues, unqueues and executes deletions and archives with Dired's keys"
    (let ((pairs (mapcan #'map-form-key-pairs
                         (map-form-groups config 'gnus-summary-mode-map))))
      (expect (mapcar (lambda (key) (cadr (cdr (assoc key pairs))))
                      '("d" "D" "a" "A" "u" "U" "x"))
              :to-equal '(mail-mark-for-deletion mail-mark-thread-for-deletion
                          mail-mark-for-archive mail-mark-thread-for-archive
                          mail-unmark mail-unmark-thread mail-execute-marks))))

  (it "folds, moves and leaves inside the thread buffer"
    (let ((pairs (mapcan #'map-form-key-pairs
                         (map-form-groups config 'mail-thread-mode-map))))
      (expect pairs :to-have-same-items-as
              '(("TAB" function mail-thread-toggle-message)
                ("<tab>" function mail-thread-toggle-message)
                ("RET" function mail-thread-open-article)
                ("<return>" function mail-thread-open-article)
                ("C-j" function mail-thread-next-message)
                ("C-k" function mail-thread-previous-message)
                ("]]" function mail-thread-next-message)
                ("[[" function mail-thread-previous-message)
                ("q" function mail-thread-quit)))))

  (it "refreshes the routine groups from the group buffer's gR"
    ;; gR is gnus-group-get-new-news, which asks nnmaildir for a
    ;; server-wide scan; gr stays Gnus's own per-group rescan
    (let ((pairs (mapcan #'map-form-key-pairs
                         (map-form-groups config 'gnus-group-mode-map))))
      (expect (cdr (assoc "gR" pairs)) :to-equal '(function refresh-mail-groups))
      (expect (assoc "gr" pairs) :to-be nil)))

  (it "takes the keys evil-collection owns back after it evilifies gnus"
    ;; evil-collection binds RET and gR from an after-load hook
    ;; registered later than this module's, so applying the keys once
    ;; loses them on a fresh boot
    (expect (seq-find (lambda (form)
                        (and (eq (car-safe form) 'add-hook)
                             (equal (cadr form) ''evil-collection-setup-hook)
                             (equal (nth 2 form) '#'bind-mail-keys)))
                      config)
            :to-be-truthy))

  (it "waits for the thread view to load before binding its map"
    ;; the file loads with the first `open-mail-thread' call, so the map
    ;; does not exist when gnus loads
    (expect (seq-find (lambda (form)
                        (and (eq (car-safe form) 'map!)
                             (eq (cadr (memq :after form)) 'mail-thread)
                             (memq 'mail-thread-mode-map form)))
                      config)
            :to-be-truthy)))

(describe "email module quoted lines"
  (it "leaves them to one painter in articles and replies"
    ;; gnus-cite's overlays would cover the depth faces, and its reply
    ;; mode puts gnus-cite faces in front of message-mode's
    (require 'gnus-art)
    (require 'gnus-msg)
    (expect gnus-treat-highlight-citation :to-be nil)
    (expect gnus-message-highlight-citation :to-be nil))
  (it "paints them by depth as an article treatment"
    (require 'gnus-art)
    (let ((gnus-treatment-function-alist (copy-sequence gnus-treatment-function-alist)))
      (dolist (form (email-tests--config-forms 'gnus-art))
        (eval form t))
      (expect (assq 'mail-treat-quotes gnus-treatment-function-alist)
              :to-equal '(mail-treat-quotes highlight-mail-quotes))
      (expect mail-treat-quotes :to-be t)))
  (it "loads the painter on the first article, before any command of its file"
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "modules/email/autoload/quotes.el" test-config-root))
      (expect (buffer-string)
              :to-match "^;;;###autoload\n(defun highlight-mail-quotes "))))

(describe "email module deferred deletion"
  (it "draws the queued verb in the first summary column"
    ;; the column is a user format function, so marks.el has to be
    ;; loaded before the first summary line is drawn
    (expect gnus-summary-line-format :to-match "\\`%uD")
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "modules/email/autoload/marks.el" test-config-root))
      (expect (buffer-string)
              :to-match "^;;;###autoload\n(defun gnus-user-format-function-D ")))
  (it "moves a queued deletion into the mirrored trash"
    (expect mail-trash-group :to-equal "nnmaildir+gmail:trash")))

(describe "email module subscriptions"
  (it "subscribes the inbox and emacs-devel on startup"
    (expect mail-groups :to-equal
            '("nnmaildir+gmail:inbox" "nntp+news.gmane.io:gmane.emacs.devel"))
    (expect (member mail-inbox-group mail-groups) :to-be-truthy)))

(describe "email module scan scope"
  (it "scans no further than a fresh subscription's level"
    ;; a scan reads every message it has no overview for, so the level
    ;; decides what a sync touches; 3 is gnus-level-default-subscribed
    (expect gnus-activate-level :to-equal 3)
    (require 'gnus)
    (expect gnus-activate-level :to-equal gnus-level-default-subscribed))
  (it "keeps the inbox in the routine scan and the big labels out of it"
    (expect (member mail-inbox-group mail-bulk-groups) :to-be nil)
    (expect mail-bulk-groups :to-have-same-items-as
            '("nnmaildir+gmail:archive" "nnmaildir+gmail:emacs"
              "nnmaildir+gmail:org-mode" "nnmaildir+gmail:new"
              "nntp+news.gmane.io:gmane.emacs.devel")))
  (it "leaves every bulk group inside the group buffer's list level"
    (require 'gnus)
    (expect (<= (1+ gnus-activate-level) gnus-level-subscribed) :to-be t)))

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
