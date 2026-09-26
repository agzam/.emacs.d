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

(defun email-tests--key-states (form map)
  "Alist of (KEY . STATES) for the keys FORM binds on MAP.
STATES is the evil state keyword `map!' reads right before the key."
  (let (states)
    (dolist (body (map-form-groups form map))
      (while body
        (let ((item (pop body)))
          (when (and (keywordp item)
                     (stringp (car body))
                     (string-match-p "\\`:[nviemorg]+\\'" (symbol-name item)))
            (push (cons (car body) item) states)))))
    (nreverse states)))

(defun email-tests--localleader-pairs (form map)
  "Alist of (KEYS . DEFINITION) FORM binds under the localleader of MAP.
KEYS holds a prefix's key and the key under it apart by a space, as
`kbd' reads them."
  (let (pairs)
    (cl-labels ((collect (body prefix)
                  (dolist (pair (map-form-key-pairs body))
                    (push (cons (concat prefix (car pair)) (cdr pair)) pairs))
                  (dolist (group body)
                    (when (eq (car-safe group) :prefix)
                      (collect (cddr group) (concat prefix (car (cadr group)) " "))))))
      (dolist (body (map-form-groups form map))
        (dolist (group body)
          (when (eq (car-safe group) :localleader)
            (collect (cdr group) "")))))
    (nreverse pairs)))

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

  (it "toggles read on ! and the star on ="
    (let ((pairs (mapcan #'map-form-key-pairs
                         (map-form-groups config 'gnus-summary-mode-map))))
      (expect (cdr (assoc "!" pairs)) :to-equal '(function mail-toggle-read))
      (expect (cdr (assoc "=" pairs)) :to-equal '(function mail-toggle-star))))

  (it "binds every mark key in visual state too, where a selection is marked"
    ;; evil's visual and motion maps answer a, A, u, U and ! there otherwise
    (let ((states (email-tests--key-states config 'gnus-summary-mode-map)))
      (expect (mapcar (lambda (key) (cdr (assoc key states)))
                      '("d" "D" "a" "A" "u" "U" "!" "=" "x"))
              :to-equal '(:nv :nv :nv :nv :nv :nv :nv :nv :n))))

  (it "scrolls the article forward on J and back on K"
    ;; both show the article at point first; gnus-summary-scroll-down
    ;; alone never moves forward, which is what M-RET gives
    (let ((pairs (mapcan #'map-form-key-pairs
                         (map-form-groups config 'gnus-summary-mode-map))))
      (expect (cdr (assoc "J" pairs)) :to-equal '(function gnus-summary-scroll-up))
      (expect (cdr (assoc "K" pairs)) :to-equal '(function gnus-summary-scroll-down))))

  (it "folds, moves and leaves inside the thread buffer"
    (let ((pairs (mapcan #'map-form-key-pairs
                         (map-form-groups config 'mail-thread-mode-map))))
      (expect (mapcar (lambda (key) (assoc key pairs))
                      '("TAB" "<tab>" "RET" "<return>" "C-j" "C-k" "]]" "[[" "q"))
              :to-equal
              '(("TAB" function mail-thread-toggle-message)
                ("<tab>" function mail-thread-toggle-message)
                ("RET" function mail-thread-open-article)
                ("<return>" function mail-thread-open-article)
                ("C-j" function mail-thread-next-message)
                ("C-k" function mail-thread-previous-message)
                ("]]" function mail-thread-next-message)
                ("[[" function mail-thread-previous-message)
                ("q" function mail-thread-quit)))))

  (it "marks from the thread buffer through the summary, which draws the marks"
    (let ((pairs (mapcan #'map-form-key-pairs
                         (map-form-groups config 'mail-thread-mode-map))))
      (expect (mapcar (lambda (key) (cdr (assoc key pairs)))
                      '("d" "D" "a" "A" "u" "U" "!" "="))
              :to-equal
              (mapcar (lambda (command) `(cmd! (run-in-mail-summary #',command t)))
                      '(mail-mark-for-deletion mail-mark-thread-for-deletion
                        mail-mark-for-archive mail-mark-thread-for-archive
                        mail-unmark mail-unmark-thread
                        mail-toggle-read mail-toggle-star)))
      ;; executing would delete the messages the view shows
      (expect (assoc "x" pairs) :to-be nil)))

  (it "answers and opens the same way wherever a message is read"
    (dolist (map '(gnus-summary-mode-map gnus-article-mode-map mail-thread-mode-map))
      (let ((pairs (mapcan #'map-form-key-pairs (map-form-groups config map)))
            (leader (email-tests--localleader-pairs config map)))
        (expect (list (cdr (assoc "r" pairs)) (cdr (assoc "R" pairs)))
                :to-equal '((function reply-to-sender) (function reply-to-everyone)))
        (expect (mapcar (lambda (keys) (cdr (assoc keys leader)))
                        '("u" "/" "c" "f" "r l" "r n" "o g" "o l"))
                :to-equal '((function sync-mail) (function search-mail)
                            (function compose-new-mail) (function forward-mail)
                            (function reply-to-list) (function follow-up-on-newsgroup)
                            (function open-message-in-gmail)
                            (function open-message-in-list-archive))))))

  (it "takes r and R back from evil-collection in normal state"
    (dolist (map '(gnus-summary-mode-map gnus-article-mode-map))
      (let ((states (email-tests--key-states config map)))
        (expect (list (cdr (assoc "r" states)) (cdr (assoc "R" states)))
                :to-equal '(:n :n)))))

  (it "folds threads with vim's fold keys"
    (let ((pairs (mapcan #'map-form-key-pairs
                         (map-form-groups config 'gnus-summary-mode-map))))
      (expect (mapcar (lambda (key) (cdr (assoc key pairs)))
                      '("TAB" "<tab>" "za" "zM" "zR"))
              :to-equal '((function toggle-mail-thread-fold)
                          (function toggle-mail-thread-fold)
                          (function toggle-mail-thread-fold)
                          (function gnus-summary-hide-all-threads)
                          (function gnus-summary-show-all-threads)))))

  (it "labels, narrows, sorts and handles threads from the summary's localleader"
    (let ((leader (email-tests--localleader-pairs config 'gnus-summary-mode-map)))
      (expect (mapcar (lambda (keys) (cdr (assoc keys leader)))
                      '("m" "l" "n" "t f" "t r" "s d" "s a" "s s"))
              :to-equal '((function gnus-summary-move-article)
                          (function gnus-summary-copy-article)
                          (function gnus-summary-limit-map)
                          (function gnus-summary-refer-thread)
                          (function mail-mark-thread-read)
                          (function sort-mail-by-date)
                          (function sort-mail-by-author)
                          (function sort-mail-by-subject)))
      ;; search moved to / so that s sorts, as in dired and ibuffer
      (expect (assoc "s" leader) :to-be nil)
      (expect (assoc "g" leader) :to-be nil)))

  (it "gives the group buffer the summary's sync, search and new message keys"
    (expect (email-tests--localleader-pairs config 'gnus-group-mode-map)
            :to-equal '(("u" function sync-mail)
                        ("/" function search-mail)
                        ("c" function compose-new-mail)
                        ("i" function open-mail-inbox))))

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

(describe "email module HTML"
  (it "renders HTML parts through the module's renderer"
    (require 'mm-decode)
    (expect mm-text-html-renderer :to-be 'render-mail-html))
  (it "loads the renderer on the first HTML part, before any command of its file"
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "modules/email/autoload/html.el" test-config-root))
      (expect (buffer-string)
              :to-match "^;;;###autoload\n(defun render-mail-html ")))
  (it "wraps article lines at the window edge instead of cutting them"
    ;; Gnus applies gnus-article-truncate-lines after the mode hooks, so
    ;; the hooks alone would lose to it on every article
    (require 'gnus-art)
    (expect gnus-article-truncate-lines :to-be nil)
    (with-temp-buffer
      (gnus-article-mode)
      (expect visual-line-mode :to-be-truthy)
      (expect visual-wrap-prefix-mode :to-be-truthy))))

(defun email-tests--gnus-layout (steps)
  "Call STEPS with the window left of Gnus, Gnus laid out in the right half.
The module's `gnus-win' config applies meanwhile.  The article buffer is
created after the summary, as in a real group, so a re-layout keeps its
window and deletes the summary's."
  (require 'gnus)
  (require 'gnus-win)
  (let ((gnus-buffer-configuration (copy-tree gnus-buffer-configuration))
        (gnus-window-to-buffer (copy-sequence gnus-window-to-buffer))
        (gnus-buffers nil)
        (gnus-summary-buffer "*Summary layout*")
        (gnus-article-buffer "*Article layout*"))
    (unwind-protect
        (progn
          (dolist (form (email-tests--config-forms 'gnus-win))
            (eval form t))
          (delete-other-windows)
          (let ((beside (selected-window)))
            (select-window (split-window beside 40 t))
            (switch-to-buffer (gnus-get-buffer-create gnus-summary-buffer))
            (gnus-get-buffer-create gnus-article-buffer)
            (funcall steps beside)))
      (advice-remove 'gnus-configure-windows 'nest-gnus-windows-a)
      (delete-other-windows)
      (dolist (buffer (gnus-buffers))
        (kill-buffer buffer)))))

(describe "email module layout"
  (it "puts the article to the right of the summary"
    (email-tests--gnus-layout
     (lambda (beside)
       (gnus-configure-windows 'article 'force)
       (let ((summary (get-buffer-window gnus-summary-buffer))
             (article (get-buffer-window gnus-article-buffer)))
         (expect (window-top-line summary) :to-equal (window-top-line article))
         (expect (window-left-column summary) :to-be-less-than (window-left-column article))
         (expect (window-total-width summary) :to-be-less-than (window-total-width article))
         (expect (window-total-width beside) :to-equal 40)))))

  (it "puts the thread view to the right of the summary, where the article goes"
    (load-module-file "modules/email/autoload/thread.el")
    (email-tests--gnus-layout
     (lambda (beside)
       (gnus-get-buffer-create mail-thread-buffer-name)
       (gnus-configure-windows 'mail-thread 'force)
       (let ((summary (get-buffer-window gnus-summary-buffer))
             (thread (get-buffer-window mail-thread-buffer-name)))
         (expect (window-top-line summary) :to-equal (window-top-line thread))
         (expect (window-left-column summary) :to-be-less-than (window-left-column thread))
         (expect (window-total-width summary) :to-be-less-than (window-total-width thread))
         (expect (selected-window) :to-be thread)
         (expect (window-total-width beside) :to-equal 40)))))

  (it "keeps the window beside Gnus at its width through a re-layout and an exit"
    ;; every read-mail-article forces the article layout again, and
    ;; summary exit forces the group one while the article is still shown
    (email-tests--gnus-layout
     (lambda (beside)
       (let (widths)
         (dolist (setting '(article article group))
           (gnus-configure-windows setting 'force)
           (push (window-total-width beside) widths))
         (expect widths :to-equal '(40 40 40))
         (expect (window-total-width (get-buffer-window gnus-group-buffer))
                 :to-equal 40))))))

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
    (expect mail-trash-group :to-equal "nnmaildir+gmail:trash"))
  (it "knows the group of All Mail, where archiving is refused"
    (expect mail-archive-group :to-equal "nnmaildir+gmail:archive")))

(describe "email module autoloads"
  (it "loads each command a key runs before its file has loaded"
    ;; a key pressed before the file loads calls a void function otherwise
    (pcase-dolist (`(,file . ,commands)
                   '(("thread.el" reply-to-sender reply-to-everyone reply-to-list
                      follow-up-on-newsgroup forward-mail compose-new-mail
                      run-in-mail-summary mail-on-screen)
                     ("mail.el" sort-mail-by-date sort-mail-by-author
                      sort-mail-by-subject toggle-mail-thread-fold)
                     ("marks.el" mail-mark-thread-read)))
      (with-temp-buffer
        (insert-file-contents
         (expand-file-name (concat "modules/email/autoload/" file) test-config-root))
        (dolist (command commands)
          (expect (buffer-string)
                  :to-match (format "^;;;###autoload\n(defun %s " command)))))))

(describe "email module stars"
  (it "draws the star in a summary column of its own"
    ;; Gnus's own column draws the tick on a read message only
    (expect gnus-summary-line-format :to-match "%U%R%uS ")
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "modules/email/autoload/marks.el" test-config-root))
      (expect (buffer-string)
              :to-match "^;;;###autoload\n(defun gnus-user-format-function-S ")))
  (it "keeps the star of an article Gnus marks read as it displays it"
    ;; ahead of Gnus's own function, which would drop the star
    (require 'gnus-sum)
    (let ((gnus-mark-article-hook (copy-sequence gnus-mark-article-hook)))
      (dolist (form (email-tests--config-forms 'gnus-sum))
        (eval form t))
      (expect gnus-mark-article-hook
              :to-equal '(mail-keep-star-on-read-h
                          gnus-summary-mark-read-and-unread-as-read))))
  (it "loads the hook function on the first article, before any command of its file"
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "modules/email/autoload/marks.el" test-config-root))
      (expect (buffer-string)
              :to-match "^;;;###autoload\n(defun mail-keep-star-on-read-h "))))

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

(describe "email module startup"
  (it "reads each maildir group on its own, never the whole store at once"
    (require 'nnmaildir)
    (unwind-protect
        (progn
          (dolist (form (email-tests--config-forms 'nnmaildir))
            (eval form t))
          (expect (advice-member-p #'defer-mail-server-scan-a 'nnmaildir-request-scan)
                  :to-be-truthy)
          ;; entering a label, and filing a copy or a moved message into one
          (expect (advice-member-p #'scan-unknown-mail-group-a 'nnmaildir-request-group)
                  :to-be-truthy)
          (expect (advice-member-p #'scan-unknown-mail-group-a
                                   'nnmaildir-request-accept-article)
                  :to-be-truthy)
          (expect (advice-member-p #'scan-mail-group-on-miss-a
                                   'nnmaildir-base-name-to-article-number)
                  :to-be-truthy))
      (advice-remove 'nnmaildir-request-scan #'defer-mail-server-scan-a)
      (advice-remove 'nnmaildir-request-group #'scan-unknown-mail-group-a)
      (advice-remove 'nnmaildir-request-accept-article #'scan-unknown-mail-group-a)
      (advice-remove 'nnmaildir-base-name-to-article-number #'scan-mail-group-on-miss-a)))
  (it "queues the routine groups once Gnus has started, after the subscriptions"
    ;; a label subscribed at this start is queued with the others
    (require 'gnus)
    (let ((gnus-started-hook nil))
      (dolist (form (email-tests--config-forms 'gnus))
        (when (and (eq (car-safe form) 'add-hook)
                   (equal (cadr form) ''gnus-started-hook))
          (eval form t)))
      (expect gnus-started-hook :to-equal '(subscribe-mail-groups queue-mail-refresh))))
  (it "loads the advice and the hook function before any command of their file ran"
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "modules/email/autoload/mail.el" test-config-root))
      (dolist (fn '(defer-mail-server-scan-a scan-mail-group-on-miss-a
                    scan-unknown-mail-group-a queue-mail-refresh))
        (expect (buffer-string)
                :to-match (format "^;;;###autoload\n(defun %s " fn))))))

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
