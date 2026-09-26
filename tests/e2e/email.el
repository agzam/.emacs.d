;;; tests/e2e/email.el --- Gnus over a fixture maildir -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; nnmaildir is pure elisp, so a maildir holding read and unread messages
;; boots Gnus for real: the group buffer, the summary buffer with its flag
;; mapping, a reply composed through the posting styles, and the one-buffer
;; thread view over a thread chained by References.  The fixture server
;; carries the real server name, so the group parameters, the inbox group
;; and the Gcc target of the module config are the ones under test.  Every
;; action goes through a real keypress.

(require 'cl-lib)
;; the let-bound Gnus variables must already be special when the scenario
;; is defined, or `gnus' loading gnus-start trips over lexical bindings
(require 'gnus)
(require 'gnus-start)
(require 'gnus-sum)
(require 'gnus-search)

(defun email-e2e--write-message (file from subject id &optional date references xref body)
  "Write a minimal RFC 822 message to FILE.
FROM, SUBJECT, ID, DATE, REFERENCES and XREF fill the headers; BODY
follows the body's first line."
  (with-temp-file file
    (insert "From: " from "\n"
            "To: to.plotnick@gmail.com\n"
            "Subject: " subject "\n"
            "Date: " (or date "Tue, 22 Sep 2026 10:00:00 +0000") "\n"
            "Message-ID: <" id "@fixture.example>\n"
            (if references (concat "References: " references "\n") "")
            (if xref (concat "Xref: " xref "\n") "")
            "\n"
            "body of " subject "\n"
            (or body ""))))

(defun email-e2e--write-alternative (file subject id plain html)
  "Write to FILE a multipart/alternative message with PLAIN and HTML parts.
SUBJECT and ID fill the headers."
  (with-temp-file file
    (insert "From: Carol <carol@example.com>\n"
            "To: to.plotnick@gmail.com\n"
            "Subject: " subject "\n"
            "Date: Tue, 22 Sep 2026 12:00:00 +0000\n"
            "Message-ID: <" id "@fixture.example>\n"
            "MIME-Version: 1.0\n"
            "Content-Type: multipart/alternative; boundary=\"alt\"\n"
            "\n"
            "--alt\n"
            "Content-Type: text/plain; charset=utf-8\n"
            "\n"
            plain "\n"
            "--alt\n"
            "Content-Type: text/html; charset=utf-8\n"
            "\n"
            html "\n"
            "--alt--\n")))

(defun email-e2e--write-list-message (file from subject id date &optional references)
  "Write to FILE a message FROM posted to emacs-devel with two people in Cc.
SUBJECT, ID, DATE and REFERENCES fill the headers."
  (with-temp-file file
    (insert "From: " from "\n"
            "To: emacs-devel@gnu.org\n"
            "Cc: Carol <carol@example.com>, Dan <dan@example.com>\n"
            "List-Id: \"Emacs development discussions.\" <emacs-devel.gnu.org>\n"
            "List-Post: <mailto:emacs-devel@gnu.org>\n"
            "Subject: " subject "\n"
            "Date: " date "\n"
            "Message-ID: <" id "@fixture.example>\n"
            (if references (concat "References: " references "\n") "")
            "\n"
            "body of " subject "\n")))

(defun email-e2e--article (subject)
  "Number of the article with SUBJECT in the current summary."
  (mail-header-number
   (seq-find (lambda (header) (equal (mail-header-subject header) subject))
             gnus-newsgroup-headers)))

(defun email-e2e--idle (seconds &optional done)
  "Sit idle for SECONDS, or until DONE returns non-nil; return DONE's value.
Idle timers never run inside a `sit-for' called from a running command.
An untimed `read-event' is idle, and a timer ends it."
  (let ((poll (run-at-time 0.05 0.05
                           (lambda ()
                             (when (and done (funcall done))
                               (throw 'email-e2e--idle t))))))
    (unwind-protect
        (catch 'email-e2e--idle
          (with-timeout (seconds nil)
            (while t (read-event))))
      (cancel-timer poll))
    (and done (funcall done))))

(defun email-e2e ()
  "Gnus reads the fixture maildir, replies, and shows a thread in one buffer."
  (let* ((root (expand-file-name "mail/" e2e-work-dir))
         (inbox (expand-file-name "inbox/" root))
         ;; the group a queued deletion is moved into has to exist when
         ;; Gnus starts, or nnmaildir refuses the article
         (trash (expand-file-name "trash/" root))
         ;; a label of its own, so the inbox holds what the flow counts
         (html (expand-file-name "html/" root))
         ;; read and star apart, the way Gmail keeps them
         (starred (expand-file-name "starred/" root))
         ;; a list thread answered every way, and the labels messages
         ;; are moved and copied into
         (lists (expand-file-name "lists/" root))
         (moved (expand-file-name "moved/" root))
         (labelled (expand-file-name "labelled/" root))
         ;; bulk groups, which no start reads: notmuch answers from the
         ;; archive, and a mailing list label is entered from its line
         (archive (expand-file-name "archive/" root))
         (emacs (expand-file-name "emacs/" root))
         ;; stands in for notmuch, which CI lacks: it answers every search
         ;; with the archive's copy, as the real index does, and counts
         ;; 2, 3 ... for a batch, which it keeps in counted-log
         (notmuch (expand-file-name "notmuch" e2e-work-dir))
         (counted-log (expand-file-name "notmuch-counted" e2e-work-dir))
         ;; what the %uS column draws on a starred message
         (star (string #x2217))
         (results '())
         ;; gnus-started-hook subscribes every group under this root;
         ;; mail-groups would add gmane, and CI has no news server
         (gmail-maildir root)
         (mail-groups nil)
         ;; moved sits with the bulk groups, so , m files into a group
         ;; nnmaildir has not read this session
         (mail-bulk-groups '("nnmaildir+gmail:archive" "nnmaildir+gmail:emacs"
                             "nnmaildir+gmail:moved"))
         (gnus-search-notmuch-program notmuch)
         (gnus-search-notmuch-remove-prefix root)
         (gnus-search-engine-instance-alist nil)
         (gnus-secondary-select-methods
          `((nnmaildir "gmail" (directory ,root) (get-new-mail nil))))
         (gnus-startup-file (expand-file-name "newsrc" e2e-work-dir))
         (gnus-init-file (expand-file-name "gnus-init" e2e-work-dir))
         (gnus-directory (expand-file-name "news/" e2e-work-dir))
         (gnus-interactive-exit nil)
         (gnus-expert-user t)
         reply)
    (dolist (dir (list inbox trash html starred lists moved labelled archive emacs))
      (dolist (sub '("cur" "new" "tmp"))
        (make-directory (expand-file-name sub dir) t)))
    (email-e2e--write-list-message (expand-file-name "cur/1700000031.31.fixture:2,S" emacs)
                                   "Eli <eli@example.com>" "emacs-devel post" "devel-post"
                                   "Sun, 20 Sep 2026 10:00:00 +0000")
    (let ((archived (expand-file-name "cur/1700000030.30.fixture:2,S" archive)))
      (email-e2e--write-message archived "Ann <ann@example.com>" "archived" "archived"
                                "Sun, 20 Sep 2026 09:00:00 +0000")
      (with-temp-file notmuch
        (insert "#!/bin/sh\n"
                "case \" $* \" in\n"
                "  *\" count \"*)\n"
                "    : > '" counted-log "'\n"
                "    n=1\n"
                "    while IFS= read -r query; do\n"
                "      printf '%s\\n' \"$query\" >> '" counted-log "'\n"
                "      n=$((n+1)); echo $n\n"
                "    done;;\n"
                "  *) printf '%s\\n' '" archived "';;\n"
                "esac\n"))
      (set-file-modes notmuch #o755))
    ;; the sender's colors would paint white on white, and the paragraph
    ;; is longer than any terminal line
    (email-e2e--write-alternative
     (expand-file-name "cur/1700000005.6.fixture:2,S" html) "html letter" "html"
     "PLAIN alternative\n\n> quoted once\n> > quoted twice"
     (concat "<p>HTML alternative</p>"
             "<p>paragraph start " (string-join (make-list 60 "wrapping") " ")
             " paragraph end</p>"
             "<p style=\"color:#ffffff;background-color:#ffffff\">white on white</p>"
             "<blockquote><p>quoted once</p>"
             "<blockquote><p>quoted twice</p></blockquote></blockquote>"))
    ;; longer than any window, so J has an article to scroll
    (email-e2e--write-message (expand-file-name "cur/1700000006.7.fixture:2,S" html)
                              "Dan <dan@example.com>" "long letter" "long"
                              "Tue, 22 Sep 2026 13:00:00 +0000" nil nil
                              (mapconcat (lambda (n) (format "line %d\n" n))
                                         (number-sequence 1 300) ""))
    (email-e2e--write-message (expand-file-name "new/1700000001.1.fixture" inbox)
                              "Someone <someone@example.com>" "fresh" "fresh")
    (email-e2e--write-message (expand-file-name "cur/1700000000.2.fixture:2,S" inbox)
                              "Other <other@example.com>" "seen" "seen")
    ;; one thread, oldest first, with only its last message unread
    (email-e2e--write-message (expand-file-name "cur/1700000002.3.fixture:2,S" inbox)
                              "Ann <ann@example.com>" "release plan" "plan"
                              "Mon, 21 Sep 2026 09:00:00 +0000")
    ;; two lines one level deep, which gnus-cite would paint too, and a
    ;; single line two levels deep, which it would skip
    (email-e2e--write-message (expand-file-name "cur/1700000003.4.fixture:2,S" inbox)
                              "Bob <bob@example.com>" "Re: release plan (Bob)" "plan-bob"
                              "Mon, 21 Sep 2026 10:00:00 +0000"
                              "<plan@fixture.example>" nil
                              (concat "> the plan, first line\n"
                                      "> the plan, second line\n"
                                      "> > the draft before it\n"))
    ;; the last one carries the Xref header every gmane article has
    (email-e2e--write-message (expand-file-name "new/1700000004.5.fixture" inbox)
                              "Ann <ann@example.com>" "Re: release plan (Ann)" "plan-ann"
                              "Mon, 21 Sep 2026 11:00:00 +0000"
                              "<plan@fixture.example> <plan-bob@fixture.example>"
                              "news.gmane.io gmane.emacs.devel:346572")
    ;; one file per state of read and star, and the flow does one thing
    ;; to each
    (pcase-dolist (`(,file ,subject ,id)
                   '(("cur/1700000010.10.fixture:2,F" "starred, left alone" "left")
                     ("cur/1700000011.11.fixture:2,F" "starred, opened" "opened")
                     ("cur/1700000012.12.fixture:2,F" "starred, shown" "shown")
                     ("cur/1700000013.13.fixture:2,FS" "starred and read, opened" "read-opened")
                     ("cur/1700000014.14.fixture:2,FS" "starred and read, marked unread" "read-unread")
                     ("new/1700000015.15.fixture" "unread, starred" "plain")
                     ("cur/1700000016.16.fixture:2,F" "starred, unstarred" "unstarred")))
      (email-e2e--write-message (expand-file-name file starred)
                                "Eve <eve@example.com>" subject id))
    ;; a read root and its starred unread answer, then two newer
    ;; messages that leave for other labels
    (email-e2e--write-list-message (expand-file-name "cur/1700000020.20.fixture:2,S" lists)
                                   "Carol <carol@example.com>" "list plan" "list-plan"
                                   "Mon, 21 Sep 2026 09:00:00 +0000")
    (email-e2e--write-list-message (expand-file-name "cur/1700000021.21.fixture:2,F" lists)
                                   "Dan <dan@example.com>" "Re: list plan" "list-plan-dan"
                                   "Mon, 21 Sep 2026 10:00:00 +0000"
                                   "<list-plan@fixture.example>")
    (email-e2e--write-message (expand-file-name "cur/1700000022.22.fixture:2,S" lists)
                              "Frank <frank@example.com>" "to move" "to-move"
                              "Tue, 22 Sep 2026 08:00:00 +0000")
    (email-e2e--write-message (expand-file-name "cur/1700000023.23.fixture:2,S" lists)
                              "Grace <grace@example.com>" "to label" "to-label"
                              "Tue, 22 Sep 2026 07:00:00 +0000")
    (cl-flet ((record (label ok &rest kv)
                (push (append (list :label (format "email: %s" label) :ok ok) kv)
                      results))
              ;; the groups nnmaildir has read this session
              (read-groups ()
                (when-let* ((server (alist-get "gmail" nnmaildir--servers nil nil #'equal)))
                  (sort (hash-table-keys (nnmaildir--srv-groups server)) #'string<)))
              (open-subjects ()
                (mapcar (lambda (message)
                          (mail-header-subject (mail-thread-message-header message)))
                        (seq-filter #'mail-thread-message-open-p mail-thread-messages)))
              (subject-at-point ()
                (mail-header-subject
                 (mail-thread-message-header (mail-thread-message-at-point))))
              (thread-text ()
                (with-current-buffer mail-thread-buffer-name
                  (buffer-substring-no-properties (point-min) (point-max))))
              (unread-p (article)
                (with-current-buffer "*Summary nnmaildir+gmail:inbox*"
                  (and (memq article gnus-newsgroup-unreads) t)))
              (messages-in (dir)
                (mapcan (lambda (sub)
                          (directory-files (expand-file-name sub dir) t "\\`[^.]"))
                        '("cur" "new")))
              (message-ids (files)
                (mapcar (lambda (file)
                          (with-temp-buffer
                            (insert-file-contents file)
                            (mail-fetch-field "Message-ID")))
                        files))
              ;; the queue glyph and Gnus's read mark that start the line
              ;; of ARTICLE, wherever point is
              (line-marks (article)
                (save-excursion
                  (gnus-summary-goto-subject article)
                  (buffer-substring-no-properties (line-beginning-position)
                                                  (+ 2 (line-beginning-position)))))
              ;; the %uS column of ARTICLE's line
              (line-star (article)
                (save-excursion
                  (gnus-summary-goto-subject article)
                  (buffer-substring-no-properties (+ 3 (line-beginning-position))
                                                  (+ 4 (line-beginning-position)))))
              ;; the maildir flags of the message with ID, as nnmaildir
              ;; saved them
              (flags-of (dir id)
                (let ((wanted (format "<%s@fixture.example>" id)))
                  (seq-some (lambda (file)
                              (when (with-temp-buffer
                                      (insert-file-contents file)
                                      (equal (mail-fetch-field "Message-ID") wanted))
                                (if (string-match ":2,\\([A-Z]*\\)\\'" file)
                                    (match-string 1 file)
                                  "")))
                            (mapcan (lambda (sub)
                                      (directory-files (expand-file-name sub dir) t "\\`[^.]"))
                                    '("cur" "new")))))
              (line-face (article)
                (save-excursion
                  (gnus-summary-goto-subject article)
                  (get-text-property (line-beginning-position) 'face)))
              (article-below (article)
                (save-excursion
                  (gnus-summary-goto-subject article)
                  (gnus-summary-find-next)))
              ;; what redisplay draws: an overlay face wins over the text's
              (quote-faces ()
                (mapcar (lambda (text)
                          (save-excursion
                            (goto-char (point-min))
                            (when (search-forward text nil t)
                              (list (get-char-property (line-beginning-position) 'face)
                                    (get-char-property (1- (line-end-position)) 'face)))))
                        '("the plan, first line" "the plan, second line"
                          "the draft before it")))
              (cite-overlays ()
                (seq-filter (lambda (overlay)
                              (string-prefix-p "gnus-cite"
                                               (format "%s" (overlay-get overlay 'face))))
                            (overlays-in (point-min) (point-max))))
              ;; each quoted line of the HTML letter with the faces drawn
              ;; at its start and end
              (html-quotes ()
                (mapcar (lambda (text)
                          (save-excursion
                            (goto-char (point-min))
                            (when (search-forward text nil t)
                              (list (buffer-substring-no-properties
                                     (line-beginning-position) (line-end-position))
                                    (get-char-property (line-beginning-position) 'face)
                                    (get-char-property (1- (line-end-position)) 'face)))))
                        '("quoted once" "quoted twice")))
              (sender-colors ()
                (save-excursion
                  (goto-char (point-min))
                  (if (not (search-forward "white on white" nil t))
                      :missing
                    (let ((face (get-char-property (1- (point)) 'face)))
                      (seq-filter (lambda (spec)
                                    (and (keywordp (car-safe spec))
                                         (or (plist-get spec :foreground)
                                             (plist-get spec :background))))
                                  (if (keywordp (car-safe face)) (list face) (ensure-list face)))))))
              ;; each window's buffer, left column and width
              (layout ()
                (format "%S" (mapcar (lambda (window)
                                       (list (buffer-name (window-buffer window))
                                             (window-left-column window)
                                             (window-total-width window)))
                                     (window-list nil 'nomini (frame-first-window)))))
              ;; buffer lines and screen lines the long paragraph takes
              (paragraph-lines ()
                (save-excursion
                  (goto-char (point-min))
                  (if (not (search-forward "paragraph start" nil t))
                      :missing
                    (let ((start (line-beginning-position)))
                      (search-forward "paragraph end")
                      (list (count-lines start (point))
                            (count-screen-lines start (point))))))))
      (unwind-protect
          (condition-case e
              (progn
                (discard-input)
                ;; the start reads no maildir: the routine groups follow on
                ;; timer turns, and nothing reads the bulk ones
                (gnus)
                (record "gnus shows the group buffer before reading any maildir"
                        (and (gnus-alive-p) (null (read-groups)))
                        :got (format "alive %s, read %S" (gnus-alive-p) (read-groups)))
                ;; the turns run on plain timers, which a timed wait
                ;; serves; `email-e2e--idle' would leave Emacs idle, and
                ;; the thread fill's cases below need it busy
                (with-timeout (10)
                  (while (or mail-refresh-queue (timerp mail-refresh-timer))
                    (accept-process-output nil 0.05)))
                (record "the routine groups are read on timer turns after the start"
                        (equal (read-groups)
                               '("html" "inbox" "labelled" "lists" "starred" "trash"))
                        :got (format "%S" (read-groups)))
                (record "a routine group's line counts its unread mail, a bulk group's line does not"
                        (and (eql (gnus-group-unread "nnmaildir+gmail:inbox") 2)
                             (not (numberp (gnus-group-unread "nnmaildir+gmail:archive"))))
                        :got (format "inbox %S, archive %S"
                                     (gnus-group-unread "nnmaildir+gmail:inbox")
                                     (gnus-group-unread "nnmaildir+gmail:archive")))
                ;; the stand-in notmuch answers with the archive's copy
                (search-mail "archived")
                (record "a search hit in a group no start read opens in the search summary"
                        (and (derived-mode-p 'gnus-summary-mode)
                             (equal (mapcar #'mail-header-subject gnus-newsgroup-headers)
                                    '("archived")))
                        :got (format "%s: %S" major-mode
                                     (mapcar #'mail-header-subject gnus-newsgroup-headers)))
                (gnus-summary-exit-no-update)
                (record "the search reads the archive and no other bulk group"
                        (and (member "archive" (read-groups))
                             (not (member "emacs" (read-groups)))
                             (not (member "moved" (read-groups))))
                        :got (format "%S" (read-groups)))
                ;; entering a label is where Gnus asks nnmaildir for it
                (gnus-group-jump-to-group "nnmaildir+gmail:emacs")
                (execute-kbd-macro (kbd "RET"))
                (record "RET on a bulk group's line reads the group and shows its mail"
                        (and (derived-mode-p 'gnus-summary-mode)
                             (equal (mapcar #'mail-header-subject gnus-newsgroup-headers)
                                    '("emacs-devel post")))
                        :got (format "%s: %S" major-mode
                                     (mapcar #'mail-header-subject gnus-newsgroup-headers)))
                (gnus-summary-exit-no-update)
                (open-mail-inbox)
                (record "opens the inbox summary"
                        (and (derived-mode-p 'gnus-summary-mode)
                             (equal gnus-newsgroup-name "nnmaildir+gmail:inbox"))
                        :got (format "%s in %s" major-mode gnus-newsgroup-name))
                (record "maps maildir flags to unread and read"
                        (and (= 5 (length gnus-newsgroup-headers))
                             (= 2 (length gnus-newsgroup-unreads)))
                        :got (format "%d headers, %d unread"
                                     (length gnus-newsgroup-headers)
                                     (length gnus-newsgroup-unreads)))
                (gnus-summary-goto-subject (email-e2e--article "fresh"))
                (execute-kbd-macro "r")
                (setq reply (current-buffer))
                (record "r composes a reply in message-mode"
                        (derived-mode-p 'message-mode)
                        :got (format "%s" major-mode))
                (record "the reply is sent as agzam.ibragimov"
                        (let ((from (message-fetch-field "From")))
                          (and from (string-match-p "agzam\\.ibragimov@gmail\\.com" from)))
                        :got (message-fetch-field "From"))
                (record "the reply answers the original sender"
                        (equal (message-fetch-field "To") "Someone <someone@example.com>")
                        :got (message-fetch-field "To"))
                (record "the reply files a copy into the synced sent group"
                        (equal (message-fetch-field "Gcc") "nnmaildir+gmail:sent")
                        :got (message-fetch-field "Gcc"))
                (set-buffer-modified-p nil)
                (kill-buffer reply)
                (setq reply nil)
                (switch-to-buffer "*Summary nnmaildir+gmail:inbox*")
                (gnus-summary-exit-no-update)
                (switch-to-buffer gnus-group-buffer)
                (gnus-group-jump-to-group "nnmaildir+gmail:inbox")
                ;; gR belongs to evil-collection until the module takes
                ;; it back, and it used to ask nnmaildir for a
                ;; server-wide scan - every label in the store
                (let (called)
                  (cl-letf (((symbol-function 'refresh-mail-groups)
                             (lambda () (interactive) (setq called 'routine-groups)))
                            ((symbol-function 'gnus-group-get-new-news)
                             (lambda (&rest _) (interactive "P") (setq called 'whole-server))))
                    (execute-kbd-macro (kbd "gR")))
                  (record "gR refreshes the routine groups, not the whole server"
                          (eq called 'routine-groups)
                          :got (format "%s" called)))
                ;; RET from the group buffer is the path the `display'
                ;; parameter governs; without it only unread mail shows
                (gnus-group-jump-to-group "nnmaildir+gmail:inbox")
                (execute-kbd-macro (kbd "RET"))
                (record "RET on the group line shows read mail too"
                        (and (derived-mode-p 'gnus-summary-mode)
                             (= 5 (length gnus-newsgroup-headers)))
                        :got (format "%s, %d headers" major-mode
                                     (length gnus-newsgroup-headers)))
                ;; the article buffer shows a shorter message without
                ;; Xref, the way it does after reading mail; the render
                ;; must not look the thread's Xref up in it
                (gnus-summary-goto-subject (email-e2e--article "fresh"))
                (gnus-summary-select-article)
                (setq ann (email-e2e--article "Re: release plan (Ann)"))
                ;; the thread view, entered from the middle message
                (gnus-summary-goto-subject (email-e2e--article "Re: release plan (Bob)"))
                (execute-kbd-macro (kbd "RET"))
                (record "RET in the summary opens the whole thread in one buffer"
                        (and (derived-mode-p 'mail-thread-mode)
                             (= 3 (length mail-thread-messages)))
                        :got (format "%s, %d messages" major-mode
                                     (length mail-thread-messages)))
                (record "the thread opens on the message the summary was on"
                        (equal (subject-at-point) "Re: release plan (Bob)")
                        :got (subject-at-point))
                (record "RET renders the entry message and leaves the unread one to the fill"
                        (and (equal (open-subjects) '("Re: release plan (Bob)"))
                             (not (string-match-p "body of Re: release plan (Ann)"
                                                  (thread-text))))
                        :got (format "%S open" (open-subjects)))
                (record "a folded message costs a line, not a render"
                        (not (string-match-p "body of release plan" (thread-text)))
                        :got (format "%d chars" (buffer-size)))
                (redisplay t)
                (record "the thread view faces each quoted line by depth, markers included"
                        (equal (quote-faces)
                               '((message-cited-text-1 message-cited-text-1)
                                 (message-cited-text-1 message-cited-text-1)
                                 (message-cited-text-2 message-cited-text-2)))
                        :got (format "%S" (quote-faces)))
                ;; q before Emacs goes idle, so the fill never gets a turn
                (execute-kbd-macro "q")
                (email-e2e--idle 0.5)
                (record "q stops the fill: the unread message stays unrendered and unread"
                        (and (not (string-match-p "body of Re: release plan (Ann)"
                                                  (thread-text)))
                             (unread-p ann))
                        :got (format "unread %S, %d chars" (unread-p ann)
                                     (length (thread-text))))
                (switch-to-buffer "*Summary nnmaildir+gmail:inbox*")
                (gnus-summary-goto-subject (email-e2e--article "Re: release plan (Bob)"))
                (execute-kbd-macro (kbd "RET"))
                (execute-kbd-macro (kbd "C-j"))
                (record "C-j reaches the unread message before its body arrives"
                        (and (equal (subject-at-point) "Re: release plan (Ann)")
                             (not (string-match-p "body of Re: release plan (Ann)"
                                                  (thread-text))))
                        :got (subject-at-point))
                (email-e2e--idle 5 (lambda ()
                                     (string-match-p "body of Re: release plan (Ann)"
                                                     (thread-text))))
                (record "the unread message renders once Emacs is idle"
                        (equal (open-subjects)
                               '("Re: release plan (Bob)" "Re: release plan (Ann)"))
                        :got (format "%S open" (open-subjects)))
                (record "the message with an Xref header renders while another is shown"
                        (string-match-p "body of Re: release plan (Ann)" (thread-text))
                        :got (format "%d chars" (length (thread-text))))
                (record "the fill marks the message it rendered read"
                        (not (unread-p ann))
                        :got (format "unread %S" (unread-p ann)))
                (execute-kbd-macro (kbd "C-k"))
                (execute-kbd-macro (kbd "C-k"))
                (record "C-k moves back to the first message"
                        (equal (subject-at-point) "release plan")
                        :got (subject-at-point))
                (execute-kbd-macro (kbd "TAB"))
                (record "TAB renders the message it unfolds"
                        (and (string-match-p "body of release plan"
                                             (buffer-substring-no-properties
                                              (point-min) (point-max)))
                             (equal (subject-at-point) "release plan"))
                        :got (format "%S open" (open-subjects)))
                (execute-kbd-macro "q")
                (record "q returns to the summary it came from"
                        (eq (window-buffer (selected-window))
                            (get-buffer "*Summary nnmaildir+gmail:inbox*"))
                        :got (buffer-name (window-buffer (selected-window))))
                ;; the thread buffer keeps no MIME handles, so the
                ;; article view is where an attachment is opened
                (switch-to-buffer "*Summary nnmaildir+gmail:inbox*")
                (gnus-summary-goto-subject (email-e2e--article "Re: release plan (Bob)"))
                (execute-kbd-macro (kbd "RET"))
                (execute-kbd-macro (kbd "RET"))
                (record "RET in the thread reads the message in the article buffer"
                        (and (derived-mode-p 'gnus-article-mode)
                             (string-match-p "body of Re: release plan (Bob)"
                                             (buffer-substring-no-properties
                                              (point-min) (point-max))))
                        :got (format "%s, %s" major-mode gnus-article-current))
                (redisplay t)
                (record "the article buffer faces the quotes the same way, with no gnus-cite overlay"
                        (and (equal (quote-faces)
                                    '((message-cited-text-1 message-cited-text-1)
                                      (message-cited-text-1 message-cited-text-1)
                                      (message-cited-text-2 message-cited-text-2)))
                             (null (cite-overlays)))
                        :got (format "%S, %d gnus-cite overlays"
                                     (quote-faces) (length (cite-overlays))))
                ;; replying cites every line once more, and message-mode
                ;; paints it with the same faces by the same depth rule
                (switch-to-buffer "*Summary nnmaildir+gmail:inbox*")
                (gnus-summary-goto-subject (email-e2e--article "Re: release plan (Bob)"))
                (execute-kbd-macro "R")
                (setq reply (current-buffer))
                (font-lock-ensure)
                (record "a cited reply paints the quotes a level deeper with the same faces"
                        (and (derived-mode-p 'message-mode)
                             (equal (quote-faces)
                                    '((message-cited-text-2 message-cited-text-2)
                                      (message-cited-text-2 message-cited-text-2)
                                      (message-cited-text-3 message-cited-text-3))))
                        :got (format "%s, %S" major-mode (quote-faces)))
                (set-buffer-modified-p nil)
                (kill-buffer reply)
                (setq reply nil)
                ;; read and star toggle Gnus's own marks; they and the
                ;; queue below move to the message below what they marked,
                ;; and take a visual selection whole
                (switch-to-buffer "*Summary nnmaildir+gmail:inbox*")
                (let* ((fresh (email-e2e--article "fresh"))
                       (below (article-below fresh))
                       (after (article-below below))
                       ;; displaying fresh further up read it
                       (before (line-marks fresh)))
                  (gnus-summary-goto-subject fresh)
                  (execute-kbd-macro "!")
                  (record "! marks a read message unread and moves down"
                          (and (equal before " R")
                               (equal (line-marks fresh) "  ")
                               (eql (gnus-summary-article-number) below))
                          :got (format "line starts %S, then %S, point on %S, below is %S"
                                       before (line-marks fresh)
                                       (gnus-summary-article-number) below))
                  (execute-kbd-macro "k!")
                  (record "! on an unread message marks it read"
                          (and (equal (line-marks fresh) " r")
                               (eql (gnus-summary-article-number) below))
                          :got (format "line starts %S, point on %S"
                                       (line-marks fresh) (gnus-summary-article-number)))
                  (execute-kbd-macro "k=")
                  (record "= stars the message and moves down"
                          (and (equal (line-marks fresh) " !")
                               (equal (line-star fresh) star)
                               (memq fresh gnus-newsgroup-marked)
                               (eql (gnus-summary-article-number) below))
                          :got (format "line starts %S, star %S, point on %S"
                                       (line-marks fresh) (line-star fresh)
                                       (gnus-summary-article-number)))
                  (execute-kbd-macro "k!")
                  (record "! marks a starred message unread, keeps the star and moves down"
                          (and (equal (line-marks fresh) "  ")
                               (equal (line-star fresh) star)
                               (memq fresh gnus-newsgroup-unreads)
                               (memq fresh gnus-newsgroup-marked)
                               (eql (gnus-summary-article-number) below))
                          :got (format "line starts %S, star %S, point on %S"
                                       (line-marks fresh) (line-star fresh)
                                       (gnus-summary-article-number)))
                  (execute-kbd-macro "k!")
                  (record "! marks a starred unread message read and keeps the star"
                          (and (equal (line-marks fresh) " !")
                               (equal (line-star fresh) star)
                               (not (memq fresh gnus-newsgroup-unreads)))
                          :got (format "line starts %S, star %S"
                                       (line-marks fresh) (line-star fresh)))
                  (execute-kbd-macro "k=")
                  (record "= on a starred message unstars it and leaves it read"
                          (and (equal (line-marks fresh) " r")
                               (equal (line-star fresh) " ")
                               (not (memq fresh gnus-newsgroup-marked))
                               (not (memq fresh gnus-newsgroup-unreads)))
                          :got (format "line starts %S, star %S"
                                       (line-marks fresh) (line-star fresh)))
                  ;; visual state opens expreg-transient, which passes =
                  ;; on to the summary but swallows ! and u
                  (gnus-summary-goto-subject fresh)
                  (execute-kbd-macro "Vj=")
                  (record "= on a visual selection stars each message in it, ends visual state and moves below"
                          (and (equal (mapcar #'line-marks (list fresh below)) '(" !" " !"))
                               (eq evil-state 'normal)
                               (not (region-active-p))
                               (eql (gnus-summary-article-number) after))
                          :got (format "lines start %S, %s state, region %S, point on %S, after is %S"
                                       (mapcar #'line-marks (list fresh below)) evil-state
                                       (region-active-p) (gnus-summary-article-number) after))
                  (gnus-summary-goto-subject fresh)
                  (execute-kbd-macro "Vj=")
                  (record "= on a visual selection of starred messages unstars each, leaving it read"
                          (equal (mapcar #'line-marks (list fresh below)) '(" r" " r"))
                          :got (format "lines start %S" (mapcar #'line-marks (list fresh below)))))
                ;; deferred deletion and archive: nothing reaches the
                ;; store until x
                (let* ((seen (email-e2e--article "seen"))
                       (below (article-below seen))
                       (after (article-below below))
                       (plan (mapcar #'email-e2e--article
                                     '("release plan" "Re: release plan (Bob)"
                                       "Re: release plan (Ann)")))
                       (ann (car (last plan))))
                  (gnus-summary-goto-subject seen)
                  (execute-kbd-macro "d")
                  (record "d queues the message at point for the trash, draws D and moves down"
                          (and (eq (alist-get seen mail-marks) 'delete)
                               (eq (aref (line-marks seen) 0) ?D)
                               (eq (car-safe (line-face seen)) 'dired-flagged)
                               (eql (gnus-summary-article-number) below)
                               (= 5 (length (messages-in inbox))))
                          :got (format "%S, line starts %S, face %S, point on %S, below is %S, %d files"
                                       mail-marks (line-marks seen) (line-face seen)
                                       (gnus-summary-article-number) below
                                       (length (messages-in inbox))))
                  (execute-kbd-macro "ku")
                  (record "u takes it back, marks it unread, clears both columns and moves down"
                          (and (null mail-marks)
                               (equal (line-marks seen) "  ")
                               (memq seen gnus-newsgroup-unreads)
                               (eql (gnus-summary-article-number) below))
                          :got (format "%S, line starts %S, unread %S, point on %S"
                                       mail-marks (line-marks seen) gnus-newsgroup-unreads
                                       (gnus-summary-article-number)))
                  (gnus-summary-goto-subject seen)
                  (execute-kbd-macro "Vjd")
                  (record "d on a visual selection queues each message in it, ends visual state and moves below"
                          (and (equal (mail-marked-articles 'delete) (sort (list seen below) #'<))
                               (eq evil-state 'normal)
                               (not (region-active-p))
                               (eql (gnus-summary-article-number) after))
                          :got (format "%S, %s state, region %S, point on %S, after is %S"
                                       mail-marks evil-state (region-active-p)
                                       (gnus-summary-article-number) after))
                  (gnus-summary-goto-subject seen)
                  (execute-kbd-macro "uu")
                  (record "u twice from the top takes both messages back and leaves both unread"
                          (and (null mail-marks)
                               (memq seen gnus-newsgroup-unreads)
                               (memq below gnus-newsgroup-unreads))
                          :got (format "%S, unread %S" mail-marks gnus-newsgroup-unreads))
                  ;; the release plan thread is the only one with more
                  ;; than one message
                  (let* ((threads (if (memq below plan)
                                      (cons seen (copy-sequence plan))
                                    (list seen below)))
                         (last (if (memq below plan) ann below)))
                    (gnus-summary-goto-subject seen)
                    (execute-kbd-macro "VjA")
                    (record "A on a visual selection queues every thread it touches and moves below them"
                            (and (equal (mail-marked-articles 'archive) (sort threads #'<))
                                 (eql (gnus-summary-article-number) (or (article-below last) last)))
                            :got (format "%S, point on %S" mail-marks (gnus-summary-article-number))))
                  (gnus-summary-goto-subject seen)
                  (execute-kbd-macro "UU")
                  (record "U twice from the top takes both threads back"
                          (null mail-marks)
                          :got (format "%S" mail-marks))
                  (gnus-summary-goto-subject seen)
                  (execute-kbd-macro "d")
                  (gnus-summary-goto-subject (email-e2e--article "Re: release plan (Bob)"))
                  (execute-kbd-macro "A")
                  (record "A queues the whole thread at point for archive and moves below it"
                          (and (equal (mail-marked-articles 'archive) (sort (copy-sequence plan) #'<))
                               (eql (gnus-summary-article-number) (or (article-below ann) ann)))
                          :got (format "%S, point on %S" mail-marks (gnus-summary-article-number))))
                (execute-kbd-macro "x")
                (record "x moves the queued deletion into the trash maildir"
                        (equal (message-ids (messages-in trash)) '("<seen@fixture.example>"))
                        :got (format "%S" (message-ids (messages-in trash))))
                (record "x deletes the archived thread's files from the label"
                        (equal (message-ids (messages-in inbox)) '("<fresh@fixture.example>"))
                        :got (format "%S" (message-ids (messages-in inbox))))
                (record "the executed lines leave the summary"
                        (and (null mail-marks)
                             (= 1 (count-lines (point-min) (point-max)))
                             (equal (mail-header-subject
                                     (gnus-summary-article-header
                                      (progn (goto-char (point-min))
                                             (gnus-summary-article-number))))
                                    "fresh"))
                        :got (format "%S, %d lines" mail-marks
                                     (count-lines (point-min) (point-max))))
                ;; an HTML letter with a plain alternative: both views
                ;; show the HTML part, quoted like plain mail and wrapped
                ;; at the window edge instead of cut there
                (switch-to-buffer "*Summary nnmaildir+gmail:inbox*")
                (gnus-summary-exit-no-update)
                (switch-to-buffer gnus-group-buffer)
                (gnus-group-jump-to-group "nnmaildir+gmail:html")
                (execute-kbd-macro (kbd "RET"))
                (gnus-summary-goto-subject (email-e2e--article "html letter"))
                (execute-kbd-macro (kbd "RET"))
                (redisplay t)
                (record "the thread view shows the HTML part, its quote levels drawn as > lines"
                        (and (derived-mode-p 'mail-thread-mode)
                             (string-match-p "HTML alternative" (thread-text))
                             (not (string-match-p "PLAIN alternative" (thread-text)))
                             (equal (html-quotes)
                                    '(("> quoted once" message-cited-text-1 message-cited-text-1)
                                      ("> > quoted twice" message-cited-text-2 message-cited-text-2))))
                        :got (format "%s, %S" major-mode (html-quotes)))
                (record "the thread view drops the sender's colors"
                        (null (sender-colors))
                        :got (format "%S" (sender-colors)))
                (record "the thread view wraps a long HTML paragraph instead of cutting it"
                        (pcase (paragraph-lines) (`(1 ,screen) (< 1 screen)))
                        :got (format "%S buffer and screen lines" (paragraph-lines)))
                (execute-kbd-macro (kbd "RET"))
                (redisplay t)
                (record "the article buffer shows the HTML part, its quote levels drawn as > lines"
                        (and (derived-mode-p 'gnus-article-mode)
                             (string-match-p "HTML alternative" (buffer-string))
                             (not (string-match-p "PLAIN alternative" (buffer-string)))
                             (equal (html-quotes)
                                    '(("> quoted once" message-cited-text-1 message-cited-text-1)
                                      ("> > quoted twice" message-cited-text-2 message-cited-text-2))))
                        :got (format "%s, %S" major-mode (html-quotes)))
                (record "the article buffer drops the sender's colors"
                        (null (sender-colors))
                        :got (format "%S" (sender-colors)))
                (record "the article buffer wraps a long HTML paragraph instead of cutting it"
                        (pcase (paragraph-lines) (`(1 ,screen) (< 1 screen)))
                        :got (format "%S buffer and screen lines, truncate-lines %S"
                                     (paragraph-lines) truncate-lines))
                ;; the thread and the article open right of the summary,
                ;; and a window left of Gnus keeps its width while Gnus
                ;; lays out again: every read forces the article layout,
                ;; q in the summary the group one
                (delete-other-windows)
                (switch-to-buffer "*Summary nnmaildir+gmail:html*")
                (set-window-buffer (split-window nil nil 'left) (get-buffer-create "*beside*"))
                (cl-flet ((summary-window ()
                            (get-buffer-window "*Summary nnmaildir+gmail:html*"))
                          (beside-width ()
                            (window-total-width (get-buffer-window "*beside*")))
                          (right-of-summary-p (buffer)
                            (let ((summary (get-buffer-window "*Summary nnmaildir+gmail:html*"))
                                  (window (get-buffer-window buffer)))
                              (and summary window
                                   (= (window-top-line summary) (window-top-line window))
                                   (< (window-left-column summary) (window-left-column window))))))
                  (let ((width (beside-width))
                        (article "*Article nnmaildir+gmail:html*"))
                    (gnus-summary-goto-subject (email-e2e--article "html letter"))
                    (execute-kbd-macro (kbd "RET"))
                    (record "RET opens the thread to the right of the summary"
                            (and (eq (window-buffer (selected-window))
                                     (get-buffer mail-thread-buffer-name))
                                 (right-of-summary-p mail-thread-buffer-name)
                                 (= width (beside-width)))
                            :got (layout))
                    (execute-kbd-macro (kbd "RET"))
                    (record "the article opens to the right of the summary"
                            (right-of-summary-p article)
                            :got (layout))
                    ;; the article is shown this time, so Gnus deletes the
                    ;; summary's window before it lays out again
                    (select-window (summary-window))
                    (execute-kbd-macro (kbd "RET"))
                    (execute-kbd-macro (kbd "RET"))
                    (record "reading an article again leaves the window beside Gnus at its width"
                            (= width (beside-width))
                            :got (format "%d wide before, now %s" width (layout)))
                    ;; RET once more from the summary while the thread shows
                    (select-window (summary-window))
                    (execute-kbd-macro (kbd "RET"))
                    (select-window (summary-window))
                    (execute-kbd-macro (kbd "RET"))
                    (execute-kbd-macro "q")
                    (record "q in the thread gives its window back to the summary"
                            (and (eq (selected-window) (summary-window))
                                 (null (get-buffer-window mail-thread-buffer-name))
                                 (= width (beside-width)))
                            :got (layout))
                    (with-current-buffer "*Summary nnmaildir+gmail:html*"
                      (gnus-summary-goto-subject (email-e2e--article "long letter")))
                    (execute-kbd-macro "J")
                    (record "J shows the article at point right of the summary, point staying there"
                            (and (eq (selected-window) (summary-window))
                                 (right-of-summary-p article)
                                 (with-current-buffer article
                                   (string-match-p "body of long letter" (buffer-string))))
                            :got (layout))
                    (let ((start (window-start (get-buffer-window article))))
                      (execute-kbd-macro "J")
                      (record "J again scrolls the article forward"
                              (< start (window-start (get-buffer-window article)))
                              :got (format "window start %d, then %d"
                                           start (window-start (get-buffer-window article)))))
                    (let ((start (window-start (get-buffer-window article))))
                      (execute-kbd-macro "K")
                      (record "K scrolls the article back"
                              (< (window-start (get-buffer-window article)) start)
                              :got (format "window start %d, then %d"
                                           start (window-start (get-buffer-window article)))))
                    (execute-kbd-macro "q")
                    (record "q in the summary leaves the window beside Gnus at its width"
                            (and (eq (window-buffer (selected-window)) (get-buffer gnus-group-buffer))
                                 (= width (beside-width)))
                            :got (format "%d wide before, now %s" width (layout)))))
                ;; read and star apart: every message of the starred
                ;; label gets one action, and q saves them all
                (delete-other-windows)
                (switch-to-buffer gnus-group-buffer)
                ;; the label was subscribed after startup merged the
                ;; maildir flags, so only a refresh merges them, the one
                ;; every sync runs
                (execute-kbd-macro (kbd "gR"))
                (gnus-group-jump-to-group "nnmaildir+gmail:starred")
                (execute-kbd-macro (kbd "RET"))
                (let ((summary (current-buffer))
                      (left (email-e2e--article "starred, left alone"))
                      (opened (email-e2e--article "starred, opened"))
                      (shown (email-e2e--article "starred, shown"))
                      (read-opened (email-e2e--article "starred and read, opened"))
                      (read-unread (email-e2e--article "starred and read, marked unread"))
                      (plain (email-e2e--article "unread, starred"))
                      (unstarred (email-e2e--article "starred, unstarred"))
                      (ids '("opened" "shown" "read-opened" "read-unread" "plain" "unstarred")))
                  (cl-flet ((state (article)
                              (with-current-buffer summary
                                (list (line-marks article) (line-star article))))
                            (at (article)
                              (select-window (get-buffer-window summary))
                              (gnus-summary-goto-subject article)))
                    (record "a message starred while unread shows its star and reads as unread"
                            (and (equal (state left) (list "  " star))
                                 (equal (state read-opened) (list " !" star)))
                            :got (format "%S, %S" (state left) (state read-opened)))
                    (at opened)
                    (execute-kbd-macro (kbd "RET"))
                    (execute-kbd-macro "q")
                    (record "RET on a starred unread message reads it and keeps the star"
                            (equal (state opened) (list " !" star))
                            :got (format "%S" (state opened)))
                    (at read-opened)
                    (execute-kbd-macro (kbd "RET"))
                    (execute-kbd-macro "q")
                    (record "RET on a starred read message keeps the star"
                            (equal (state read-opened) (list " !" star))
                            :got (format "%S" (state read-opened)))
                    (at shown)
                    (execute-kbd-macro "J")
                    (record "J on a starred unread message reads it and keeps the star"
                            (equal (state shown) (list " !" star))
                            :got (format "%S" (state shown)))
                    (at read-unread)
                    (execute-kbd-macro "!")
                    (record "! on a starred read message marks it unread and keeps the star"
                            (equal (state read-unread) (list "  " star))
                            :got (format "%S" (state read-unread)))
                    (at plain)
                    (execute-kbd-macro "=")
                    (record "= stars an unread message and leaves it unread"
                            (equal (state plain) (list "  " star))
                            :got (format "%S" (state plain)))
                    (at unstarred)
                    (execute-kbd-macro "=")
                    (record "= unstars a starred unread message and leaves it unread"
                            (equal (state unstarred) (list "  " " "))
                            :got (format "%S" (state unstarred)))
                    (select-window (get-buffer-window summary))
                    (execute-kbd-macro "q"))
                  (record "q leaves a message starred while unread flagged and unread"
                          (equal (flags-of starred "left") "F")
                          :got (format "%S" (flags-of starred "left")))
                  (record "q saves read and star apart, the way Gmail keeps them"
                          (equal (mapcar (lambda (id) (flags-of starred id)) ids)
                                 '("FS" "FS" "FS" "F" "F" ""))
                          :got (format "%S" (mapcar (lambda (id) (flags-of starred id)) ids))))
                ;; the reply keys and the localleader wherever a message is
                ;; read: a list thread answered each way, messages moved
                ;; and copied between labels, the summary folded, narrowed
                ;; and sorted.  What would reach the network is stubbed
                (delete-other-windows)
                (switch-to-buffer gnus-group-buffer)
                (execute-kbd-macro (kbd "gR"))
                (gnus-group-jump-to-group "nnmaildir+gmail:lists")
                (execute-kbd-macro (kbd "RET"))
                (let ((summary (current-buffer))
                      (plan (email-e2e--article "list plan"))
                      (answer (email-e2e--article "Re: list plan"))
                      (to-move (email-e2e--article "to move"))
                      (to-label (email-e2e--article "to label"))
                      (browsed nil)
                      (called nil)
                      (searched nil))
                  (cl-flet* ((at (article)
                               (delete-other-windows)
                               (switch-to-buffer summary)
                               (gnus-summary-goto-subject article))
                             (field (message key)
                               (or (plist-get message key) ""))
                             ;; the message buffer KEYS open, killed once read
                             (composed (keys)
                               (execute-kbd-macro (kbd keys))
                               (if (not (derived-mode-p 'message-mode))
                                   (list :mode major-mode)
                                 ;; a forward carries the original's headers
                                 ;; in its body
                                 (prog1 (append (list :mode major-mode)
                                                (save-restriction
                                                  (message-narrow-to-headers)
                                                  (list :from (message-fetch-field "From")
                                                        :to (message-fetch-field "To")
                                                        :cc (message-fetch-field "Cc")
                                                        :subject (message-fetch-field "Subject")))
                                                (list :body (save-excursion
                                                              (message-goto-body)
                                                              (buffer-substring-no-properties
                                                               (point) (point-max)))))
                                   (set-buffer-modified-p nil)
                                   (kill-buffer (current-buffer)))))
                             (recipients (message)
                               (concat (field message :to) " " (field message :cc)))
                             (hidden-p (article)
                               (with-current-buffer summary
                                 (invisible-p (gnus-data-pos (gnus-data-find article)))))
                             (top-subject ()
                               (with-current-buffer summary
                                 (save-excursion
                                   (goto-char (point-min))
                                   (mail-header-subject (gnus-summary-article-header)))))
                             (queued ()
                               (with-current-buffer summary
                                 (mail-marked-articles 'delete)))
                             ;; the queries of notmuch's last count
                             (counted ()
                               (when (file-exists-p counted-log)
                                 (with-temp-buffer
                                   (insert-file-contents counted-log)
                                   (split-string (buffer-string) "\n" t)))))
                    (cl-letf (((symbol-function 'browse-url)
                               (lambda (url &rest _) (push url browsed)))
                              ((symbol-function 'sync-mail)
                               (lambda (&optional _) (interactive "P") (push 'sync called)))
                              ((symbol-function 'search-mail)
                               (lambda (&optional query)
                                 (interactive)
                                 (push 'search called)
                                 (setq searched query)))
                              ((symbol-function 'open-mail-inbox)
                               (lambda () (interactive) (push 'inbox called)))
                              ;; notmuch is not on CI
                              ((symbol-function 'gnus-summary-refer-thread)
                               (lambda (&rest _) (interactive "P")
                                 (push (list 'fetch (gnus-summary-article-number)) called)))
                              ;; no news server either
                              ((symbol-function 'gnus-summary-followup-with-original)
                               (lambda (&rest _) (interactive "P")
                                 (push (list 'follow-up (gnus-summary-article-number)) called)))
                              ;; Gnus's own group prompt, answered
                              ((symbol-function 'gnus-read-move-group-name)
                               (lambda (prompt &rest _)
                                 (if (equal prompt "Copy")
                                     "nnmaildir+gmail:labelled"
                                   "nnmaildir+gmail:moved"))))
                      (record "the list thread shows its answer starred and unread"
                              (and (= 4 (length gnus-newsgroup-headers))
                                   (memq answer gnus-newsgroup-unreads)
                                   (memq answer gnus-newsgroup-marked)
                                   (not (memq plan gnus-newsgroup-unreads)))
                              :got (format "%d headers, unread %S, starred %S"
                                           (length gnus-newsgroup-headers)
                                           gnus-newsgroup-unreads gnus-newsgroup-marked))
                      (at plan)
                      (execute-kbd-macro (kbd "TAB"))
                      (let ((folded (hidden-p answer)))
                        (execute-kbd-macro (kbd "TAB"))
                        (record "TAB folds the thread at point and unfolds it again"
                                (and folded (not (hidden-p answer)))
                                :got (format "folded %S, then %S" folded (hidden-p answer))))
                      (execute-kbd-macro "za")
                      (let ((folded (hidden-p answer)))
                        (execute-kbd-macro "za")
                        (record "za folds the thread at point and unfolds it again"
                                (and folded (not (hidden-p answer)))
                                :got (format "folded %S, then %S" folded (hidden-p answer))))
                      (execute-kbd-macro "zM")
                      (let ((folded (hidden-p answer)))
                        (execute-kbd-macro "zR")
                        (record "zM folds every thread and zR unfolds them"
                                (and folded (not (hidden-p answer)))
                                :got (format "folded %S, then %S" folded (hidden-p answer))))
                      (execute-kbd-macro (kbd ", n u"))
                      (let ((limit (copy-sequence gnus-newsgroup-limit)))
                        (execute-kbd-macro (kbd ", n w"))
                        (record ", n u narrows the summary to unread mail and , n w widens it again"
                                (and (equal limit (list answer))
                                     (= 4 (length gnus-newsgroup-limit)))
                                :got (format "%S, then %S" limit gnus-newsgroup-limit)))
                      ;; before anything displays the answer, which reads it
                      (at plan)
                      (execute-kbd-macro (kbd ", t r"))
                      (record ", t r marks the whole thread read and keeps the answer's star"
                              (and (not (memq plan gnus-newsgroup-unreads))
                                   (not (memq answer gnus-newsgroup-unreads))
                                   (memq answer gnus-newsgroup-marked))
                              :got (format "unread %S, starred %S"
                                           gnus-newsgroup-unreads gnus-newsgroup-marked))
                      (at plan)
                      (let ((message (composed "r")))
                        (record "r answers the sender only, quoting the message"
                                (and (eq (plist-get message :mode) 'message-mode)
                                     (string-match-p "carol@example\\.com" (field message :to))
                                     (not (string-match-p "dan@\\|emacs-devel@" (recipients message)))
                                     (string-match-p "^> body of list plan" (field message :body)))
                                :got (format "%S" message)))
                      (at plan)
                      (let ((message (composed "R")))
                        (record "R answers the sender and every recipient, quoting the message"
                                (and (string-match-p "carol@example\\.com" (recipients message))
                                     (string-match-p "dan@example\\.com" (recipients message))
                                     (string-match-p "emacs-devel@gnu\\.org" (recipients message))
                                     (not (string-match-p "plotnick\\|agzam" (recipients message)))
                                     (string-match-p "^> body of list plan" (field message :body)))
                                :got (format "%S" message)))
                      (at plan)
                      (let ((message (composed ", r l")))
                        (record ", r l answers the list only"
                                (and (equal (field message :to) "emacs-devel@gnu.org")
                                     (null (plist-get message :cc)))
                                :got (format "%S" message)))
                      (at plan)
                      (let ((message (composed ", f")))
                        (record ", f forwards the message"
                                (and (eq (plist-get message :mode) 'message-mode)
                                     (string-match-p "list plan" (field message :subject))
                                     (string-match-p "body of list plan" (field message :body)))
                                :got (format "%S" message)))
                      (at plan)
                      (let ((message (composed ", c")))
                        (record ", c starts a new message sent as agzam.ibragimov"
                                (and (eq (plist-get message :mode) 'message-mode)
                                     (string-match-p "agzam\\.ibragimov@gmail\\.com"
                                                     (field message :from))
                                     (not (string-match-p "list plan" (field message :subject))))
                                :got (format "%S" message)))
                      (at answer)
                      (execute-kbd-macro (kbd ", u"))
                      (execute-kbd-macro (kbd ", /"))
                      (execute-kbd-macro (kbd ", t f"))
                      (execute-kbd-macro (kbd ", r n"))
                      (record ", u syncs, , / searches, , t f fetches the thread and , r n follows up, at point"
                              (equal (reverse called)
                                     `(sync search (fetch ,answer) (follow-up ,answer)))
                              :got (format "%S" (reverse called)))
                      (setq called nil)
                      (at answer)
                      (execute-kbd-macro (kbd ", o g"))
                      (execute-kbd-macro (kbd ", o l"))
                      (record ", o g opens the message in Gmail and , o l in its list archive"
                              (equal (reverse browsed)
                                     (list (gmail-message-url "<list-plan-dan@fixture.example>")
                                           "https://yhetil.org/emacs-devel/list-plan-dan%40fixture.example"))
                              :got (format "%S" (reverse browsed)))
                      (setq browsed nil)
                      ;; RET takes the first query the prompt offers
                      (at plan)
                      (execute-kbd-macro (kbd ", ? RET"))
                      (record ", ? searches for mail like the message at point, its list first"
                              (and (equal searched "List:\"emacs-devel.gnu.org\"")
                                   (equal (car (counted)) searched)
                                   (member "from:\"carol@example.com\"" (counted)))
                              :got (format "searched %S, counted %S" searched (counted)))
                      (setq searched nil)
                      (at plan)
                      (execute-kbd-macro (kbd "RET"))
                      (execute-kbd-macro (kbd "C-j"))
                      (execute-kbd-macro (kbd ", ? RET"))
                      (record ", ? in the thread view takes the message at point"
                              (and (equal searched "List:\"emacs-devel.gnu.org\"")
                                   (member "from:\"dan@example.com\"" (counted))
                                   (not (member "from:\"carol@example.com\"" (counted))))
                              :got (format "searched %S, counted %S" searched (counted)))
                      (setq called nil
                            searched nil)
                      ;; the thread view acts on the message at point
                      (at plan)
                      (execute-kbd-macro (kbd "RET"))
                      (execute-kbd-macro (kbd "C-j"))
                      (let ((message (composed "r")))
                        (record "r in the thread view answers the message at point"
                                (and (eq (plist-get message :mode) 'message-mode)
                                     (string-match-p "dan@example\\.com" (field message :to))
                                     (string-match-p "^> body of Re: list plan" (field message :body)))
                                :got (format "%S" message)))
                      (at plan)
                      (execute-kbd-macro (kbd "RET"))
                      (let ((view (selected-window)))
                        (execute-kbd-macro "=")
                        (record "= in the thread view stars the message at point and stays in the view"
                                (and (with-current-buffer summary (memq plan gnus-newsgroup-marked))
                                     (eq (selected-window) view)
                                     (derived-mode-p 'mail-thread-mode))
                                :got (format "starred %S, %s"
                                             (with-current-buffer summary gnus-newsgroup-marked)
                                             major-mode))
                        (execute-kbd-macro "=")
                        (record "= again in the thread view unstars it"
                                (not (with-current-buffer summary (memq plan gnus-newsgroup-marked)))
                                :got (format "%S" (with-current-buffer summary gnus-newsgroup-marked)))
                        (execute-kbd-macro "D")
                        (record "D in the thread view queues the whole thread in the summary"
                                (equal (queued) (sort (list plan answer) #'<))
                                :got (format "%S" (queued)))
                        (execute-kbd-macro "U")
                        (record "U in the thread view takes the thread back and marks it unread, the answer's star kept"
                                (and (null (queued))
                                     (with-current-buffer summary
                                       (and (memq plan gnus-newsgroup-unreads)
                                            (memq answer gnus-newsgroup-unreads)
                                            (equal (line-marks answer) "  ")
                                            (equal (line-star answer) star))))
                                :got (with-current-buffer summary
                                       (format "%S, unread %S, answer's line starts %S, star %S"
                                               (queued) gnus-newsgroup-unreads
                                               (line-marks answer) (line-star answer))))
                        (execute-kbd-macro (kbd ", o g"))
                        (record ", o g in the thread view opens the message at point in Gmail"
                                (equal browsed (list (gmail-message-url "<list-plan@fixture.example>")))
                                :got (format "%S" browsed))
                        (setq browsed nil))
                      ;; and so does the article buffer, on its article
                      (at plan)
                      (execute-kbd-macro (kbd "RET"))
                      (execute-kbd-macro (kbd "RET"))
                      (let ((in-article (derived-mode-p 'gnus-article-mode)))
                        (execute-kbd-macro (kbd ", o l"))
                        (let ((message (composed "r")))
                          (record "the article buffer opens its message in the list archive and answers it on r"
                                  (and in-article
                                       (equal browsed
                                              '("https://yhetil.org/emacs-devel/list-plan%40fixture.example"))
                                       (string-match-p "carol@example\\.com" (field message :to))
                                       (string-match-p "^> body of list plan" (field message :body)))
                                  :got (format "article %S, %S, %S" in-article browsed message))))
                      (setq browsed nil)
                      (at plan)
                      (let ((tops (list (top-subject))))
                        ;; a macro starts with no last command, so a second
                        ;; press counts only inside the macro of the first
                        (dolist (keys '(", s d" ", s d , s d" ", s s" ", s s , s s"))
                          (execute-kbd-macro (kbd keys))
                          (push (top-subject) tops))
                        (setq tops (nreverse tops))
                        (record ", s d and , s s sort by date and by subject, the same key again reversing"
                                (equal tops '("to move" "to move" "list plan" "list plan" "to move"))
                                :got (format "%S" tops)))
                      (at to-move)
                      (execute-kbd-macro (kbd ", m"))
                      (record ", m moves the message into another label"
                              (and (equal (message-ids (messages-in moved))
                                          '("<to-move@fixture.example>"))
                                   (not (member "<to-move@fixture.example>"
                                                (message-ids (messages-in lists)))))
                              :got (format "moved %S, lists %S"
                                           (message-ids (messages-in moved))
                                           (message-ids (messages-in lists))))
                      (at to-label)
                      (execute-kbd-macro (kbd ", l"))
                      (record ", l adds a label: a copy lands there and the message stays"
                              (and (equal (message-ids (messages-in labelled))
                                          '("<to-label@fixture.example>"))
                                   (member "<to-label@fixture.example>"
                                           (message-ids (messages-in lists))))
                              :got (format "labelled %S, lists %S"
                                           (message-ids (messages-in labelled))
                                           (message-ids (messages-in lists))))
                      ;; expreg-transient opens with visual state and replays
                      ;; a , it lets through, except inside a keyboard macro,
                      ;; which general assumes recorded the replayed key
                      (at plan)
                      (let ((general--simulate-as-is t))
                        (execute-kbd-macro (kbd "V j , l")))
                      (let ((ids (sort (message-ids (messages-in labelled)) #'string<)))
                        (record ", l on a visual selection labels each message in it and ends visual state"
                                (and (equal ids '("<list-plan-dan@fixture.example>"
                                                  "<list-plan@fixture.example>"
                                                  "<to-label@fixture.example>"))
                                     (eq evil-state 'normal))
                                :got (format "%S, %s state" ids evil-state)))
                      ;; U left the thread unread, and the article view read
                      ;; the root again
                      (at plan)
                      (execute-kbd-macro "q")
                      (record "q saves the root read and the answer starred and unread"
                              (and (equal (flags-of lists "list-plan") "S")
                                   (equal (flags-of lists "list-plan-dan") "F"))
                              :got (format "%S %S" (flags-of lists "list-plan")
                                           (flags-of lists "list-plan-dan")))
                      ;; the group buffer shares the reading buffers' keys
                      (delete-other-windows)
                      (switch-to-buffer gnus-group-buffer)
                      (execute-kbd-macro (kbd ", u"))
                      (execute-kbd-macro (kbd ", /"))
                      (execute-kbd-macro (kbd ", i"))
                      (record "the group buffer's , u syncs, , / searches and , i opens the inbox"
                              (equal (reverse called) '(sync search inbox))
                              :got (format "%S" (reverse called)))
                      (let ((message (composed ", c")))
                        (record "the group buffer's , c starts a new message sent as agzam.ibragimov"
                                (and (eq (plist-get message :mode) 'message-mode)
                                     (string-match-p "agzam\\.ibragimov@gmail\\.com"
                                                     (field message :from)))
                                :got (format "%S" message)))
                      ;; the new-tab templates reach Gnus on G
                      (delete-other-windows)
                      (switch-to-buffer gnus-group-buffer)
                      (let ((tabs (length (tab-bar-tabs))))
                        (execute-kbd-macro (kbd "SPC l t G"))
                        (record "SPC l t G shows the group buffer in a new tab"
                                (and (= (length (tab-bar-tabs)) (1+ tabs))
                                     (eq (window-buffer (selected-window))
                                         (get-buffer gnus-group-buffer)))
                                :got (format "%d tabs, then %d, showing %s" tabs
                                             (length (tab-bar-tabs))
                                             (buffer-name (window-buffer (selected-window)))))
                        (when (< tabs (length (tab-bar-tabs)))
                          (tab-bar-close-tab)))))))
            (error (record "flow signalled" nil :err e)))
        (when (buffer-live-p reply)
          (with-current-buffer reply
            (set-buffer-modified-p nil))
          (kill-buffer reply))
        ;; whatever a key composed before the flow died
        (dolist (buffer (buffer-list))
          (when (eq (buffer-local-value 'major-mode buffer) 'message-mode)
            (with-current-buffer buffer
              (set-buffer-modified-p nil))
            (kill-buffer buffer)))
        ;; the thread view loads with its first use, so its buffer name
        ;; is void when the flow died before reaching it
        (when-let* ((name (bound-and-true-p mail-thread-buffer-name))
                    (thread (get-buffer name)))
          (kill-buffer thread))
        ;; SPC l t G on a running Gnus queues refresh turns
        (when (timerp (bound-and-true-p mail-refresh-timer))
          (cancel-timer mail-refresh-timer))
        (when (gnus-alive-p)
          ;; a live summary makes gnus-group-exit ask whether to update it
          (dolist (name '("*Summary nnmaildir+gmail:inbox*" "*Summary nnmaildir+gmail:html*"
                          "*Summary nnmaildir+gmail:starred*" "*Summary nnmaildir+gmail:lists*"))
            (when-let* ((summary (get-buffer name)))
              (with-current-buffer summary
                (gnus-summary-exit-no-update))))
          (with-current-buffer gnus-group-buffer
            (gnus-group-exit)))
        (when-let* ((beside (get-buffer "*beside*")))
          (kill-buffer beside))
        (delete-other-windows)
        (discard-input)))
    (nreverse results)))

(add-to-list 'e2e-scenarios #'email-e2e)
