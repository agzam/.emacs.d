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
;; the Org compose cases bind its folders before a message buffer saves
(require 'nndraft)
;; , q's case moves the drafts folder by binding these
(require 'nndraft)
;; the feed and news variables live in autoload files no key has loaded yet
(defvar feed-directory)
(defvar feed-fetch-program)
(defvar feed-fetch-timer)
(defvar news-active-file)
(defvar news-fetch-directory)
(defvar news-fetch-script)
(defvar news-fetch-process)
(defvar news-fetched-at)
(defvar reddit-maildir)
(defvar reddit-fetch-directory)
(defvar reddit-fetch-script)
(defvar reddit-fetch-process)
(defvar reddit-fetch-pending)
(defvar reddit-fetched-at)
(defvar reddit-thread-fetch-interval)
(defvar mail-load-script)
(defvar mail-load-directory)
(defvar mail-table-directory)
(defvar mail-load-waiters)
(defvar mail-address-file)
(defvar mail-addresses-read-at)
(defvar mail-label-keys-file)
(defvar mail-label-jobs)
(defvar mail-label-colors)
(defvar mail-label-refresh-timer)
(defvar mbsync-program)
(defvar notmuch-program)
(defvar mail-imap-program)
(defvar mail-sync-config)
(defvar mail-sync-directory)
(defvar mail-sync-process)
(defvar mail-sync-report)
(defvar mail-sync-timer)
(defvar mail-live-preview-name)
(defvar mail-live-history)

(defun email-e2e--write-message (file from subject id &optional date references xref body)
  "Write a minimal RFC 822 message to FILE.
FROM, SUBJECT, ID, DATE, REFERENCES and XREF fill the headers; BODY
follows the body's first line."
  (with-temp-file file
    (insert "From: " from "\n"
            "To: inbox@example.com\n"
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
            "To: inbox@example.com\n"
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

(defconst email-e2e--pixel
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg=="
  "A one-pixel PNG, base64-encoded, for an image in a composed message.")

(defun email-e2e--parts (raw)
  "Each part of the encoded message RAW as (DEPTH TYPE TEXT ID), in order.
A multipart has no TEXT; a leaf's TEXT is decoded by its charset."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    ;; smtpmail sends the separator line as the empty line
    (insert (replace-regexp-in-string
             (concat "^" (regexp-quote mail-header-separator) "$") "" raw t t))
    (let ((handle (mm-dissect-buffer t))
          parts)
      (cl-labels ((walk (h depth)
                    (if (stringp (car h))
                        (progn
                          (push (list depth (substring-no-properties (car h)) nil nil) parts)
                          (dolist (child (cdr h))
                            (walk child (1+ depth))))
                      (push (list depth (mm-handle-media-type h)
                                  (let ((charset (mail-content-type-get (mm-handle-type h) 'charset)))
                                    (with-temp-buffer
                                      (mm-insert-part h)
                                      (if charset
                                          (mm-decode-string (buffer-string) (intern (downcase charset)))
                                        (buffer-string))))
                                  (mm-handle-id h))
                            parts))))
        (walk handle 0))
      (mm-destroy-parts handle)
      (nreverse parts))))

(defun email-e2e--contrast (a b)
  "WCAG contrast ratio of the hex colors A and B, read exactly."
  (cl-flet ((luminance (color)
              (cl-loop for value in (color-values-from-color-spec color)
                       for weight in '(0.2126 0.7152 0.0722)
                       sum (let ((c (/ value 65535.0)))
                             (* weight (if (<= c 0.04045)
                                           (/ c 12.92)
                                         (expt (/ (+ c 0.055) 1.055) 2.4)))))))
    (let ((x (luminance a))
          (y (luminance b)))
      (/ (+ (max x y) 0.05) (+ (min x y) 0.05)))))

(defun email-e2e--colors (html)
  "Every hex text color HTML sets, in order."
  (let ((start 0)
        (colors '()))
    (while (and html (string-match "[^-]color: *\\(#[[:xdigit:]]+\\)" html start))
      (push (match-string 1 html) colors)
      (setq start (match-end 0)))
    (nreverse colors)))

(defun email-e2e--with-foreground (face color fn)
  "Call FN with FACE's foreground COLOR, the old one restored after."
  (let ((old (face-attribute face :foreground)))
    (unwind-protect
        (progn
          (set-face-attribute face nil :foreground color)
          (funcall fn))
      (set-face-attribute face nil :foreground old))))

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

(defun email-e2e--write-news-post (file subject id date &optional references flags)
  "Write to FILE a post of gmane.test with SUBJECT, ID, DATE and REFERENCES.
It carries the List-Id and the dead Archived-At an emacs-devel post of
gmane has.  FLAGS, when given, ends the name as a maildir suffix."
  (with-temp-file (if flags (concat file ":2," flags) file)
    (insert "From: Eli <eli@example.com>\n"
            "Newsgroups: gmane.test\n"
            "List-Id: \"Emacs development discussions.\" <emacs-devel.gnu.org>\n"
            "Subject: " subject "\n"
            "Date: " date "\n"
            "Message-ID: <" id "@fixture.example>\n"
            (if references (concat "References: " references "\n") "")
            "Archived-At: <http://permalink.gmane.org/gmane.test/" id ">\n"
            "\n"
            "body of " subject "\n")))

(defun email-e2e--write-reddit-message (file id date &optional references flags html)
  "Write to FILE a message of r.test shaped as the Reddit fetch writes one.
ID is its Message-ID's local part, REFERENCES its References, FLAGS,
when given, ends the name as a maildir suffix, and HTML, when given,
replaces the body."
  (with-temp-file (if flags (concat file ":2," flags) file)
    (insert "From: \"u/ann\" <ann@reddit.invalid>\n"
            "Subject: " (if references "Re: " "") "Reddit post\n"
            "Date: " date "\n"
            "Message-ID: <" id "@reddit.com>\n"
            (if references
                (format "References: %s\nIn-Reply-To: %s\n"
                        references (car (last (split-string references))))
              "")
            "Archived-At: <https://www.reddit.com/r/test/" id "/>\n"
            "MIME-Version: 1.0\n"
            "Content-Type: text/html; charset=utf-8\n\n"
            (or html (concat "<p>body of " id "</p>")) "\n")))

(defun email-e2e--write-feed (file posts)
  "Write to FILE an Atom feed shaped like Reddit's, holding POSTS.
Each post is (ID TITLE HOUR), published on 26 Sep 2026 at HOUR."
  (with-temp-file file
    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
            "<feed xmlns=\"http://www.w3.org/2005/Atom\">\n"
            "<title>newest submissions : emacs</title>\n"
            "<id>/r/emacs/new/.rss</id>\n"
            "<updated>2026-09-27T02:40:47+00:00</updated>\n")
    (pcase-dolist (`(,id ,title ,hour) posts)
      (insert "<entry>\n"
              "<author><name>/u/alice</name></author>\n"
              "<id>t3_" id "</id>\n"
              "<title>" title "</title>\n"
              "<link href=\"https://www.reddit.com/r/emacs/comments/" id "/\"/>\n"
              "<published>2026-09-26T" hour ":00:00+00:00</published>\n"
              "<updated>2026-09-26T" hour ":00:00+00:00</updated>\n"
              "<content type=\"html\">&lt;p&gt;body of the " id " post&lt;/p&gt;</content>\n"
              "</entry>\n"))
    (insert "</feed>\n")))

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

(defun email-e2e--org-compose (record)
  "Compose in Org through real keys, calling RECORD on each case.
A stand-in keeps what would leave.  The summaries the cases open are
closed whatever happens, so the flow's exit asks nothing."
  (let* ((sent nil)
         (message-send-mail-function (lambda () (setq sent (buffer-string))))
         (message-confirm-send t)
         (pixel (expand-file-name "pixel.png" e2e-work-dir))
         (notes (expand-file-name "notes.txt" e2e-work-dir))
         ;; the drafts a message buffer saves stay in the work dir
         (drafts (file-name-as-directory (expand-file-name "org-drafts" e2e-work-dir)))
         (nndraft-directory drafts)
         (nnmh-directory drafts)
         (nndraft-current-group nil))
    (make-directory (expand-file-name "drafts" drafts) t)
    (let ((coding-system-for-write 'no-conversion))
      (with-temp-file pixel
        (set-buffer-multibyte nil)
        (insert (base64-decode-string email-e2e--pixel))))
    (with-temp-file notes
      (insert "attached notes\n"))
    (cl-flet* ((without-gcc ()
                 (save-excursion
                   (save-restriction
                     (message-narrow-to-headers)
                     (message-remove-header "Gcc"))))
               (reply-in (group subject)
                 (delete-other-windows)
                 (switch-to-buffer gnus-group-buffer)
                 (gnus-group-jump-to-group group)
                 (execute-kbd-macro (kbd "RET"))
                 (gnus-summary-goto-subject (email-e2e--article subject))
                 (execute-kbd-macro "r")
                 (without-gcc)
                 (current-buffer))
               (compose ()
                 (delete-other-windows)
                 (switch-to-buffer gnus-group-buffer)
                 (execute-kbd-macro (kbd ", c"))
                 (without-gcc)
                 (message-goto-to)
                 (insert "bob@example.com")
                 (message-goto-subject)
                 (insert "Org compose")
                 (message-goto-body)
                 (current-buffer))
               (send ()
                 (setq sent nil)
                 ;; C-c C-c alone queues; these cases send at once
                 (execute-kbd-macro (kbd "C-u C-c C-c y"))
                 (and sent (email-e2e--parts sent)))
               (part (parts type)
                 (nth 2 (seq-find (lambda (p) (equal (nth 1 p) type)) parts)))
               (format-line ()
                 (format-mode-line mode-name)))
      (unwind-protect
          (progn
            (let ((reply (reply-in "nnmaildir+gmail:inbox" "fresh")))
              (funcall record "a reply to a plain-text mail leaves as plain text, the mode line says so"
                       (and (eq mail-format 'plain)
                            (string-match-p "Plain" (format-line)))
                       :got (format "%S, %S" mail-format (format-line)))
              (execute-kbd-macro "Noted *with thanks*.\r\r")
              (let* ((parts (send))
                     (text (part parts "text/plain")))
                (funcall record "the plain reply leaves as Markdown, the quote as typed, the signature last"
                         (and (= 1 (length parts))
                              (not (buffer-live-p reply))
                              ;; point lands under the attribution, above the quote
                              (string-match-p "\\`On .*, Someone wrote:\n\nNoted \\*\\*with thanks\\*\\*\\.\n\n> body of fresh$" text)
                              (string-match-p "\n-- \nBest regards,\nAg\n*\\'" text))
                         :got (format "%S" parts))))
            (let ((reply (reply-in "nnmaildir+gmail:html" "html letter")))
              (funcall record "a reply to an HTML mail leaves as HTML, the mode line says so"
                       (and (eq mail-format 'html)
                            (string-match-p "HTML" (format-line)))
                       :got (format "%S, %S" mail-format (format-line)))
              (execute-kbd-macro "Here is *bold* and ~code~.\r\r")
              (let* ((parts (send))
                     (text (part parts "text/plain"))
                     (html (part parts "text/html")))
                (funcall record "the HTML reply leaves as Markdown and HTML, quotes in blockquotes, the signature in both"
                         (and (equal (mapcar #'cadr parts)
                                     '("multipart/alternative" "text/plain" "text/html"))
                              (not (buffer-live-p reply))
                              (string-match-p "^Here is \\*\\*bold\\*\\* and `code`\\.$" text)
                              (string-match-p "\n-- \nBest regards,\nAg\n*\\'" text)
                              (string-match-p "<b>bold</b> and <code>code</code>" html)
                              (string-match-p "<blockquote class=\"gmail_quote\"" html)
                              (string-match-p "<p>-- <br>\nBest regards,<br>\nAg</p>" html))
                         :got (format "%S" parts))))
            (let ((new (compose))
                  (states '()))
              (push (cons mail-format (format-line)) states)
              (execute-kbd-macro (kbd "C-, p"))
              (push (cons mail-format (format-line)) states)
              (execute-kbd-macro (kbd "C-, p"))
              (push (cons mail-format (format-line)) states)
              (setq states (nreverse states))
              (funcall record "a new message starts as HTML, and C-, p flips it to plain text and back"
                       (and (equal (mapcar #'car states) '(html plain html))
                            (string-match-p "HTML" (cdr (nth 0 states)))
                            (string-match-p "Plain" (cdr (nth 1 states)))
                            (string-match-p "HTML" (cdr (nth 2 states))))
                       :got (format "%S" states))
              (insert "* Plan\n"
                      "Some *bold*, /italic/, _under_, ~code~, +gone+ and [[https://gnu.org][a link]], snake_case, café 🎉.\n\n"
                      "- one\n  1. nested\n- [X] done\n\nTerms:\n- term :: meaning\n\n"
                      "| a | b |\n|---+---|\n| 1 | 2 |\n\n"
                      "#+begin_src emacs-lisp\n(message \"hi\")\n#+end_src\n\n"
                      "#+begin_quote\nAn Org quote.\n#+end_quote\n\n"
                      "[[file:" pixel "]]\n\n"
                      "<#part type=\"text/plain\" filename=\"" notes "\" disposition=attachment>\n<#/part>\n")
              ;; a string color that reads at 1.4:1 on the light code block
              (let* ((parts (email-e2e--with-foreground 'font-lock-string-face "#d7d5d1" #'send))
                     (text (part parts "text/plain"))
                     (html (part parts "text/html"))
                     (image (seq-find (lambda (p) (equal (nth 1 p) "image/png")) parts))
                     (missing (seq-remove
                               (lambda (element) (string-match-p element (or html "")))
                               (list "<h2 id=\"[^\"]+\">Plan</h2>" "<b>bold</b>" "<i>italic</i>"
                                     "<u>under</u>" "<code>code</code>" "<del>gone</del>"
                                     "<a href=\"https://gnu.org\">a link</a>" "snake_case" "café 🎉"
                                     "<ul class=\"org-ul\">" "<ol class=\"org-ol\">" "&#x2611; done"
                                     "<dl class=\"org-dl\">" "<table "
                                     "<pre[^>]*class=\"src src-emacs-lisp\"><code>(message <span style=\"color: "
                                     "<blockquote style=\"[^\"]*border-left[^\"]*\">\n<p>\nAn Org quote"
                                     (format "src=\"cid:%s\""
                                             (string-trim (or (nth 3 image) "") "<" ">"))
                                     "<p>-- <br>\nBest regards,<br>\nAg</p>"))))
                (funcall record "a new message carries every Org element in HTML, its image inline, its attachment after"
                         (and (not (buffer-live-p new))
                              (equal (mapcar #'cadr parts)
                                     '("multipart/mixed" "multipart/alternative" "text/plain"
                                       "multipart/related" "text/html" "image/png" "text/plain"))
                              (nth 3 image)
                              (null missing)
                              (equal (nth 2 (car (last parts))) "attached notes\n"))
                         :got (format "missing %S in %S" missing parts))
                (funcall record "its Markdown part renders the same elements"
                         (and text
                              (seq-every-p (lambda (element) (string-match-p element text))
                                           '("^# Plan$" "Some \\*\\*bold\\*\\*, \\*italic\\*, _under_, `code`, ~~gone~~"
                                             "\\[a link\\](https://gnu.org)" "café 🎉" "^- one$" "^- \\[x\\] done$"
                                             "^| a | b |$" "^```emacs-lisp$" "^> An Org quote\\.$" "^pixel\\.png$"
                                             "\n-- \nBest regards,\nAg\n*\\'")))
                         :got (format "%S" text))
                (let* ((start (and html (string-search "<pre" html)))
                       (code (and start (substring html start (string-search "</pre>" html start))))
                       (colors (email-e2e--colors code))
                       (unreadable (seq-filter (lambda (color) (< (email-e2e--contrast color "#f6f8fa") 4.5))
                                               colors)))
                  (funcall record "its code block is light, and every color in it reads at 4.5:1 or better"
                           (and code
                                (string-match-p "\\`<pre style=\"background-color: #f6f8fa;" code)
                                colors
                                (null unreadable))
                           :got (format "unreadable %S in %S" unreadable code)))))
            (let ((new (compose)))
              (insert "Edit *me* here.\n")
              (execute-kbd-macro (kbd "<escape> , o"))
              (let ((org (current-buffer)))
                (funcall record ", o opens the body in an Org buffer"
                         (and (not (eq org new))
                              (derived-mode-p 'org-mode)
                              (bound-and-true-p org-mime-src-mode)
                              (string-match-p "Edit \\*me\\* here\\." (buffer-string)))
                         :got (format "%S in %s: %S" major-mode (buffer-name)
                                      (buffer-substring-no-properties (point-min) (min (point-max) 200))))
                ;; in the message itself, C-c C-c would start a send
                (when (derived-mode-p 'org-mode)
                  (execute-kbd-macro (kbd "C-c C-c")))
                (funcall record "C-c C-c there puts the body back into the message"
                         (and (not (buffer-live-p org))
                              (eq (current-buffer) new)
                              (string-match-p "\\`Edit \\*me\\* here\\.\n" (mail-body)))
                         :got (format "%s: %S" (buffer-name) (mail-body))))
              (setq sent nil)
              (let ((quit (condition-case nil
                              (progn (execute-kbd-macro (kbd "C-u C-c C-c n")) nil)
                            (quit t))))
                (funcall record "n at Send message? leaves the Org body as typed, nothing sent"
                         (and quit
                              (null sent)
                              (buffer-live-p new)
                              (string-match-p "\\`Edit \\*me\\* here\\.\n"
                                              (with-current-buffer new (mail-body))))
                         :got (format "quit %S, sent %S, body %S" quit sent
                                      (and (buffer-live-p new) (with-current-buffer new (mail-body))))))))
        (dolist (buffer (buffer-list))
          (when (memq (buffer-local-value 'major-mode buffer) '(message-mode org-mode))
            (with-current-buffer buffer
              (when (or (derived-mode-p 'message-mode) (bound-and-true-p org-mime-src-mode))
                (set-buffer-modified-p nil)
                (kill-buffer buffer)))))
        (dolist (group '("nnmaildir+gmail:inbox" "nnmaildir+gmail:html"))
          (when-let* ((summary (get-buffer (gnus-summary-buffer-name group))))
            (with-current-buffer summary
              (gnus-summary-exit-no-update))))))))

(defun email-e2e--delayed-send (record)
  "Queue mail through real keys and send it from the queue, calling RECORD.
A stand-in keeps what leaves.  The queue lives in the work dir, and the
buffers, summary and queued files the cases leave are cleared after."
  (let* ((sent '())
         (message-send-mail-function (lambda () (push (buffer-string) sent)))
         (message-confirm-send t)
         ;; the drafts folder , q binds after this, so its queue is this one
         (drafts (file-name-as-directory (expand-file-name "drafts" e2e-work-dir)))
         (nndraft-directory drafts)
         (nnmh-directory drafts)
         (nndraft-current-group nil)
         (queue (expand-file-name "delayed" drafts))
         (mail-queue-retry-time 0)
         (mail-queue-failure nil))
    (make-directory (expand-file-name "drafts" drafts) t)
    (cl-flet* ((numbered (dir)
                 (and (file-directory-p dir)
                      (directory-files dir t "\\`[0-9]+\\'")))
               (queued () (numbered queue))
               (contents (file)
                 (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string)))
               (head (file field)
                 (with-temp-buffer
                   (insert-file-contents file)
                   (goto-char (point-min))
                   (when (search-forward "\n\n" nil t)
                     (narrow-to-region (point-min) (point)))
                   (mail-fetch-field field)))
               (seconds-left (file)
                 (- (float-time (encode-time (parse-time-string (head file "X-Gnus-Delayed"))))
                    (float-time)))
               (backdate (file seconds)
                 (with-temp-file file
                   (insert-file-contents file)
                   (goto-char (point-min))
                   (re-search-forward "^X-Gnus-Delayed: .*$")
                   (replace-match (concat "X-Gnus-Delayed: "
                                          (message-make-date (time-subtract nil seconds)))
                                  t t)))
               (compose (subject)
                 (delete-other-windows)
                 (switch-to-buffer gnus-group-buffer)
                 (execute-kbd-macro (kbd ", c"))
                 (save-excursion
                   (save-restriction
                     (message-narrow-to-headers)
                     (message-remove-header "Gcc")))
                 (message-goto-to)
                 (insert "bob@example.com")
                 (message-goto-subject)
                 (insert subject)
                 (message-goto-body)
                 (current-buffer))
               (new-message-buffers (before)
                 (seq-filter (lambda (buffer)
                               (and (not (memq buffer before))
                                    (eq (buffer-local-value 'major-mode buffer) 'message-mode)))
                             (buffer-list)))
               (warnings ()
                 (if-let* ((buffer (get-buffer "*Warnings*")))
                     (with-current-buffer buffer (buffer-string))
                   "")))
      (unwind-protect
          (condition-case err
              (progn
                (funcall record "Gnus's start puts the queue on its demon, every 10 s after 5 s idle, and on Emacs's quit"
                         (and (member '(send-due-mail 2 1) gnus-demon-handlers)
                              (= gnus-demon-timestep 5)
                              (plist-get gnus-demon-timers 'send-due-mail)
                              (memq 'send-queued-mail-before-quit kill-emacs-query-functions)
                              (memq 'stop-mail-queue gnus-exit-gnus-hook))
                         :got (format "%S, step %S" gnus-demon-handlers gnus-demon-timestep))
                (let ((new (compose "Lunch on Friday")))
                  (insert "Noon works *for me*.\n")
                  (execute-kbd-macro (kbd "C-, p"))
                  (execute-kbd-macro (kbd "C-c C-c"))
                  (let ((files (queued)))
                    (funcall record "C-c C-c queues the message for a minute: its buffer goes, nothing leaves, the Org body waits with its format"
                             (and (not (buffer-live-p new))
                                  (eq (window-buffer) (get-buffer gnus-group-buffer))
                                  (null sent)
                                  (= 1 (length files))
                                  (< 55 (seconds-left (car files)) 65)
                                  (equal (head (car files) "X-Mail-Format") "plain")
                                  (string-match-p "Noon works \\*for me\\*\\." (contents (car files))))
                             :got (format "live %S, window %S, sent %S, queue %S"
                                          (buffer-live-p new) (window-buffer) sent (mapcar #'contents files)))
                    (when files
                      (backdate (car files) 120))
                    (let ((before (buffer-list))
                          (shown (window-buffer)))
                      (send-due-mail)
                      (let* ((raw (car sent))
                             (parts (and raw (email-e2e--parts raw))))
                        (funcall record "a due message leaves in the format it was queued with, its queue headers gone, and the queue forgets it"
                                 (and (= 1 (length sent))
                                      (= 1 (length parts))
                                      (string-match-p "Noon works \\*\\*for me\\*\\*\\." (nth 2 (car parts)))
                                      (not (string-match-p "^X-\\(Mail-Format\\|Gnus-Delayed\\):" raw))
                                      (null (queued))
                                      (null (new-message-buffers before))
                                      (eq (window-buffer) shown))
                                 :got (format "sent %S, parts %S, queue %S, buffers %S, window %S for %S"
                                              sent parts (queued) (new-message-buffers before)
                                              (window-buffer) shown))))))
                (setq sent nil)
                (compose "Second thoughts")
                (insert "First draft.\n")
                (execute-kbd-macro (kbd "C-c C-c"))
                (when (queued)
                  (backdate (car (queued)) 600))
                (delete-other-windows)
                (switch-to-buffer gnus-group-buffer)
                (gnus-group-jump-to-group (mail-queue-group))
                (execute-kbd-macro (kbd "RET"))
                (let ((summary (current-buffer)))
                  (goto-char (point-min))
                  (execute-kbd-macro "e")
                  (let ((editing (current-buffer)))
                    (funcall record "e on a queued message takes it out of the queue and opens its Org body"
                             (and (not (eq editing summary))
                                  (derived-mode-p 'message-mode)
                                  (null (queued))
                                  (string-match-p "First draft\\." (mail-body)))
                             :got (format "%S in %s, queue %S" major-mode (buffer-name) (queued)))
                    (when (derived-mode-p 'message-mode)
                      (message-goto-body)
                      (insert "Second thought.\n")
                      (execute-kbd-macro (kbd "C-c C-c")))
                    (let ((files (queued)))
                      (funcall record "C-c C-c queues the edited message anew: one copy, a fresh minute"
                               (and (not (buffer-live-p editing))
                                    (= 1 (length files))
                                    (< 55 (seconds-left (car files)) 65)
                                    (string-match-p "Second thought\\." (contents (car files)))
                                    (null (numbered (expand-file-name "drafts" drafts))))
                               :got (format "live %S, queue %S, drafts %S" (buffer-live-p editing)
                                            (mapcar #'contents files)
                                            (numbered (expand-file-name "drafts" drafts)))))
                    (send-due-mail)
                    (funcall record "a message queued anew waits out its fresh minute"
                             (and (null sent) (= 1 (length (queued))))
                             :got (format "sent %S, queue %S" sent (queued))))
                  (when (buffer-live-p summary)
                    (with-current-buffer summary
                      (gnus-summary-exit-no-update))))
                (setq sent nil)
                (let ((new (compose "Right away")))
                  (insert "Now.\n")
                  (execute-kbd-macro (kbd "C-u C-c C-c y"))
                  (funcall record "C-u C-c C-c sends at once, past the queue"
                           (and (= 1 (length sent))
                                (not (buffer-live-p new))
                                (= 1 (length (queued))))
                           :got (format "sent %S, queue %S" (length sent) (length (queued)))))
                (setq sent nil)
                (when (queued)
                  (backdate (car (queued)) 120))
                (let ((before (buffer-list)))
                  (let ((message-send-mail-function (lambda () (error "550 refused"))))
                    (send-due-mail))
                  (funcall record "a due message that fails to leave stays queued, leaves no buffer, holds the queue off and warns"
                           (and (null sent)
                                (= 1 (length (queued)))
                                (null (new-message-buffers before))
                                (time-less-p nil mail-queue-retry-time)
                                (string-search "Delayed mail to bob@example.com failed: 550 refused" (warnings)))
                           :got (format "queue %S, buffers %S, retry %S, warnings %S"
                                        (queued) (new-message-buffers before) mail-queue-retry-time (warnings))))
                (send-due-mail)
                (funcall record "after a failure the queue waits out its backoff"
                         (and (null sent) (= 1 (length (queued))))
                         :got (format "sent %S" sent))
                (setq mail-queue-retry-time 0)
                (let* ((message-send-mail-function (lambda ()
                                                     (y-or-n-p "Really send? ")
                                                     (push "answered" sent)))
                       (result (with-timeout (10 'waited)
                                 (send-due-mail)
                                 'returned)))
                  (funcall record "a queued send that would ask a question fails at once instead of waiting"
                           (and (eq result 'returned)
                                (null sent)
                                (= 1 (length (queued)))
                                (string-search "would ask" (warnings)))
                           :got (format "%S, sent %S, warnings %S" result sent (warnings))))
                (setq mail-queue-retry-time 0
                      mail-queue-failure nil
                      sent nil)
                (let ((new (compose "From normal state")))
                  (insert "Bye.\n")
                  (execute-kbd-macro (kbd "<escape> Z Z"))
                  (funcall record "ZZ queues the message the way C-c C-c does"
                           (and (not (buffer-live-p new))
                                (null sent)
                                (= 2 (length (queued))))
                           :got (format "live %S, sent %S, queue %S" (buffer-live-p new) sent (length (queued)))))
                (stop-mail-queue)
                (funcall record "quitting Gnus sends every queued message, due or not, and stops the demon"
                         (and (= 2 (length sent))
                              (null (queued))
                              (not (assq 'send-due-mail gnus-demon-handlers))
                              (not (memq 'send-queued-mail-before-quit kill-emacs-query-functions)))
                         :got (format "sent %S, queue %S, demon %S" (length sent) (queued) gnus-demon-handlers)))
            ;; a quit from a key would end every later scenario of the run
            ((error quit)
             (funcall record "the delayed-send cases run to their end" nil
                      :got (format "%S" err))))
        (dolist (buffer (buffer-list))
          (when (eq (buffer-local-value 'major-mode buffer) 'message-mode)
            (with-current-buffer buffer
              (set-buffer-modified-p nil)
              (let ((kill-buffer-query-functions nil))
                (kill-buffer buffer)))))
        (when-let* ((summary (get-buffer (gnus-summary-buffer-name (mail-queue-group)))))
          (with-current-buffer summary
            (gnus-summary-exit-no-update)))
        (mapc #'delete-file (queued))
        (when (get-buffer "*Warnings*")
          (kill-buffer "*Warnings*"))
        (unless (assq 'send-due-mail gnus-demon-handlers)
          (start-mail-queue))
        (delete-other-windows)
        (switch-to-buffer gnus-group-buffer)))))

(defun email-e2e ()
  "Gnus reads the fixture maildir, replies, and shows a thread in one buffer."
  ;; a probe that owns the server runs the module's timer, whose syncs
  ;; would land in the middle of a case
  (when (timerp (bound-and-true-p mail-sync-timer))
    (cancel-timer mail-sync-timer)
    (setq mail-sync-timer nil))
  (with-timeout (60)
    (while (and (fboundp 'mail-sync-running-p) (mail-sync-running-p))
      (accept-process-output nil 0.1)))
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
         ;; stands in for notmuch, which CI lacks: a search whose query
         ;; names a file under hits/ answers with the copies listed there,
         ;; All Mail's first as the real index names it, and any other
         ;; search with the archive's copy.  It logs each search's
         ;; arguments, and counts 2, 3 ... for a batch, kept in counted-log
         (notmuch (expand-file-name "notmuch" e2e-work-dir))
         (counted-log (expand-file-name "notmuch-counted" e2e-work-dir))
         (hits (expand-file-name "hits/" e2e-work-dir))
         ;; for the live search: the threads a query lists, a thread's
         ;; newest match, and files a search prints whole either way
         (summaries (expand-file-name "summaries/" e2e-work-dir))
         (newest (expand-file-name "newest/" e2e-work-dir))
         (whole (expand-file-name "whole/" e2e-work-dir))
         (searches-log (expand-file-name "notmuch-searches" e2e-work-dir))
         ;; what the %uS column draws on a starred message
         (star (string #x2217))
         (results '())
         ;; r/emacs through the same nnatom method the module gives
         ;; Reddit's feed, downloaded by a curl stand-in that waits for
         ;; go, then delivers served
         (feed (expand-file-name "r-emacs.atom" e2e-work-dir))
         (feed-group (concat "nnatom+" feed ":r/emacs"))
         (feed-directory (expand-file-name "feeds/" e2e-work-dir))
         (feed-fetch-program (expand-file-name "curl" e2e-work-dir))
         (served (expand-file-name "served.atom" e2e-work-dir))
         (go (expand-file-name "go" e2e-work-dir))
         ;; a news group in a store of its own, which a stand-in for the
         ;; batch fetch fills from news-outbox/ once news-go exists
         (news-root (expand-file-name "news/" e2e-work-dir))
         (news-group "nnmaildir+news:gmane.test")
         (news-dir (expand-file-name "gmane.test/" news-root))
         (news-outbox (expand-file-name "news-outbox/" e2e-work-dir))
         (news-go (expand-file-name "news-go" e2e-work-dir))
         (news-maildir news-root)
         (news-server "news.example.org")
         (news-active-file (expand-file-name "news-active" e2e-work-dir))
         (news-fetch-directory (expand-file-name "news-fetch/" e2e-work-dir))
         (news-fetch-script (expand-file-name "news-fetch.el" e2e-work-dir))
         (news-fetch-process nil)
         (news-fetched-at nil)
         ;; a subreddit in a store of its own, which a stand-in for the
         ;; batch fetch fills from reddit-outbox/ once reddit-go exists
         (reddit-root (expand-file-name "reddit/" e2e-work-dir))
         (reddit-group "nnmaildir+reddit:r.test")
         (reddit-dir (expand-file-name "r.test/" reddit-root))
         (reddit-outbox (expand-file-name "reddit-outbox/" e2e-work-dir))
         (reddit-go (expand-file-name "reddit-go" e2e-work-dir))
         (reddit-maildir reddit-root)
         (reddit-fetch-directory (expand-file-name "reddit-fetch/" e2e-work-dir))
         (reddit-fetch-script (expand-file-name "reddit-fetch.el" e2e-work-dir))
         (reddit-fetch-process nil)
         (reddit-fetch-pending nil)
         (reddit-fetched-at nil)
         ;; Gnus shows one message more than display, so the subreddit shows
         ;; its two newest and leaves its post out, the way older posts
         ;; fall out of 500
         (gnus-parameters
          (mapcar (lambda (entry)
                    (if (equal (car entry) "\\`nnmaildir\\+reddit:")
                        `(,(car entry) (display . 1)
                          ,@(assq-delete-all 'display (copy-sequence (cdr entry))))
                      entry))
                  gnus-parameters))
         ;; the real batch reads the groups, held back while load-hold
         ;; exists, and saves its tables here
         (load-hold (expand-file-name "load-hold" e2e-work-dir))
         (mail-load-script (expand-file-name "mail-load.el" e2e-work-dir))
         (mail-load-directory (expand-file-name "mail-load/" e2e-work-dir))
         (mail-table-directory (expand-file-name "mail-tables/" e2e-work-dir))
         ;; stands in for curl as Gmail's IMAP server: it logs each
         ;; request to imap-calls, and a rename or a delete changes the
         ;; boxes gmail-boxes lists.  The label cases read an isyncrc
         ;; whose account commands print no secret
         (mail-imap-program (expand-file-name "imap-curl" e2e-work-dir))
         (imap-calls (expand-file-name "imap-calls" e2e-work-dir))
         (label-config (expand-file-name "isyncrc-labels" e2e-work-dir))
         (removed (expand-file-name "removed/" e2e-work-dir))
         ;; stand in for mbsync and notmuch new: each logs its call to
         ;; sync-calls, mbsync waits while sync-hold exists, and its -ls
         ;; prints gmail-boxes
         (mbsync-program (expand-file-name "mbsync" e2e-work-dir))
         (notmuch-program notmuch)
         (sync-calls (expand-file-name "sync-calls" e2e-work-dir))
         (sync-hold (expand-file-name "sync-hold" e2e-work-dir))
         (gmail-boxes (expand-file-name "gmail-boxes" e2e-work-dir))
         (mail-sync-directory (expand-file-name "sync-state/" e2e-work-dir))
         (mail-sync-process nil)
         (mail-sync-report nil)
         (gnus-search-notmuch-config-file (expand-file-name "notmuch-config" e2e-work-dir))
         ;; the fixtures' mail goes here; custom.el holds the real one
         (mail-inbox-address "inbox@example.com")
         ;; a reply reads no notmuch and writes no saved addresses
         (mail-address-file (expand-file-name "mail-addresses.eld" e2e-work-dir))
         (mail-addresses-read-at (float-time))
         ;; the keys the fixture's labels get stay out of the real state
         (mail-label-keys-file (expand-file-name "mail-label-keys.eld" e2e-work-dir))
         ;; one fixture label in Gmail's colors, the others in the default chip
         (mail-label-colors '(("labelled" "#9b811e" "white")))
         ;; gnus-started-hook subscribes every group under this root;
         ;; the module's mail-groups would add gmane and Reddit, which
         ;; CI cannot reach
         (gmail-maildir root)
         (mail-groups (list feed-group news-group reddit-group))
         ;; html and moved are left out; trash and labelled start empty
         (mail-topics `(("Gmail" ("nnmaildir+gmail:inbox" . "Inbox")
                         ("nnmaildir+gmail:archive" . "All Mail")
                         ("nnmaildir+gmail:trash" . "Trash"))
                        ("Labels" ("nnmaildir+gmail:starred" . "Read and star apart")
                         ("nnmaildir+gmail:labelled"))
                        ("Lists" ("nnmaildir+gmail:lists" . "A list thread")
                         ("nnmaildir+gmail:emacs" . "emacs-devel")
                         (,feed-group . "Reddit, every new post")
                         (,news-group . "A news group")
                         (,reddit-group . "A subreddit"))))
         ;; moved sits with the bulk groups, so , m files into a group
         ;; nnmaildir has not read this session
         (mail-bulk-groups '("nnmaildir+gmail:archive" "nnmaildir+gmail:emacs"
                             "nnmaildir+gmail:moved"))
         (gnus-search-notmuch-program notmuch)
         (gnus-search-notmuch-remove-prefix root)
         (gnus-search-engine-instance-alist nil)
         (gnus-secondary-select-methods
          `((nnmaildir "gmail" (directory ,root) (get-new-mail nil))
            (nnmaildir "news" (directory ,news-root) (get-new-mail nil)
                       (gnus-search-engine gnus-search-notmuch (remove-prefix ,news-root)))
            (nnmaildir "reddit" (directory ,reddit-root) (get-new-mail nil))
            (nnatom ,feed (nnatom-read-title-function ,(lambda (_) "r/emacs"))
                    (nnatom-read-feed-function read-atom-feed))))
         (gnus-startup-file (expand-file-name "newsrc" e2e-work-dir))
         (gnus-init-file (expand-file-name "gnus-init" e2e-work-dir))
         (gnus-directory (expand-file-name "news/" e2e-work-dir))
         ;; an exit closes any summary a dead case left open, unasked
         (gnus-interactive-exit 'quiet)
         (gnus-expert-user t)
         reply)
    (dolist (dir (list inbox trash html starred lists moved labelled archive emacs))
      (dolist (sub '("cur" "new" "tmp"))
        (make-directory (expand-file-name sub dir) t)))
    (make-directory removed t)
    (with-temp-file mail-imap-program
      (insert "#!/bin/sh\n"
              "cat > /dev/null\n"
              "while [ $# -gt 0 ]; do [ \"$1\" = --request ] && request=$2; shift; done\n"
              "printf '%s\\n' \"$request\" >> '" imap-calls "'\n"
              "first=$(printf '%s' \"$request\" | cut -d'\"' -f2)\n"
              "second=$(printf '%s' \"$request\" | cut -d'\"' -f4)\n"
              "case \"$request\" in\n"
              "  RENAME*) { grep -vxF \"$first\" '" gmail-boxes "'; echo \"$second\"; }"
              " > '" gmail-boxes ".new'; mv '" gmail-boxes ".new' '" gmail-boxes "';;\n"
              "  DELETE*) grep -vxF \"$first\" '" gmail-boxes "' > '" gmail-boxes ".new';"
              " mv '" gmail-boxes ".new' '" gmail-boxes "';;\n"
              "  STATUS*) printf '* STATUS \"%s\" (UIDVALIDITY 7)\\r\\n' \"$first\";;\n"
              "esac\n"))
    (set-file-modes mail-imap-program #o755)
    (write-region "IMAPAccount gmail\nHost imap.example.com\nUserCmd \"echo e2e\"\nPassCmd \"echo e2e\"\n"
                  nil label-config)
    (with-temp-file mbsync-program
      (insert "#!/bin/sh\n"
              "printf '%s\\n' \"mbsync $*\" >> '" sync-calls "'\n"
              "while [ -f '" sync-hold "' ]; do sleep 0.05; done\n"
              "case \" $* \" in *\" -ls \"*) cat '" gmail-boxes "';; esac\n"))
    (set-file-modes mbsync-program #o755)
    (write-region "===== gmail-remote:\nINBOX\n[Gmail]/All Mail\n" nil gmail-boxes)
    (email-e2e--write-feed served '(("first" "First fixture post" "10")
                                    ("second" "Second fixture post" "11")))
    (with-temp-file feed-fetch-program
      (insert "#!/bin/sh\n"
              "while [ $# -gt 0 ]; do [ \"$1\" = --output ] && out=$2; shift; done\n"
              "while [ ! -f '" go "' ]; do sleep 0.05; done\n"
              "cp '" served "' \"$out\"\n"))
    (set-file-modes feed-fetch-program #o755)
    ;; the store holds a read post and an unread answer; the stand-in
    ;; for the batch fetch delivers what news-outbox/ holds
    (dolist (sub '("cur" "new" "tmp"))
      (make-directory (expand-file-name sub news-dir) t))
    (make-directory news-outbox t)
    (email-e2e--write-news-post (expand-file-name "cur/1700000040.40.fixture" news-dir)
                                "news post one" "news-one" "Fri, 25 Sep 2026 10:00:00 +0000"
                                nil "S")
    (email-e2e--write-news-post (expand-file-name "cur/1700000041.41.fixture" news-dir)
                                "news post two" "news-two" "Sat, 26 Sep 2026 10:00:00 +0000"
                                "<news-one@fixture.example>" "")
    (email-e2e--write-news-post (expand-file-name "1700000042.42.fixture" news-outbox)
                                "news post three" "news-three" "Sun, 27 Sep 2026 10:00:00 +0000"
                                "<news-one@fixture.example> <news-two@fixture.example>")
    ;; what a fetch saves of the server's group list
    (write-region (concat "gmane.test 0000000003 0000000001 y\n"
                          "gmane.other 0000000005 0000000001 y\n")
                  nil news-active-file)
    (with-temp-file news-fetch-script
      (prin1 `(defun news-fetch-main (store _active _server)
                (while (not (file-exists-p ,news-go))
                  (sleep-for 0.05))
                (dolist (file (directory-files ,news-outbox t "\\`[^.]"))
                  (rename-file file (expand-file-name
                                     (concat "gmane.test/new/" (file-name-nondirectory file))
                                     store)))
                (kill-emacs 0))
             (current-buffer)))
    ;; a read post, a comment on it, a reply to the comment and a second
    ;; comment; the stand-in's fetch brings a reply to the reply.  The
    ;; reply's paragraph and quote are longer than any window is wide
    (dolist (sub '("cur" "new" "tmp"))
      (make-directory (expand-file-name sub reddit-dir) t))
    (make-directory reddit-outbox t)
    (email-e2e--write-reddit-message (expand-file-name "cur/1700000050.50.fixture" reddit-dir)
                                     "p1" "Mon, 28 Sep 2026 10:00:00 +0000" nil "S")
    (email-e2e--write-reddit-message (expand-file-name "cur/1700000051.51.fixture" reddit-dir)
                                     "c1.p1" "Mon, 28 Sep 2026 11:00:00 +0000"
                                     "<p1@reddit.com>" "")
    (email-e2e--write-reddit-message (expand-file-name "cur/1700000052.52.fixture" reddit-dir)
                                     "c2.p1" "Mon, 28 Sep 2026 12:00:00 +0000"
                                     "<p1@reddit.com> <c1.p1@reddit.com>" ""
                                     (concat "<p>long comment "
                                             (string-join (make-list 30 "wrapping") " ")
                                             "</p><blockquote><p>long quote "
                                             (string-join (make-list 30 "quoted") " ")
                                             "</p></blockquote>"))
    (email-e2e--write-reddit-message (expand-file-name "cur/1700000054.54.fixture" reddit-dir)
                                     "c4.p1" "Mon, 28 Sep 2026 14:00:00 +0000"
                                     "<p1@reddit.com>" "")
    (email-e2e--write-reddit-message (expand-file-name "1700000053.53.fixture" reddit-outbox)
                                     "c3.p1" "Mon, 28 Sep 2026 13:00:00 +0000"
                                     "<p1@reddit.com> <c1.p1@reddit.com> <c2.p1@reddit.com>")
    (with-temp-file reddit-fetch-script
      (prin1 `(defun reddit-fetch-main (store _base &optional _threads)
                (while (not (file-exists-p ,reddit-go))
                  (sleep-for 0.05))
                (let ((files (directory-files ,reddit-outbox t "\\`[^.]")))
                  (dolist (file files)
                    (rename-file file (expand-file-name
                                       (concat "r.test/new/" (file-name-nondirectory file))
                                       store)))
                  (message "r.test: 0 posts, %d comments, 0 threads waiting" (length files)))
                (kill-emacs 0))
             (current-buffer)))
    ;; the real batch reader, which waits while load-hold exists
    (with-temp-file mail-load-script
      (insert ";; -*- lexical-binding: t; -*-\n")
      (prin1 `(progn
                (load ,(expand-file-name "scripts/mail-load.el" doom-emacs-dir) nil t)
                (advice-add 'mail-load-main :before
                            (lambda (&rest _)
                              (while (file-exists-p ,load-hold)
                                (sleep-for 0.05)))))
             (current-buffer)))
    ;; the start's batch waits, so the flow acts on groups still loading
    (write-region "" nil load-hold)
    (email-e2e--write-list-message (expand-file-name "cur/1700000031.31.fixture:2,S" emacs)
                                   "Eli <eli@example.com>" "emacs-devel post" "devel-post"
                                   "Sun, 20 Sep 2026 10:00:00 +0000")
    (let ((archived (expand-file-name "cur/1700000030.30.fixture:2,S" archive)))
      (email-e2e--write-message archived "Ann <ann@example.com>" "archived" "archived"
                                "Sun, 20 Sep 2026 09:00:00 +0000")
      (dolist (dir (list hits summaries newest whole))
        (make-directory dir t))
      (with-temp-file notmuch
        (insert "#!/bin/sh\n"
                "case \" $* \" in\n"
                "  *\" new --quiet \"*)\n"
                "    printf '%s\\n' \"notmuch $*\" >> '" sync-calls "';;\n"
                "  *\" count \"*)\n"
                "    : > '" counted-log "'\n"
                "    n=1\n"
                "    while IFS= read -r query; do\n"
                "      printf '%s\\n' \"$query\" >> '" counted-log "'\n"
                "      n=$((n+1)); echo $n\n"
                "    done;;\n"
                "  *\" --output=summary \"*)\n"
                "    for query; do :; done\n"
                "    if [ -f '" summaries "'\"$query\" ]; then cat '" summaries "'\"$query\";"
                " else echo '()'; fi;;\n"
                "  *\" --output=messages \"*)\n"
                "    for query; do :; done\n"
                "    if [ -f '" newest "'\"$query\" ]; then cat '" newest "'\"$query\";"
                " elif [ -f '" hits "'\"$query\" ]; then cat '" hits "'\"$query\"; fi;;\n"
                "  *)\n"
                "    printf '%s\\n' \"$*\" >> '" searches-log "'\n"
                "    for query; do :; done\n"
                "    if [ -f '" whole "'\"$query\" ]; then\n"
                "      cat '" whole "'\"$query\"\n"
                "    elif [ -f '" hits "'\"$query\" ]; then\n"
                "      case \" $* \" in\n"
                "        *\" --duplicate=1 \"*) head -n 1 '" hits "'\"$query\";;\n"
                "        *) cat '" hits "'\"$query\";;\n"
                "      esac\n"
                "    else\n"
                "      printf '%s\\n' '" archived "'\n"
                "    fi;;\n"
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
              ;; a feed download the curl stand-in still holds
              (feed-downloading ()
                (seq-some (lambda (process)
                            (and (string-prefix-p "feed " (process-name process))
                                 (process-live-p process)))
                          (process-list)))
              ;; what pressing KEYS says, which the echo area no longer
              ;; holds once the macro returns
              (saying (keys)
                (let* ((said nil)
                       (note (lambda (format &rest args)
                               (when format
                                 (push (apply #'format-message format args) said)))))
                  (advice-add 'message :before note)
                  (unwind-protect (execute-kbd-macro (kbd keys))
                    (advice-remove 'message note))
                  (nreverse said)))
              ;; every load done, and the work that waited on one; the
              ;; loader yields to input, and a terminal answer the boot
              ;; left unread would hold it back
              (loads-done (&optional seconds)
                (with-timeout ((or seconds 30) nil)
                  (while (or (mail-load-busy-p) mail-load-waiters)
                    (discard-input)
                    (accept-process-output nil 0.05)))
                ;; work that waited ran in a timer, which keeps the
                ;; flow's buffer; the window shows what it opened
                (set-buffer (window-buffer (selected-window)))
                (not (or (mail-load-busy-p) mail-load-waiters)))
              ;; the groups nnmaildir has read this session
              (read-groups ()
                (when-let* ((server (alist-get "gmail" nnmaildir--servers nil nil #'equal)))
                  (sort (hash-table-keys (nnmaildir--srv-groups server)) #'string<)))
              ;; what the curl stand-in was asked, a request a line
              (imap-requests ()
                (when (file-exists-p imap-calls)
                  (with-temp-buffer
                    (insert-file-contents imap-calls)
                    (split-string (buffer-string) "\n" t))))
              ;; a label change, until the sentinel of its last step ran
              (label-change-done ()
                (with-timeout (10 nil)
                  (while (mail-sync-running-p)
                    (accept-process-output nil 0.05)))
                t)
              ;; what the stand-ins for mbsync and notmuch new were asked,
              ;; a line a run
              (sync-runs ()
                (when (file-exists-p sync-calls)
                  (with-temp-buffer
                    (insert-file-contents sync-calls)
                    (split-string (buffer-string) "\n" t))))
              ;; the line a run of the mbsync stand-in on ARGS logs, and
              ;; that of the notmuch one when it indexes
              (mbsync-run (args)
                (format "mbsync -c %s %s" (expand-file-name mail-sync-config) args))
              (notmuch-new ()
                (format "notmuch --config=%s new --quiet"
                        (expand-file-name gnus-search-notmuch-config-file)))
              ;; a sync, FULL or by its markers, run to its end; ACT
              ;; changes the store as mbsync would, while mbsync is held
              (synced (&optional act full)
                (write-region "" nil sync-hold)
                (start-mail-sync full)
                (when act
                  (funcall act))
                (delete-file sync-hold)
                (with-timeout (10 nil)
                  (while (mail-sync-running-p)
                    (accept-process-output nil 0.05)))
                (not (mail-sync-running-p)))
              ;; the group buffer from the top: a topic line as its name,
              ;; a group line as (GROUP TOPIC TEXT)
              (group-lines ()
                (with-current-buffer gnus-group-buffer
                  (save-excursion
                    (goto-char (point-min))
                    (let (lines)
                      (while (not (eobp))
                        (push (or (gnus-group-topic-name)
                                  (list (gnus-group-group-name) (gnus-current-topic)
                                        (buffer-substring-no-properties
                                         (line-beginning-position) (line-end-position))))
                              lines)
                        (forward-line))
                      (nreverse lines)))))
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
              ;; what the %U and %R letters of ARTICLE's line show as,
              ;; once jit-lock has drawn the line
              (mark-symbols (article)
                (save-excursion
                  (gnus-summary-goto-subject article)
                  ;; jit-lock signals on a buffer it was never set up in
                  (when jit-lock-mode
                    (jit-lock-fontify-now (line-beginning-position) (line-end-position)))
                  (mapcar (lambda (pos)
                            (or (get-text-property pos 'display)
                                (string (char-after pos))))
                          (list (+ 1 (line-beginning-position))
                                (+ 2 (line-beginning-position))))))
              ;; the label chips ARTICLE's line shows, as redisplay draws
              ;; them once the redraw a label change asks for has run;
              ;; nil when the line has no slot for labels
              (line-chips (article)
                (with-timeout (2 nil)
                  (while (timerp mail-label-refresh-timer)
                    (accept-process-output nil 0.01)))
                (save-excursion
                  (when (gnus-summary-goto-subject article nil t)
                    (when jit-lock-mode
                      (jit-lock-fontify-now (line-beginning-position) (line-end-position)))
                    (when-let* ((slot (text-property-not-all (line-beginning-position)
                                                             (line-end-position)
                                                             'mail-labels nil)))
                      (or (get-text-property slot 'display) "undrawn")))))
              ;; the %U and %R letters themselves
              (mark-letters (article)
                (save-excursion
                  (gnus-summary-goto-subject article)
                  (buffer-substring-no-properties (+ 1 (line-beginning-position))
                                                  (+ 3 (line-beginning-position)))))
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
                            (count-screen-lines start (point)))))))
              ;; the screen column where the line holding TEXT shows its
              ;; text: LEAD characters in on the first screen line, then
              ;; where each continuation line starts
              (wrap-columns (text lead)
                (save-excursion
                  (goto-char (point-min))
                  (when (search-forward text nil t)
                    (let ((end (line-end-position))
                          (columns (list (car (posn-x-y (posn-at-point
                                                         (+ (line-beginning-position) lead)))))))
                      (goto-char (line-beginning-position))
                      (while (and (= 1 (vertical-motion 1)) (< (point) end))
                        (push (car (posn-x-y (posn-at-point))) columns))
                      (nreverse columns))))))
      (unwind-protect
          (condition-case e
              (progn
                (discard-input)
                ;; the start reads no maildir: a batch Emacs reads every
                ;; group, held back while load-hold exists
                (gnus)
                (record "gnus shows the group buffer before reading any maildir"
                        (and (gnus-alive-p) (null (read-groups)))
                        :got (format "alive %s, read %S" (gnus-alive-p) (read-groups)))
                (gnus-group-jump-to-group "nnmaildir+gmail:emacs")
                (let* ((start (float-time))
                       (said (saying "RET")))
                  (record "RET on a group still loading returns at once, saying it loads"
                          (and (< (- (float-time) start) 0.5)
                               (derived-mode-p 'gnus-group-mode)
                               (seq-some (lambda (line)
                                           (string-match-p
                                            "\\`Loading .* to open emacs\\.\\.\\.\\'" line))
                                         said))
                          :got (format "%.2f s, %s, %S" (- (float-time) start) major-mode said)))
                ;; loads run on plain timers and a process filter, which a
                ;; timed wait serves; `email-e2e--idle' would leave Emacs
                ;; idle, and the thread fill's cases below need it busy
                (delete-file load-hold)
                (loads-done)
                (record "the group opens once its load is done, Gnus still on screen"
                        (and (derived-mode-p 'gnus-summary-mode)
                             (equal gnus-newsgroup-name "nnmaildir+gmail:emacs"))
                        :got (format "%s in %s" major-mode gnus-newsgroup-name))
                (when (derived-mode-p 'gnus-summary-mode)
                  (gnus-summary-exit-no-update))
                (record "every maildir group is read in the background after the start"
                        (equal (read-groups)
                               '("archive" "emacs" "html" "inbox" "labelled" "lists" "moved"
                                 "starred" "trash"))
                        :got (format "%S" (read-groups)))
                (record "each group's line counts its unread mail once it is read"
                        (and (eql (gnus-group-unread "nnmaildir+gmail:inbox") 2)
                             (eql (gnus-group-unread "nnmaildir+gmail:archive") 0))
                        :got (format "inbox %S, archive %S"
                                     (gnus-group-unread "nnmaildir+gmail:inbox")
                                     (gnus-group-unread "nnmaildir+gmail:archive")))
                (record "the batch saves a table of each group it reads"
                        (file-exists-p (expand-file-name "gmail/archive" mail-table-directory))
                        :got (format "%S" (and (file-directory-p mail-table-directory)
                                               (directory-files-recursively
                                                mail-table-directory ""))))
                ;; what the group buffer lists once the turns have read
                ;; the routine groups
                (with-current-buffer gnus-group-buffer
                  (gnus-group-list-groups))
                (let* ((lines (group-lines))
                       (layout (mapcar (lambda (line)
                                         (if (stringp line) line (cons (car line) (cadr line))))
                                       lines))
                       (groups (seq-remove #'stringp lines))
                       (inbox (nth 2 (assoc "nnmaildir+gmail:inbox" groups))))
                  (record "the start puts each group mail-topics names under its topic, in order"
                          ;; moved, read empty like html, is left out too
                          (equal layout
                                 `("Gnus"
                                   "Gmail" ("nnmaildir+gmail:inbox" . "Gmail")
                                   ("nnmaildir+gmail:archive" . "Gmail")
                                   ("nnmaildir+gmail:trash" . "Gmail")
                                   "Labels" ("nnmaildir+gmail:starred" . "Labels")
                                   ("nnmaildir+gmail:labelled" . "Labels")
                                   "Lists" ("nnmaildir+gmail:lists" . "Lists")
                                   ("nnmaildir+gmail:emacs" . "Lists")
                                   (,feed-group . "Lists")
                                   (,news-group . "Lists")
                                   (,reddit-group . "Lists")
                                   "misc"))
                          :got (format "%S" layout))
                  ;; Gnus subscribed them into the root topic
                  (record "the start files the labels mail-topics leaves out under Labels"
                          (equal (list (gnus-group-topic "nnmaildir+gmail:html")
                                       (gnus-group-topic "nnmaildir+gmail:moved"))
                                 '("Labels" "Labels"))
                          :got (format "%S" gnus-topic-alist))
                  (record "a group line shows the bare name and the description mail-topics gives"
                          (and inbox (string-match-p "\\` +2 +inbox +Inbox\\'" inbox))
                          :got (format "%S" inbox))
                  ;; html's two messages are read, and labelled holds none
                  (record "a group mail-topics names is listed with nothing unread, another is not"
                          (and (eql (gnus-group-unread "nnmaildir+gmail:labelled") 0)
                               (assoc "nnmaildir+gmail:labelled" groups)
                               (eql (gnus-group-unread "nnmaildir+gmail:html") 0)
                               (not (assoc "nnmaildir+gmail:html" groups)))
                          :got (format "labelled %S, html %S: %S"
                                       (gnus-group-unread "nnmaildir+gmail:labelled")
                                       (gnus-group-unread "nnmaildir+gmail:html")
                                       (mapcar #'car groups)))
                  ;; the stand-in holds the start's download, so the group
                  ;; is still unread while the start goes on
                  (record "the start downloads the feed without waiting for it"
                          (and (eql (gnus-group-level feed-group) (1+ gnus-activate-level))
                               (feed-downloading)
                               (null (gnus-active feed-group)))
                          :got (format "level %S, downloading %S, active %S"
                                       (gnus-group-level feed-group) (feed-downloading)
                                       (gnus-active feed-group)))
                  (let ((line (nth 2 (assoc feed-group groups))))
                    (record "the feed's line reads r/emacs and its description"
                            (and line (string-match-p "\\` +\\* +r/emacs +Reddit, every new post\\'"
                                                      line))
                            :got (format "%S" line)))
                  ;; the stand-in holds the start's fetch too, which waits
                  ;; for the batch that read the news group
                  (record "the start fetches news without waiting for it, once its group is read"
                          (and (process-live-p news-fetch-process)
                               (eql (gnus-group-level news-group) (1+ gnus-activate-level))
                               (gnus-active news-group))
                          :got (format "fetching %S, level %S, active %S"
                                       (process-live-p news-fetch-process)
                                       (gnus-group-level news-group) (gnus-active news-group))))
                (write-region "" nil go)
                (with-timeout (10)
                  (while (not (gnus-active feed-group))
                    (accept-process-output nil 0.05)))
                (record "the finished download reads the feed's group"
                        (eql (gnus-group-unread feed-group) 2)
                        :got (format "unread %S" (gnus-group-unread feed-group)))
                (gnus-group-jump-to-group feed-group)
                (execute-kbd-macro (kbd "RET"))
                (record "RET on the feed's line shows its posts"
                        (and (derived-mode-p 'gnus-summary-mode)
                             (equal (sort (mapcar #'mail-header-subject gnus-newsgroup-headers)
                                          #'string<)
                                    '("First fixture post" "Second fixture post")))
                        :got (format "%s: %S" major-mode
                                     (mapcar #'mail-header-subject gnus-newsgroup-headers)))
                (when (derived-mode-p 'gnus-summary-mode)
                  (gnus-summary-exit-no-update))
                ;; , u downloads the feed again and returns before it arrives
                (delete-file go)
                (email-e2e--write-feed served '(("first" "First fixture post" "10")
                                                ("second" "Second fixture post" "11")
                                                ("third" "Third fixture post" "12")))
                (let ((start (float-time)))
                  (execute-kbd-macro (kbd ", u"))
                  (record ", u returns while the feed is still downloading"
                          (and (< (- (float-time) start) 0.5) (feed-downloading))
                          :got (format "%.2f s, downloading %S"
                                       (- (float-time) start) (feed-downloading))))
                (write-region "" nil go)
                (with-timeout (10)
                  (while (not (eql (gnus-group-unread feed-group) 3))
                    (accept-process-output nil 0.05)))
                (record ", u's download counts the feed's new post"
                        (eql (gnus-group-unread feed-group) 3)
                        :got (format "unread %S" (gnus-group-unread feed-group)))
                ;; a news group shows its newest posts, read ones included
                (gnus-group-jump-to-group news-group)
                (execute-kbd-macro (kbd "RET"))
                (record "RET on the news group shows its newest posts, the read ones too"
                        (and (derived-mode-p 'gnus-summary-mode)
                             (equal (sort (mapcar #'mail-header-subject gnus-newsgroup-headers)
                                          #'string<)
                                    '("news post one" "news post two")))
                        :got (format "%s: %S" major-mode
                                     (mapcar #'mail-header-subject gnus-newsgroup-headers)))
                ;; the first entry reads the flags a fetch delivered
                (record "the first entry into the news group shows a post the fetch delivered read as read"
                        (and (derived-mode-p 'gnus-summary-mode)
                             (equal (mapcar (lambda (article)
                                              (mail-header-subject
                                               (gnus-summary-article-header article)))
                                            gnus-newsgroup-unreads)
                                    '("news post two")))
                        :got (format "unread %S"
                                     (and (derived-mode-p 'gnus-summary-mode)
                                          (mapcar (lambda (article)
                                                    (mail-header-subject
                                                     (gnus-summary-article-header article)))
                                                  gnus-newsgroup-unreads))))
                (record "a news group's lines leave no room for Gmail's labels"
                        (and (derived-mode-p 'gnus-summary-mode)
                             (null (text-property-not-all (point-min) (point-max)
                                                          'mail-labels nil)))
                        :got (buffer-substring-no-properties (point-min) (point-max)))
                ;; the post's Archived-At points into the dead permalink.gmane.org
                (let (browsed)
                  (when (derived-mode-p 'gnus-summary-mode)
                    (gnus-summary-goto-subject (email-e2e--article "news post one"))
                    (cl-letf (((symbol-function 'browse-url)
                               (lambda (url &rest _) (push url browsed))))
                      (execute-kbd-macro (kbd "C-c C-o"))
                      (execute-kbd-macro (kbd ", o l"))))
                  (record "C-c C-o on a gmane post of an Emacs list opens its yhetil thread"
                          (equal (car (last browsed))
                                 "https://yhetil.org/emacs-devel/news-one%40fixture.example/t/#u")
                          :got (format "%S" (reverse browsed)))
                  (record ", o l on a gmane post opens its yhetil page, not its dead Archived-At"
                          (and (= 2 (length browsed))
                               (equal (car browsed)
                                      "https://yhetil.org/emacs-devel/news-one%40fixture.example/"))
                          :got (format "%S" (reverse browsed))))
                (when (derived-mode-p 'gnus-summary-mode)
                  (gnus-summary-exit-no-update))
                ;; the start's fetch delivers once go exists, into a
                ;; group Gnus read, so its line counts the new post
                (write-region "" nil news-go)
                (with-timeout (15)
                  (while (not (eql (gnus-group-unread news-group) 2))
                    (accept-process-output nil 0.05)))
                (record "a finished fetch brings its posts into the news group Gnus read"
                        (eql (gnus-group-unread news-group) 2)
                        :got (format "unread %S, fetching %S" (gnus-group-unread news-group)
                                     (process-live-p news-fetch-process)))
                ;; , u fetches news again and returns before it arrives
                (delete-file news-go)
                (email-e2e--write-news-post (expand-file-name "1700000043.43.fixture" news-outbox)
                                            "news post four" "news-four"
                                            "Sun, 27 Sep 2026 12:00:00 +0000")
                (let ((start (float-time)))
                  (execute-kbd-macro (kbd ", u"))
                  (record ", u returns while news is still being fetched"
                          (and (< (- (float-time) start) 0.5)
                               (process-live-p news-fetch-process))
                          :got (format "%.2f s, fetching %S" (- (float-time) start)
                                       (process-live-p news-fetch-process))))
                (write-region "" nil news-go)
                (with-timeout (15)
                  (while (not (eql (gnus-group-unread news-group) 3))
                    (accept-process-output nil 0.05)))
                (record ", u's fetch counts the news group's new post"
                        (eql (gnus-group-unread news-group) 3)
                        :got (format "unread %S" (gnus-group-unread news-group)))
                ;; , u syncs mail too, and returns while mbsync is held
                (let ((unread (gnus-group-unread "nnmaildir+gmail:inbox"))
                      (fetched (expand-file-name "fetched/" root))
                      (synced-file (expand-file-name "new/1700000091.91.fixture" inbox))
                      (start (float-time)))
                  (write-region "" nil sync-hold)
                  (execute-kbd-macro (kbd ", u"))
                  (record ", u returns while mbsync still syncs"
                          (and (< (- (float-time) start) 0.5) (mail-sync-running-p) t)
                          :got (format "%.2f s, syncing %S" (- (float-time) start)
                                       (mail-sync-running-p)))
                  ;; meanwhile mbsync brings a message to the inbox and a
                  ;; label made in Gmail, and writes both state files
                  (dolist (sub '("cur" "new" "tmp"))
                    (make-directory (expand-file-name sub fetched) t))
                  (email-e2e--write-message (expand-file-name "cur/1700000090.90.fixture:2," fetched)
                                            "Kim <kim@example.com>" "labelled in Gmail" "fetched")
                  (write-region "" nil (expand-file-name ".mbsyncstate" fetched))
                  (email-e2e--write-message synced-file "Kim <kim@example.com>" "synced" "synced")
                  (write-region "" nil (expand-file-name ".mbsyncstate" inbox))
                  (delete-file sync-hold)
                  (with-timeout (10)
                    (while (mail-sync-running-p)
                      (accept-process-output nil 0.05)))
                  (loads-done)
                  (record "the sync ends on its own, and reads the inbox again and the label it brought"
                          (and (not (mail-sync-running-p))
                               (eql (gnus-group-unread "nnmaildir+gmail:inbox") (1+ unread))
                               (member "fetched" (read-groups))
                               (eql (gnus-group-unread "nnmaildir+gmail:fetched") 1))
                          :got (format "syncing %S, inbox %S then %S, read %S, fetched %S"
                                       (mail-sync-running-p) unread
                                       (gnus-group-unread "nnmaildir+gmail:inbox") (read-groups)
                                       (gnus-group-unread "nnmaildir+gmail:fetched")))
                  ;; the feed's , u found no marker, the later ones fresh ones
                  (record "the first sync lists Gmail's labels and syncs every box, a later one the inbox, through the module's isyncrc"
                          (and (equal (seq-take (sync-runs) 3)
                                      (list (mbsync-run "-ls gmail-remote") (mbsync-run "fast bulk")
                                            (notmuch-new)))
                               (equal (last (sync-runs) 2) (list (mbsync-run "inbox") (notmuch-new)))
                               (file-exists-p (expand-file-name "last-fast" mail-sync-directory))
                               (file-exists-p (expand-file-name "last-full" mail-sync-directory)))
                          :got (format "%S" (sync-runs)))
                  (record "the first sync writes the notmuch config, over the stores' root"
                          (and (file-exists-p gnus-search-notmuch-config-file)
                               (with-temp-buffer
                                 (insert-file-contents gnus-search-notmuch-config-file)
                                 (string-prefix-p (format "[database]\nmail_root=%s\n"
                                                          (directory-file-name e2e-work-dir))
                                                  (buffer-string))))
                          :got (format "%S" (and (file-exists-p gnus-search-notmuch-config-file)
                                                 (with-temp-buffer
                                                   (insert-file-contents
                                                    gnus-search-notmuch-config-file)
                                                   (buffer-string)))))
                  ;; Gmail lists no such label any more, and the phone
                  ;; archived the message, which the read moved to cur/; a
                  ;; full sync takes both out
                  (synced (lambda ()
                            (dolist (file (messages-in inbox))
                              (when (string-prefix-p (file-name-nondirectory synced-file)
                                                     (file-name-nondirectory file))
                                (delete-file file)))
                            (write-region "" nil (expand-file-name ".mbsyncstate" inbox)))
                          t)
                  (loads-done)
                  (record "a full sync sets aside the folder of a label Gmail dropped, and its group goes"
                          (and (not (file-exists-p fetched))
                               (directory-files (expand-file-name "removed/" mail-sync-directory)
                                                nil "\\`fetched-")
                               (not (gnus-group-entry "nnmaildir+gmail:fetched"))
                               (equal (last (sync-runs) 3)
                                      (list (mbsync-run "-ls gmail-remote") (mbsync-run "fast bulk")
                                            (notmuch-new))))
                          :got (format "folder %S, entry %S, runs %S" (file-exists-p fetched)
                                       (and (gnus-group-entry "nnmaildir+gmail:fetched") t)
                                       (last (sync-runs) 3)))
                  (record "the sync reads the inbox again without the message mbsync took out"
                          (eql (gnus-group-unread "nnmaildir+gmail:inbox") unread)
                          :got (format "inbox %S, was %S" (gnus-group-unread "nnmaildir+gmail:inbox")
                                       unread)))
                ;; a Reddit post and its comments read as one thread, each
                ;; under what it answers, the post heading it from outside
                ;; the window
                (gnus-group-jump-to-group reddit-group)
                (execute-kbd-macro (kbd "RET"))
                (let ((levels (and (derived-mode-p 'gnus-summary-mode)
                                   (sort (mapcar (lambda (data)
                                                   (cons (mail-header-id (gnus-data-header data))
                                                         (gnus-data-level data)))
                                                 gnus-newsgroup-data)
                                         (lambda (a b)
                                           (or (< (cdr a) (cdr b))
                                               (and (= (cdr a) (cdr b))
                                                    (string< (car a) (car b)))))))))
                  (record "RET on the Reddit group threads each comment under the one it answers, under its post"
                          (equal levels '(("<p1@reddit.com>" . 0) ("<c1.p1@reddit.com>" . 1)
                                          ("<c4.p1@reddit.com>" . 1) ("<c2.p1@reddit.com>" . 2)))
                          :got (format "%s: %S" major-mode levels)))
                (let (opened)
                  (when (derived-mode-p 'gnus-summary-mode)
                    (gnus-summary-goto-subject
                     (gnus-data-number
                      (seq-find (lambda (data)
                                  (equal (mail-header-id (gnus-data-header data))
                                         "<c2.p1@reddit.com>"))
                                gnus-newsgroup-data)))
                    (cl-letf (((symbol-function 'browse-url)
                               (lambda (url &rest _) (setq opened url))))
                      (execute-kbd-macro (kbd ", o l"))))
                  (record ", o l on a Reddit message opens it where its Archived-At points"
                          (equal opened "https://www.reddit.com/r/test/c2.p1/")
                          :got (format "%S" opened)))
                (when (derived-mode-p 'gnus-summary-mode)
                  (gnus-summary-exit-no-update))
                ;; the start's fetch delivers once reddit-go exists, and
                ;; its end reads the group again
                (write-region "" nil reddit-go)
                (with-timeout (15)
                  (while (not (eql (gnus-group-unread reddit-group) 4))
                    (accept-process-output nil 0.05)))
                (record "a finished Reddit fetch brings its comments into the group Gnus read"
                        (eql (gnus-group-unread reddit-group) 4)
                        :got (format "unread %S, fetching %S" (gnus-group-unread reddit-group)
                                     (process-live-p reddit-fetch-process)))
                ;; the thread view lays the thread out as Reddit does, each
                ;; comment under the one it answers, the summary's newest
                ;; subthread first notwithstanding
                (gnus-group-jump-to-group reddit-group)
                (execute-kbd-macro (kbd "RET"))
                (when (derived-mode-p 'gnus-summary-mode)
                  (gnus-summary-goto-subject
                   (gnus-data-number
                    (seq-find (lambda (data)
                                (equal (mail-header-id (gnus-data-header data))
                                       "<c2.p1@reddit.com>"))
                              gnus-newsgroup-data)))
                  (execute-kbd-macro (kbd "RET")))
                (redisplay t)
                (let ((layout (and (derived-mode-p 'mail-thread-mode)
                                   (mapcar (lambda (message)
                                             (list (mail-header-id (mail-thread-message-header message))
                                                   (when-let* ((guides (get-text-property
                                                                        (mail-thread-message-marker message)
                                                                        'line-prefix)))
                                                     (substring-no-properties guides))))
                                           mail-thread-messages))))
                  (record "RET on a Reddit comment lists its thread in reply order, a guide bar a level"
                          (equal layout '(("<p1@reddit.com>" nil)
                                          ("<c1.p1@reddit.com>" "│ ")
                                          ("<c2.p1@reddit.com>" "│ │ ")
                                          ("<c3.p1@reddit.com>" "│ │ │ ")
                                          ("<c4.p1@reddit.com>" "│ ")))
                          :got (format "%s: %S" major-mode layout)))
                ;; the post is read, the way the fetch delivers an old one.
                ;; Waiting for the fill would leave Emacs idle, and the
                ;; inbox fill's cases below need it busy
                (let ((queued (and (derived-mode-p 'mail-thread-mode)
                                   (mapcar (lambda (message)
                                             (mail-header-id (mail-thread-message-header message)))
                                           mail-thread-waiting))))
                  (record "RET on a Reddit comment leaves every other message to the fill, its read post too"
                          (equal (sort (copy-sequence queued) #'string<)
                                 '("<c1.p1@reddit.com>" "<c3.p1@reddit.com>"
                                   "<c4.p1@reddit.com>" "<p1@reddit.com>"))
                          :got (format "%S" queued)))
                (let ((bars (string-width (mail-thread-guides 2)))
                      (plain (wrap-columns "long comment" 0))
                      (quoted (wrap-columns "long quote" 2)))
                  (record "a long line two deep wraps right of its two guide bars"
                          (and (< 1 (length plain))
                               (seq-every-p (lambda (column) (eql column bars)) plain))
                          :got (format "columns %S, bars %d wide, wrap-prefix %S" plain bars
                                       (save-excursion
                                         (goto-char (point-min))
                                         (and (search-forward "long comment" nil t)
                                              (get-text-property (point) 'wrap-prefix)))))
                  (record "a long quote two deep wraps right of its > marker"
                          (and (< 1 (length quoted))
                               (seq-every-p (lambda (column) (eql column (+ bars 2))) quoted))
                          :got (format "columns %S, bars %d wide" quoted bars)))
                (let (opened)
                  (when (derived-mode-p 'mail-thread-mode)
                    (cl-letf (((symbol-function 'browse-url)
                               (lambda (url &rest _) (setq opened url))))
                      (execute-kbd-macro (kbd "C-c C-o"))))
                  (record "C-c C-o in the thread view opens the Reddit comment at point on its page"
                          (equal opened "https://www.reddit.com/r/test/c2.p1/")
                          :got (format "%S in %s" opened major-mode)))
                ;; the real column is wider than this window, so the
                ;; thread opens again on a narrow one
                (when (derived-mode-p 'mail-thread-mode)
                  (execute-kbd-macro "q"))
                (set-buffer (window-buffer (selected-window)))
                (when (derived-mode-p 'gnus-summary-mode)
                  (let ((mail-thread-width 20))
                    (execute-kbd-macro (kbd "RET"))))
                ;; the harness inhibits redisplay, whose window change
                ;; hooks make this call
                (when (derived-mode-p 'mail-thread-mode)
                  (visual-fill-column-adjust))
                (let* ((window (get-buffer-window mail-thread-buffer-name))
                       (margins (and window (window-margins window))))
                  (record "the thread view wraps at its column, a right margin taking the rest of the window"
                          (and window (memq (car margins) '(nil 0)) (cdr margins)
                               (eql (window-body-width window) 32))
                          :got (format "margins %S, %S of %S columns" margins
                                       (and window (window-body-width window))
                                       (and window (window-total-width window)))))
                (let* ((start (and (derived-mode-p 'mail-thread-mode)
                                   (save-excursion
                                     (goto-char (point-min))
                                     (and (search-forward "long comment" nil t)
                                          (match-beginning 0)))))
                       (words (when start
                                (save-excursion
                                  (goto-char start)
                                  (let (found)
                                    (while (re-search-forward "\\w+" (pos-eol) t)
                                      (push (match-beginning 0) found))
                                    (nreverse found)))))
                       visited row)
                  (when start
                    (goto-char start)
                    (dotimes (_ (1- (length words)))
                      (execute-kbd-macro "w")
                      (push (point) visited))
                    (setq visited (nreverse visited)))
                  (record "w walks a wrapped paragraph word by word, from each screen line on to the next"
                          (and words
                               (< 2 (count-screen-lines start (car (last words))))
                               (equal visited (cdr words)))
                          :got (format "words at %S, w stopped at %S" words visited))
                  (when start
                    (goto-char start)
                    (setq row (cdr (posn-x-y (posn-at-point))))
                    (execute-kbd-macro "j"))
                  (record "j moves down one screen line of a wrapped paragraph"
                          (and row
                               (< start (point) (car (last words)))
                               (eql (cdr (posn-x-y (posn-at-point))) (1+ row)))
                          :got (format "row %S, then %S at %S" row
                                       (cdr (posn-x-y (posn-at-point))) (point))))
                ;; Reddit has a reply to c4 the store lacks, which the
                ;; thread's opening fetches; its first opening a moment ago
                ;; still counts, so the interval is lifted
                (email-e2e--write-reddit-message (expand-file-name "1700000055.55.fixture"
                                                                   reddit-outbox)
                                                 "c5.p1" "Mon, 28 Sep 2026 15:00:00 +0000"
                                                 "<p1@reddit.com> <c4.p1@reddit.com>")
                (when (derived-mode-p 'mail-thread-mode)
                  (execute-kbd-macro "q"))
                (set-buffer (window-buffer (selected-window)))
                (when (derived-mode-p 'gnus-summary-mode)
                  (let ((reddit-thread-fetch-interval 0))
                    (execute-kbd-macro (kbd "RET"))))
                (cl-flet ((layout ()
                            (with-current-buffer mail-thread-buffer-name
                              (mapcar (lambda (message)
                                        (cons (mail-header-id (mail-thread-message-header message))
                                              (mail-thread-message-depth message)))
                                      mail-thread-messages))))
                  ;; the fetch, the read of its folder and the summary
                  ;; run on processes and plain timers, which a timed
                  ;; wait serves without leaving Emacs idle
                  (with-timeout (20)
                    (while (not (assoc "<c5.p1@reddit.com>" (layout)))
                      (accept-process-output nil 0.05)))
                  (record "RET on a Reddit thread fetches its newest reply and lays it out under its parent"
                          (and (equal (layout)
                                      '(("<p1@reddit.com>" . 0) ("<c1.p1@reddit.com>" . 1)
                                        ("<c2.p1@reddit.com>" . 2) ("<c3.p1@reddit.com>" . 3)
                                        ("<c4.p1@reddit.com>" . 1) ("<c5.p1@reddit.com>" . 2)))
                               (with-current-buffer mail-thread-buffer-name
                                 (equal (mail-header-id (mail-thread-message-header
                                                         (mail-thread-message-at-point)))
                                        "<c2.p1@reddit.com>")))
                          :got (format "%S, at %S" (layout)
                                       (with-current-buffer mail-thread-buffer-name
                                         (mail-header-id (mail-thread-message-header
                                                          (mail-thread-message-at-point)))))))
                (when (derived-mode-p 'mail-thread-mode)
                  (execute-kbd-macro "q"))
                (set-buffer (window-buffer (selected-window)))
                (when (derived-mode-p 'gnus-summary-mode)
                  (gnus-summary-exit-no-update))
                ;; the stand-in notmuch answers with the archive's copy,
                ;; and a hit maps to its group only once that group loaded
                (write-region "" nil load-hold)
                (load-mail-groups (list "nnmaildir+gmail:archive"))
                (search-mail "archived")
                (record "a search while a group loads opens nothing yet"
                        (derived-mode-p 'gnus-group-mode)
                        :got (format "%s" major-mode))
                (delete-file load-hold)
                (loads-done)
                (record "the search opens its hit once the load is done"
                        (and (derived-mode-p 'gnus-summary-mode)
                             (equal (mapcar #'mail-header-subject gnus-newsgroup-headers)
                                    '("archived")))
                        :got (format "%s: %S" major-mode
                                     (mapcar #'mail-header-subject gnus-newsgroup-headers)))
                (when (derived-mode-p 'gnus-summary-mode)
                  (gnus-summary-exit-no-update))
                ;; entering a label is where Gnus asks nnmaildir for it
                (gnus-group-jump-to-group "nnmaildir+gmail:emacs")
                (execute-kbd-macro (kbd "RET"))
                (record "RET on a bulk group's line shows its mail"
                        (and (derived-mode-p 'gnus-summary-mode)
                             (equal (mapcar #'mail-header-subject gnus-newsgroup-headers)
                                    '("emacs-devel post")))
                        :got (format "%s: %S" major-mode
                                     (mapcar #'mail-header-subject gnus-newsgroup-headers)))
                (gnus-summary-exit-no-update)
                ;; a list post held in All Mail and in the news store;
                ;; notmuch names All Mail's copy first
                (let ((copies (list (expand-file-name "cur/1700000044.44.archive" archive)
                                    (expand-file-name "cur/1700000044.44.news" news-dir))))
                  (dolist (copy copies)
                    (email-e2e--write-news-post copy "news twin" "news-twin"
                                                "Sun, 27 Sep 2026 13:00:00 +0000" nil "S"))
                  (with-temp-file (expand-file-name "news-twin" hits)
                    (insert (mapconcat (lambda (copy) (concat copy ":2,S\n")) copies ""))))
                (search-mail "news-twin")
                (let ((groups (and (derived-mode-p 'gnus-summary-mode)
                                   (mapcar (lambda (article) (nnselect-article-group article))
                                           gnus-newsgroup-articles))))
                  (record "a search shows a message both stores hold once, in the news group"
                          (equal groups (list news-group))
                          :got (format "%s: %S" major-mode groups)))
                (when (derived-mode-p 'gnus-summary-mode)
                  (gnus-summary-exit-no-update))
                ;; Gmail never sees a news post, so none goes to the trash
                (let ((trashed (messages-in trash)))
                  (gnus-group-jump-to-group news-group)
                  (execute-kbd-macro (kbd "RET"))
                  (gnus-summary-goto-subject (email-e2e--article "news post two"))
                  (execute-kbd-macro (kbd "dx"))
                  (let ((left (message-ids (messages-in news-dir))))
                    (record "d x in a news group deletes the post's file and trashes nothing"
                            (and (not (member "<news-two@fixture.example>" left))
                                 (member "<news-one@fixture.example>" left)
                                 (equal (messages-in trash) trashed))
                            :got (format "news %S, trash %d before and %d after" left
                                         (length trashed) (length (messages-in trash))))))
                (when (derived-mode-p 'gnus-summary-mode)
                  (gnus-summary-exit-no-update))
                ;; the inbox opens once its fresh read lands
                (write-region "" nil load-hold)
                (let ((start (float-time)))
                  (open-mail-inbox)
                  (record "open-mail-inbox returns at once and opens nothing before the inbox is read"
                          (and (< (- (float-time) start) 0.5)
                               (not (get-buffer "*Summary nnmaildir+gmail:inbox*")))
                          :got (format "%.2f s, %S" (- (float-time) start)
                                       (get-buffer "*Summary nnmaildir+gmail:inbox*"))))
                (delete-file load-hold)
                (loads-done)
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
                ;; Gnus keeps its letters, and jit-lock shows symbols over
                ;; them; a first entry finds every message unseen
                (let ((fresh (email-e2e--article "fresh"))
                      (seen (email-e2e--article "seen")))
                  (record "unread mail shows a dot, read mail and the unseen dot a blank"
                          (and (equal (mark-symbols fresh) '("●" " "))
                               (equal (mark-letters fresh) (string gnus-unread-mark gnus-unseen-mark))
                               (equal (mark-symbols seen) '(" " " "))
                               (equal (mark-letters seen) (string gnus-ancient-mark gnus-unseen-mark)))
                          :got (format "fresh %S over %S, seen %S over %S"
                                       (mark-symbols fresh) (mark-letters fresh)
                                       (mark-symbols seen) (mark-letters seen))))
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
                ;; parameter governs; without it only unread mail shows.
                ;; Asked for old headers, nnmaildir would hand over every
                ;; header of the group on each entry
                (let* ((fetch-olds nil)
                       (note (lambda (_articles &optional _group _server fetch-old)
                               (push fetch-old fetch-olds))))
                  (advice-add 'nnmaildir-retrieve-headers :before note)
                  (unwind-protect
                      (progn
                        (gnus-group-jump-to-group "nnmaildir+gmail:inbox")
                        (execute-kbd-macro (kbd "RET")))
                    (advice-remove 'nnmaildir-retrieve-headers note))
                  (record "entering a group asks nnmaildir for no older headers"
                          (and fetch-olds (seq-every-p #'null fetch-olds))
                          :got (format "%S" fetch-olds)))
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
                (let (opened)
                  (cl-letf (((symbol-function 'browse-url)
                             (lambda (url &rest _) (setq opened url))))
                    (execute-kbd-macro (kbd "C-c C-o")))
                  (record "C-c C-o in the thread view opens a message of the Gmail inbox in Gmail"
                          (equal opened (gmail-message-url "<plan@fixture.example>"))
                          :got (format "%S at %S" opened (subject-at-point))))
                ;; the stand-in notmuch answers the query with Bob's
                ;; message, after the key has returned
                (with-temp-file (expand-file-name "(from:bob)" hits)
                  (insert "id:plan-bob@fixture.example\n"))
                (execute-kbd-macro (kbd ", n from:bob RET"))
                (let ((waited (process-live-p mail-thread-filter-process)))
                  (with-timeout (5)
                    (while mail-thread-filter-process
                      (accept-process-output nil 0.05)))
                  (record ", n hides the messages a notmuch query does not match"
                          (and (equal (mapcar (lambda (message)
                                                (mail-header-subject
                                                 (mail-thread-message-header message)))
                                              (mail-thread-shown-messages))
                                      '("Re: release plan (Bob)"))
                               ;; what redisplay skips: the first message's line
                               (invisible-p (mail-thread-message-marker (car mail-thread-messages)))
                               (equal (subject-at-point) "Re: release plan (Bob)")
                               (string-suffix-p "1 of 3 shown: from:bob" header-line-format))
                          :got (format "answered after the key %S, shown %S, at %S, %S" waited
                                       (mapcar (lambda (message)
                                                 (mail-header-subject
                                                  (mail-thread-message-header message)))
                                               (mail-thread-shown-messages))
                                       (subject-at-point) header-line-format)))
                (execute-kbd-macro (kbd ", n RET"))
                (record ", n with an empty query shows every message again"
                        (and (= 3 (length (mail-thread-shown-messages)))
                             (string-suffix-p "3 messages" header-line-format))
                        :got (format "%d shown, %S" (length (mail-thread-shown-messages))
                                     header-line-format))
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
                ;; the other two are read by now, so the view folds them
                (execute-kbd-macro (kbd "zR"))
                (email-e2e--idle 5 (lambda () (= 3 (length (open-subjects)))))
                (record "zR unfolds every message, rendering the read ones the view left folded"
                        (and (equal (open-subjects)
                                    '("release plan" "Re: release plan (Bob)"
                                      "Re: release plan (Ann)"))
                             (string-match-p "body of release plan" (thread-text)))
                        :got (format "%S open" (open-subjects)))
                (execute-kbd-macro (kbd "zM"))
                (record "zM folds every message, point on the line of the one it was in"
                        (and (null (open-subjects))
                             (equal (subject-at-point) "Re: release plan (Bob)")
                             (eql (point) (marker-position (mail-thread-message-marker
                                                            (mail-thread-message-at-point)))))
                        :got (format "%S open, at %S" (open-subjects) (subject-at-point)))
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
                (let (opened)
                  (cl-letf (((symbol-function 'browse-url)
                             (lambda (url &rest _) (setq opened url))))
                    (execute-kbd-macro (kbd "C-c C-o")))
                  (record "C-c C-o in the article buffer opens its message in Gmail"
                          (equal opened (gmail-message-url "<plan-bob@fixture.example>"))
                          :got (format "%S in %s" opened major-mode)))
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
                  ;; Gnus replaces the letter in place, keeping the old
                  ;; letter's properties
                  (record "the dot comes back on a message ! marks unread"
                          (equal (car (mark-symbols fresh)) "●")
                          :got (format "%S over %S" (mark-symbols fresh) (mark-letters fresh)))
                  (execute-kbd-macro "k!")
                  (record "! on an unread message marks it read"
                          (and (equal (line-marks fresh) " r")
                               (eql (gnus-summary-article-number) below))
                          :got (format "line starts %S, point on %S"
                                       (line-marks fresh) (gnus-summary-article-number)))
                  (record "the dot leaves a message ! marks read"
                          (equal (car (mark-symbols fresh)) " ")
                          :got (format "%S over %S" (mark-symbols fresh) (mark-letters fresh)))
                  ;; what a sent reply does to the message it answers: the
                  ;; line keeps its face, so only the letter is redrawn
                  (let ((bob (email-e2e--article "Re: release plan (Bob)")))
                    (save-excursion (gnus-summary-mark-article-as-replied bob))
                    (record "a message marked replied shows the reply arrow"
                            (equal (cadr (mark-symbols bob)) "↩")
                            :got (format "%S over %S" (mark-symbols bob) (mark-letters bob))))
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
                (let* ((pair (sort (mapcar #'email-e2e--article '("fresh" "seen")) #'<))
                       (bob (email-e2e--article "Re: release plan (Bob)")))
                  (cl-flet ((mark-pair ()
                              (dolist (article pair)
                                (gnus-summary-goto-subject article)
                                (execute-kbd-macro "m"))
                              (gnus-summary-goto-subject bob))
                            (unread (article)
                              (and (memq article gnus-newsgroup-unreads) t))
                            (secondary (article)
                              (aref (mark-letters article) 1)))
                    (mark-pair)
                    (let ((drawn (mapcar #'secondary pair))
                          (before (mapcar #'unread pair))
                          (bob-before (unread bob)))
                      (execute-kbd-macro "!")
                      (record "! with messages marked toggles them instead of the one at point, and unmarks them"
                              (and (equal drawn (list gnus-process-mark gnus-process-mark))
                                   (equal (mapcar #'unread pair)
                                          (make-list 2 (not (seq-some #'identity before))))
                                   (eq (unread bob) bob-before)
                                   (null gnus-newsgroup-processable)
                                   (not (memq gnus-process-mark (mapcar #'secondary pair)))
                                   (eql (gnus-summary-article-number) bob))
                              :got (format "drawn %S, unread %S then %S, bob %S then %S, marked %S, point on %S"
                                           drawn before (mapcar #'unread pair) bob-before (unread bob)
                                           gnus-newsgroup-processable (gnus-summary-article-number))))
                    (mark-pair)
                    (execute-kbd-macro "d")
                    (record "d with messages marked queues them for the trash, unmarks them and stays"
                            (and (equal (mail-marked-articles 'delete) pair)
                                 (null gnus-newsgroup-processable)
                                 (eql (gnus-summary-article-number) bob))
                            :got (format "%S, marked %S, point on %S" mail-marks
                                         gnus-newsgroup-processable (gnus-summary-article-number)))
                    (mark-pair)
                    (execute-kbd-macro "u")
                    (record "u with messages marked takes them out of the queue and marks them unread"
                            (and (null mail-marks)
                                 (equal (mapcar #'unread pair) '(t t))
                                 (null gnus-newsgroup-processable)
                                 (eql (gnus-summary-article-number) bob))
                            :got (format "%S, unread %S, marked %S, point on %S" mail-marks
                                         (mapcar #'unread pair) gnus-newsgroup-processable
                                         (gnus-summary-article-number)))))
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
                  (record "d queues the message at point for the trash, draws a cross and moves down"
                          (and (eq (alist-get seen mail-marks) 'delete)
                               (eq (aref (line-marks seen) 0) ?×)
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
                          :got (format "%S, point on %S" mail-marks (gnus-summary-article-number)))
                  (record "a queued archive draws a down arrow on every line of the thread"
                          (seq-every-p (lambda (article) (eq (aref (line-marks article) 0) ?↓))
                                       plan)
                          :got (format "%S" (mapcar #'line-marks plan)))
                  (gnus-summary-goto-subject seen)
                  (execute-kbd-macro "m"))
                (execute-kbd-macro "x")
                (record "x moves the queued deletion into the trash maildir"
                        (equal (message-ids (messages-in trash)) '("<seen@fixture.example>"))
                        :got (format "%S" (message-ids (messages-in trash))))
                (record "x takes the mark off a marked message it sends away"
                        (null gnus-newsgroup-processable)
                        :got (format "%S" gnus-newsgroup-processable))
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
                ;; gR reads the routine groups again in the background,
                ;; as every sync does
                (execute-kbd-macro (kbd "gR"))
                (loads-done)
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
                (loads-done)
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
                              ;; , / opens the live search's minibuffer
                              ((symbol-function 'search-mail-live)
                               (lambda () (interactive) (push 'search called)))
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
                                     (not (string-match-p "inbox@example\\|agzam" (recipients message)))
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
                                           "https://yhetil.org/emacs-devel/list-plan-dan%40fixture.example/"))
                              :got (format "%S" (reverse browsed)))
                      (setq browsed nil)
                      (at answer)
                      (execute-kbd-macro (kbd "C-c C-o"))
                      (record "C-c C-o opens Emacs list mail Gmail holds in its yhetil thread"
                              (equal browsed
                                     '("https://yhetil.org/emacs-devel/list-plan-dan%40fixture.example/t/#u"))
                              :got (format "%S" browsed))
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
                      (at to-move)
                      (execute-kbd-macro "m")
                      (at plan)
                      (execute-kbd-macro (kbd "RET"))
                      (execute-kbd-macro (kbd "C-j"))
                      (let ((message (composed "r"))
                            (marked (with-current-buffer summary
                                      (copy-sequence gnus-newsgroup-processable))))
                        (record "r in the thread view answers the message at point, not the summary's marked one"
                                (and (string-match-p "dan@example\\.com" (field message :to))
                                     (string-match-p "^> body of Re: list plan" (field message :body))
                                     (not (string-match-p "to move" (field message :body)))
                                     (equal marked (list to-move)))
                                :got (format "%S, marked %S" message marked)))
                      (at plan)
                      (execute-kbd-macro "M")
                      (let ((after-m (copy-sequence gnus-newsgroup-processable)))
                        (execute-kbd-macro (kbd ", * *"))
                        (let ((all (sort (copy-sequence gnus-newsgroup-processable) #'<)))
                          (execute-kbd-macro (kbd ", * u"))
                          (record "M marks nothing, , * * marks every message and , * u unmarks them all"
                                  (and (equal after-m (list to-move))
                                       (equal all (sort (list plan answer to-move to-label) #'<))
                                       (null gnus-newsgroup-processable)
                                       (not (eq (key-binding "%") 'gnus-uu-mark-by-regexp)))
                                  :got (format "M %S, , * * %S, , * u %S, %% runs %S"
                                               after-m all gnus-newsgroup-processable
                                               (key-binding "%")))))
                      (cl-flet ((mark-both ()
                                  (dolist (article (list to-move to-label))
                                    (at article)
                                    (execute-kbd-macro "m"))
                                  (at plan)))
                        (mark-both)
                        (execute-kbd-macro (kbd ", *"))
                        (let ((menu (with-current-buffer " *transient*"
                                      (buffer-substring-no-properties (point-min) (point-max)))))
                          (execute-kbd-macro "!")
                          (record ", * names the marked messages, and ! there toggles them with point staying"
                                  (and (string-match-p "Selected: 2 marked" menu)
                                       (memq to-move gnus-newsgroup-unreads)
                                       (memq to-label gnus-newsgroup-unreads)
                                       (not (memq plan gnus-newsgroup-unreads))
                                       (null gnus-newsgroup-processable)
                                       (eql (gnus-summary-article-number) plan))
                                  :got (format "%S, unread %S, marked %S, point on %S" menu
                                               gnus-newsgroup-unreads gnus-newsgroup-processable
                                               (gnus-summary-article-number))))
                        (mark-both)
                        (execute-kbd-macro (kbd ", * !")))
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
                                              '("https://yhetil.org/emacs-devel/list-plan%40fixture.example/"))
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
                      ;; the label menu: keys from the label names, and a
                      ;; change that lands once its turns have run
                      (cl-flet ((menu-text ()
                                  (with-current-buffer " *transient*"
                                    (buffer-substring-no-properties (point-min) (point-max))))
                                (labels-done ()
                                  (loads-done)
                                  (with-timeout (10 nil)
                                    (while mail-label-jobs
                                      (discard-input)
                                      (accept-process-output nil 0.05)))
                                  (set-buffer (window-buffer (selected-window))))
                                ;; the label names on ARTICLE's line
                                (chip-names (article)
                                  (split-string (substring-no-properties
                                                 (or (line-chips article) "no slot")))))
                        (at to-label)
                        (execute-kbd-macro (kbd ", l"))
                        (let ((menu (menu-text)))
                          (execute-kbd-macro (kbd "L"))
                          ;; a GUI's Return key, which the summary binds in normal state
                          (execute-kbd-macro (kbd "<return>"))
                          (labels-done)
                          ;; l is the picker's, so no label gets it
                          (record ", l lists each label under its key, the message's own checked, and L <return> adds labelled"
                                  (and (string-match-p "Labels: the message on screen" menu)
                                       (string-match-p "t ✓ lists" menu)
                                       (string-match-p "L   labelled" menu)
                                       (string-match-p "i   Inbox" menu)
                                       (string-match-p "l by name" menu)
                                       (equal (message-ids (messages-in labelled))
                                              '("<to-label@fixture.example>"))
                                       (member "<to-label@fixture.example>"
                                               (message-ids (messages-in lists))))
                                  :got (format "%S, labelled %S" menu
                                               (message-ids (messages-in labelled))))
                          (let ((chips (line-chips to-label)))
                            (record "the line shows the label the menu added at once, in its Gmail colors, next to its own group's"
                                    (and (equal (chip-names to-label) '("labelled" "lists"))
                                         (equal (get-text-property 1 'face chips)
                                                '(:background "#9b811e" :foreground "white")))
                                    :got (format "%S" chips))))
                        ;; two marked, one of them labelled already
                        (dolist (article (list to-label plan))
                          (at article)
                          (execute-kbd-macro "m"))
                        (at answer)
                        (execute-kbd-macro (kbd ", l"))
                        (let ((menu (menu-text)))
                          (execute-kbd-macro (kbd "L"))
                          (let ((toggled (menu-text)))
                            (execute-kbd-macro (kbd "RET"))
                            (labels-done)
                            (record ", l on marked messages half-fills a label some have, L checks it, RET adds it and uses the marks up"
                                    (and (string-match-p "Labels: 2 marked messages" menu)
                                         (string-match-p "L ◐ labelled" menu)
                                         (string-match-p "L ✓ labelled" toggled)
                                         (equal (sort (message-ids (messages-in labelled)) #'string<)
                                                '("<list-plan@fixture.example>"
                                                  "<to-label@fixture.example>"))
                                         (null gnus-newsgroup-processable))
                                    :got (format "%S then %S, labelled %S, marked %S" menu toggled
                                                 (message-ids (messages-in labelled))
                                                 gnus-newsgroup-processable))))
                        (at plan)
                        (execute-kbd-macro (kbd ", l"))
                        ;; savehist would keep the pick in the real history
                        (let ((minibuffer-history nil))
                          (execute-kbd-macro (kbd "l labelled RET")))
                        (let ((picked (menu-text)))
                          (execute-kbd-macro "q")
                          (labels-done)
                          (record ", l l picks a label by name, and closing the menu takes it off, the message staying where it is read"
                                  (and (string-match-p "L   labelled" picked)
                                       (equal (message-ids (messages-in labelled))
                                              '("<to-label@fixture.example>"))
                                       (member "<list-plan@fixture.example>"
                                               (message-ids (messages-in lists)))
                                       (with-current-buffer summary
                                         (gnus-summary-goto-subject plan)))
                                  :got (format "%S, labelled %S" picked
                                               (message-ids (messages-in labelled)))))
                        (record "the line drops the chip of a label taken off"
                                (equal (chip-names plan) '("lists"))
                                :got (format "%S" (line-chips plan)))
                        ;; expreg-transient opens with visual state and replays
                        ;; a , it lets through, except inside a keyboard macro,
                        ;; which general assumes recorded the replayed key
                        (at plan)
                        (let ((general--simulate-as-is t))
                          (execute-kbd-macro (kbd "V j , l")))
                        (execute-kbd-macro (kbd "L"))
                        (execute-kbd-macro (kbd "RET"))
                        (labels-done)
                        (let ((ids (sort (message-ids (messages-in labelled)) #'string<)))
                          (record ", l on a visual selection labels each message in it and ends visual state"
                                  (and (equal ids '("<list-plan-dan@fixture.example>"
                                                    "<list-plan@fixture.example>"
                                                    "<to-label@fixture.example>"))
                                       (eq evil-state 'normal))
                                  :got (format "%S, %s state" ids evil-state)))
                        ;; a label made from the menu, over a whole thread
                        (at plan)
                        (execute-kbd-macro (kbd ", L"))
                        (let ((menu (menu-text))
                              (travel (expand-file-name "travel/" root)))
                          ;; savehist would keep the answer in the real history
                          (let ((minibuffer-history nil))
                            (execute-kbd-macro (kbd "+ travel RET")))
                          (let ((reopened (menu-text)))
                            (execute-kbd-macro (kbd "RET"))
                            (labels-done)
                            (record ", L takes the thread, + makes a label checked on it, and RET fills the label"
                                    (and (string-match-p "Labels: 2 messages" menu)
                                         (string-match-p "T ✓ travel" reopened)
                                         (gnus-group-entry "nnmaildir+gmail:travel")
                                         (equal (sort (message-ids (messages-in travel)) #'string<)
                                                '("<list-plan-dan@fixture.example>"
                                                  "<list-plan@fixture.example>")))
                                    :got (format "%S then %S, travel %S" menu reopened
                                                 (and (file-directory-p travel)
                                                      (message-ids (messages-in travel)))))
                            (record "a label made with + goes under Labels"
                                    (equal (gnus-group-topic "nnmaildir+gmail:travel") "Labels")
                                    :got (format "%S" gnus-topic-alist))
                            (record "a reply shows its labels even when the line above has the same"
                                    (and (equal (chip-names plan) '("labelled" "lists" "travel"))
                                         (equal (chip-names answer) '("labelled" "lists" "travel")))
                                    :got (format "%S then %S" (line-chips plan) (line-chips answer)))
                            ;; in the label's own summary the message leaves
                            ;; once the label is off
                            (delete-other-windows)
                            (switch-to-buffer gnus-group-buffer)
                            ;; a group no topic names has no line to RET on;
                            ;; entering it needs Gnus to know the copies
                            (gnus-summary-read-group "nnmaildir+gmail:travel" t t)
                            (let ((root (email-e2e--article "list plan")))
                              (record "a label's own summary shows every label of a line, its own too"
                                      (equal (chip-names root) '("labelled" "lists" "travel"))
                                      :got (format "%S" (line-chips root))))
                            (let ((first (gnus-summary-article-number)))
                              (execute-kbd-macro (kbd ", l"))
                              (execute-kbd-macro (kbd "T"))
                              (execute-kbd-macro (kbd "RET"))
                              (labels-done)
                              (record ", l T RET in the label's own summary takes the message out of view"
                                      (and (equal gnus-newsgroup-name "nnmaildir+gmail:travel")
                                           (= 1 (length (messages-in travel)))
                                           (not (gnus-summary-goto-subject first)))
                                      :got (format "%s, travel %S, %S still shown" gnus-newsgroup-name
                                                   (message-ids (messages-in travel))
                                                   (gnus-summary-goto-subject first))))
                            (execute-kbd-macro "q")))
                        ;; the thread view labels the message at point
                        (at plan)
                        (execute-kbd-macro (kbd "RET"))
                        (let ((header (with-current-buffer mail-thread-buffer-name
                                        header-line-format)))
                          (record "the thread view names every label of the thread after its subject, its group's too"
                                  (and (stringp header)
                                       (string-prefix-p "list plan  " header)
                                       (seq-every-p (lambda (name)
                                                      (member name (split-string
                                                                    (substring-no-properties header))))
                                                    '("labelled" "lists" "travel"))
                                       (eq (get-text-property (string-search " lists " header)
                                                              'face header)
                                           'mail-label))
                                  :got (format "%S" header)))
                        (execute-kbd-macro (kbd ", l"))
                        (let ((menu (menu-text)))
                          ;; M-m is embark-select, which moves down after
                          ;; each pick: Inbox and emacs are the first two
                          (let ((minibuffer-history nil))
                            (execute-kbd-macro (kbd "l M-m M-m RET")))
                          (let ((picked (menu-text)))
                            (execute-kbd-macro (kbd "i e"))
                            (let ((said (saying "q")))
                              (record ", l in the thread view opens the menu on the message at point"
                                      (and (string-match-p "Labels: the message on screen" menu)
                                           (string-match-p "t ✓ lists" menu)
                                           (string-match-p "L ✓ labelled" menu))
                                      :got menu)
                              (record "l with M-m picks several labels by name, and closing a menu set back changes nothing"
                                      (and (string-match-p "i ✓ Inbox" picked)
                                           (string-match-p "e ✓ emacs" picked)
                                           (member "Labels: nothing changed" said))
                                      :got (format "%S, said %S" picked said)))))
                        ;; a NUL byte in an upload makes Gmail drop the connection
                        (with-temp-buffer
                          (insert "From: nul@example.com\nSubject: nul\n"
                                  "Message-ID: <nul@fixture.example>\n\nbyte\0code\n")
                          (gnus-request-accept-article "nnmaildir+gmail:moved" nil t t))
                        (let ((file (seq-find (lambda (file)
                                                (equal (car (message-ids (list file)))
                                                       "<nul@fixture.example>"))
                                              (messages-in moved))))
                          (record "a message filed into a label leaves its NUL bytes behind"
                                  (and file
                                       (with-temp-buffer
                                         (insert-file-contents-literally file)
                                         (and (search-forward "bytecode" nil t)
                                              (not (search-backward "\0" nil t))
                                              (not (search-forward "\0" nil t)))))
                                  :got (format "%S" file))))
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
                          (tab-bar-close-tab))))))
                ;; search results act on the inbox copy: each message
                ;; below sits in the inbox and in All Mail, and notmuch
                ;; names All Mail's copy first
                (cl-flet* ((twin (id flags &rest labels)
                             ;; a copy of ID's message in All Mail, the inbox
                             ;; and LABELS, All Mail's first in hits/ID
                             (let ((files (mapcar (lambda (dir)
                                                    (expand-file-name
                                                     (format "cur/%s.%s:2,%s" id
                                                             (file-name-nondirectory
                                                              (directory-file-name dir))
                                                             flags)
                                                     dir))
                                                  (append (list archive inbox) labels))))
                               (dolist (file files)
                                 (email-e2e--write-message file "Tom <tom@example.com>" id id))
                               (with-temp-file (expand-file-name id hits)
                                 (insert (mapconcat (lambda (file) (concat file "\n")) files "")))))
                           ;; , / is the live search; this is the summary
                           ;; search its export runs
                           (search-for (query)
                             (search-mail query))
                           (searches ()
                             (with-temp-buffer
                               (insert-file-contents searches-log)
                               (last (split-string (buffer-string) "\n" t) 2)))
                           (where (id)
                             (list :inbox (flags-of inbox id) :archive (flags-of archive id)
                                   :trash (flags-of trash id)))
                           ;; what the phone did to ID, as mbsync brings it:
                           ;; the inbox file renamed to Gmail's FLAGS
                           (phone (id flags)
                             (let ((file (seq-find (lambda (file)
                                                     (string-prefix-p (concat id ".")
                                                                      (file-name-nondirectory file)))
                                                   (messages-in inbox))))
                               (rename-file file (expand-file-name
                                                  (format "cur/%s:2,%s"
                                                          (car (split-string
                                                                (file-name-nondirectory file) ":"))
                                                          flags)
                                                  inbox)))))
                  (delete-other-windows)
                  (switch-to-buffer gnus-group-buffer)
                  (dolist (id '("twin-read" "twin-star" "twin-keep" "twin-trash" "twin-gone"
                                "twin-carry" "twin-drop"))
                    (twin id "S"))
                  (twin "twin-label" "S" lists)
                  ;; the phone changes these while the inbox summary is open
                  (pcase-dolist (`(,id ,flags) '(("phone-read" "") ("phone-star" "")
                                                 ("phone-unread" "S") ("phone-unstar" "FS")
                                                 ("phone-mixed" "S") ("own-read" "")
                                                 ("phone-search" "")))
                    (twin id flags))
                  (execute-kbd-macro (kbd "gR"))
                  (loads-done)
                  (search-for "twin-read")
                  (record "a search hit is the inbox copy when the inbox holds the message"
                          (and (derived-mode-p 'gnus-summary-mode)
                               (equal (nnselect-article-group (car gnus-newsgroup-articles))
                                      "nnmaildir+gmail:inbox"))
                          :got (format "%s, %S" major-mode (bound-and-true-p gnus-newsgroup-selection)))
                  (let ((chips (line-chips (car gnus-newsgroup-articles))))
                    (record "a search hit shows Inbox among its labels, a search leaving none out"
                            (equal (split-string (substring-no-properties (or chips "no slot")))
                                   '("Inbox"))
                            :got (format "%S" chips)))
                  (record "a search asks notmuch for the newest 500, then for every copy of them"
                          (let ((runs (searches)))
                            (and (string-match-p "--duplicate=1 --limit=500 twin-read\\'" (car runs))
                                 (string-match-p "--output=files --limit=500 twin-read\\'" (cadr runs))))
                          :got (format "%S" (searches)))
                  (execute-kbd-macro "!q")
                  (record "! in search results marks the inbox copy unread, and q saves it"
                          (equal (where "twin-read") '(:inbox "" :archive "S" :trash nil))
                          :got (format "%S" (where "twin-read")))
                  ;; hits/ still names the inbox file as it was before ! renamed
                  ;; it, as notmuch does until its next run
                  (search-for "twin-read")
                  (record "a search finds the inbox copy Gnus renamed since notmuch indexed it"
                          (and (derived-mode-p 'gnus-summary-mode)
                               (equal (nnselect-article-group (car gnus-newsgroup-articles))
                                      "nnmaildir+gmail:inbox"))
                          :got (format "%s in %s" major-mode (buffer-name)))
                  (when (derived-mode-p 'gnus-summary-mode)
                    (execute-kbd-macro "q"))
                  (search-for "twin-star")
                  (execute-kbd-macro "=q")
                  (record "= in search results stars the inbox copy"
                          (equal (where "twin-star") '(:inbox "FS" :archive "S" :trash nil))
                          :got (format "%S" (where "twin-star")))
                  (search-for "twin-keep")
                  (execute-kbd-macro "dqn")
                  (record "q asks before it leaves a queue behind, and n leaves without running it"
                          (and (eq (window-buffer (selected-window)) (get-buffer gnus-group-buffer))
                               (equal (where "twin-keep") '(:inbox "S" :archive "S" :trash nil)))
                          :got (format "%s, %S" (buffer-name (window-buffer (selected-window)))
                                       (where "twin-keep")))
                  (search-for "twin-trash")
                  (execute-kbd-macro "dqy")
                  (record "y runs the queue first: d sends the inbox copy to the trash"
                          (pcase (where "twin-trash")
                            (`(:inbox nil :archive "S" :trash ,trashed) trashed))
                          :got (format "%S" (where "twin-trash")))
                  (search-for "twin-gone")
                  (execute-kbd-macro "axq")
                  (record "a in search results archives: the inbox copy goes, All Mail's stays"
                          (equal (where "twin-gone") '(:inbox nil :archive "S" :trash nil))
                          :got (format "%S" (where "twin-gone")))
                  ;; the phone reads and stars a hit while its search is
                  ;; open, and a sync started from Emacs merges that
                  (search-for "phone-search")
                  (phone "phone-search" "FS")
                  (refresh-mail-group "nnmaildir+gmail:inbox")
                  (execute-kbd-macro "q")
                  (record "q in search results keeps a read and a star the phone made meanwhile"
                          (equal (flags-of inbox "phone-search") "FS")
                          :got (format "%S" (where "phone-search")))
                  (gnus-group-jump-to-group "nnmaildir+gmail:lists")
                  (execute-kbd-macro (kbd "RET"))
                  (let* ((label (email-e2e--article "twin-label"))
                         (before (line-chips label)))
                    (gnus-summary-goto-subject label)
                    (execute-kbd-macro "ax")
                    (record "a in a label archives: the inbox copy goes, the label keeps the message"
                            (and (null (flags-of inbox "twin-label"))
                                 (equal (flags-of lists "twin-label") "S")
                                 (gnus-summary-goto-subject label nil t)
                                 (null mail-marks))
                            :got (format "inbox %S, lists %S, line %S, queue %S"
                                         (flags-of inbox "twin-label") (flags-of lists "twin-label")
                                         (gnus-summary-goto-subject label nil t) mail-marks))
                    (let ((after (line-chips label)))
                      (record "a label's line shows Inbox while the inbox holds the message, and drops it once archived"
                              (and (equal (split-string (substring-no-properties (or before "")))
                                          '("Inbox" "lists"))
                                   (equal (split-string (substring-no-properties (or after "")))
                                          '("lists")))
                              :got (format "%S then %S" before after))))
                  (gnus-summary-goto-subject (email-e2e--article "to label"))
                  (condition-case nil
                      (execute-kbd-macro "a")
                    (user-error nil))
                  (record "a refuses a message the inbox does not hold"
                          (and (null mail-marks)
                               (equal (flags-of lists "to-label") "S"))
                          :got (format "queue %S, lists %S" mail-marks (flags-of lists "to-label")))
                  (execute-kbd-macro "q")
                  ;; a search opened from the inbox summary, which stays
                  ;; open under it and writes its own state on exit
                  (open-mail-inbox)
                  (loads-done)
                  (let ((summary (current-buffer))
                        (carry (email-e2e--article "twin-carry"))
                        (drop (email-e2e--article "twin-drop")))
                    (record "the inbox shows Inbox on its own lines"
                            (equal (split-string (substring-no-properties (or (line-chips carry) "")))
                                   '("Inbox"))
                            :got (format "%S" (line-chips carry)))
                    (search-for "twin-carry")
                    (execute-kbd-macro "!q")
                    (record "the inbox summary under a search learns what it changed"
                            (and (eq (current-buffer) summary)
                                 (memq carry gnus-newsgroup-unreads)
                                 (equal (line-marks carry) "  "))
                            :got (format "%s, unread %S, line starts %S" (buffer-name)
                                         (memq carry gnus-newsgroup-unreads)
                                         (line-marks carry)))
                    (search-for "twin-drop")
                    (execute-kbd-macro "dxq")
                    (record "a message trashed from that search leaves the inbox summary's lines"
                            (and (eq (current-buffer) summary)
                                 (not (gnus-summary-goto-subject drop nil t))
                                 (flags-of trash "twin-drop"))
                            :got (format "%s, line %S, trash %S" (buffer-name)
                                         (gnus-summary-goto-subject drop nil t)
                                         (flags-of trash "twin-drop")))
                    ;; mail arriving while that summary is open: a sync
                    ;; started from Emacs reads twin-late in, and nothing
                    ;; reads twin-fresh before the search does
                    (twin "twin-late" "")
                    (refresh-mail-group "nnmaildir+gmail:inbox")
                    (twin "twin-fresh" "")
                    (search-for "twin-fresh")
                    (execute-kbd-macro "!q")
                    (record "! in search results reaches mail that arrived after the inbox was read"
                            (equal (flags-of inbox "twin-fresh") "S")
                            :got (format "%S" (where "twin-fresh")))
                    (search-for "twin-late")
                    (execute-kbd-macro "!q")
                    (record "! in search results reaches mail a sync read in after the summary opened"
                            (equal (flags-of inbox "twin-late") "S")
                            :got (format "%S" (where "twin-late")))
                    ;; the phone changes mail the summary shows, the summary
                    ;; reads one message and unreads another, and a sync
                    ;; started from Emacs merges the phone's changes
                    (phone "phone-read" "S")
                    (phone "phone-star" "F")
                    (phone "phone-unread" "")
                    (phone "phone-unstar" "S")
                    (phone "phone-mixed" "FS")
                    (gnus-summary-goto-subject (email-e2e--article "own-read"))
                    (execute-kbd-macro "!")
                    (gnus-summary-goto-subject (email-e2e--article "phone-mixed"))
                    (execute-kbd-macro "!")
                    (refresh-mail-group "nnmaildir+gmail:inbox")
                    ;; the files, and the group the next summary reads
                    (let ((numbers (mapcar #'email-e2e--article
                                           '("phone-read" "phone-star"
                                             "phone-unread" "phone-unstar"))))
                      (execute-kbd-macro "q")
                      (pcase-let* ((`(,read ,star ,unread ,unstar) numbers)
                                   (info (gnus-get-info "nnmaildir+gmail:inbox"))
                                   (ticks (alist-get 'tick (gnus-info-marks info))))
                        (record "q in that inbox keeps a read and a star the phone made meanwhile"
                                (and (equal (list (flags-of inbox "phone-read")
                                                  (flags-of inbox "phone-star"))
                                            '("S" "F"))
                                     (range-member-p read (gnus-info-read info))
                                     (range-member-p star ticks))
                                :got (format "%S %S, group read %S, starred %S"
                                             (where "phone-read") (where "phone-star")
                                             (range-member-p read (gnus-info-read info))
                                             (range-member-p star ticks)))
                        (record "q in that inbox keeps an unread and an unstar the phone made meanwhile"
                                (and (equal (list (flags-of inbox "phone-unread")
                                                  (flags-of inbox "phone-unstar"))
                                            '("" "S"))
                                     (not (range-member-p unread (gnus-info-read info)))
                                     (not (range-member-p unstar ticks)))
                                :got (format "%S %S, group read %S, starred %S"
                                             (where "phone-unread") (where "phone-unstar")
                                             (range-member-p unread (gnus-info-read info))
                                             (range-member-p unstar ticks)))))
                    (record "q in that inbox saves its own read marks beside the phone's star"
                            (equal (list (flags-of inbox "own-read") (flags-of inbox "phone-mixed"))
                                   '("S" "F"))
                            :got (format "%S %S" (where "own-read") (where "phone-mixed")))
                    (record "q in that inbox keeps what the search saved instead of its older state"
                            (equal (where "twin-carry") '(:inbox "" :archive "S" :trash nil))
                            :got (format "%S" (where "twin-carry")))
                    (record "q in that inbox keeps the read marks searches gave mail newer than it"
                            (and (equal (flags-of inbox "twin-late") "S")
                                 (equal (flags-of inbox "twin-fresh") "S"))
                            :got (format "late %S, fresh %S"
                                         (where "twin-late") (where "twin-fresh"))))
                  ;; mbsync renames files behind nnmaildir's back: a flag
                  ;; set on the phone, and a UID once an upload went up
                  (cl-flet* ((try (keys)
                               ;; the error KEYS raise fails their case, not the flow
                               (condition-case err
                                   (progn (execute-kbd-macro (kbd keys)) nil)
                                 (error err)))
                             (enter (group)
                               (switch-to-buffer gnus-group-buffer)
                               (gnus-group-jump-to-group group)
                               (try "RET"))
                             (leave ()
                               (when (derived-mode-p 'gnus-summary-mode)
                                 (execute-kbd-macro "q")))
                             ;; the article with SUBJECT here, or nil
                             (article-of (subject)
                               (when-let* ((header (seq-find (lambda (header)
                                                               (equal (mail-header-subject header)
                                                                      subject))
                                                             gnus-newsgroup-headers)))
                                 (mail-header-number header)))
                             (trash-file (id)
                               (seq-find (lambda (file)
                                           (equal (message-ids (list file))
                                                  (list (format "<%s@fixture.example>" id))))
                                         (messages-in trash)))
                             ;; FILE renamed as mbsync names an upload once it has UID
                             (upload (file uid)
                               (when file
                                 (let ((name (file-name-nondirectory file)))
                                   (string-match "\\`\\([^:]*\\)\\(.*\\)\\'" name)
                                   (rename-file file (expand-file-name
                                                      (format "%s,U=%d%s" (match-string 1 name)
                                                              uid (match-string 2 name))
                                                      (file-name-directory file)))))))
                    (delete-other-windows)
                    (switch-to-buffer gnus-group-buffer)
                    (dolist (id '("renamed-search" "renamed-enter" "synced-star"))
                      (twin id ""))
                    (execute-kbd-macro (kbd "gR"))
                    (loads-done)
                    (phone "renamed-search" "F")
                    (phone "renamed-enter" "S")
                    (let ((err (condition-case err
                                   (progn (search-mail "renamed-search") nil)
                                 (error err))))
                      (record "a search shows a message the phone renamed since its group was read"
                              (and (derived-mode-p 'gnus-summary-mode)
                                   (equal (nnselect-article-group (car gnus-newsgroup-articles))
                                          "nnmaildir+gmail:inbox"))
                              :got (format "%s in %s, %S" major-mode (buffer-name) err)))
                    (leave)
                    (let ((err (enter "nnmaildir+gmail:inbox")))
                      (record "RET shows a message the phone renamed since its group was read"
                              (article-of "renamed-enter")
                              :got (format "%s, %S" (buffer-name) err)))
                    (leave)
                    ;; the trash copy d x filed above, renamed by the push
                    (upload (trash-file "twin-trash") 1)
                    (let ((err (enter "nnmaildir+gmail:trash")))
                      (record "RET shows a copy mbsync renamed with a UID once it went up"
                              (article-of "twin-trash")
                              :got (format "%s, %S" (buffer-name) err)))
                    ;; the push of another copy ends while the trash is open
                    (let ((before (flags-of trash "twin-drop"))
                          (article (article-of "twin-drop")))
                      (upload (trash-file "twin-drop") 2)
                      (when article
                        (gnus-summary-goto-subject article)
                        (execute-kbd-macro "!"))
                      (leave)
                      (let ((name (file-name-nondirectory (or (trash-file "twin-drop") "gone"))))
                        (record "a flag saved on a copy mbsync renamed meanwhile keeps its UID"
                                (and article
                                     (string-match-p ",U=2:2," name)
                                     (not (equal (flags-of trash "twin-drop") before)))
                                :got (format "article %S, %s, flags %S then %S" article name
                                             before (flags-of trash "twin-drop")))))
                    ;; a sync: mbsync stars synced-star and rewrites the
                    ;; inbox's state file
                    (synced (lambda ()
                              (phone "synced-star" "F")
                              (write-region "" nil (expand-file-name ".mbsyncstate" inbox))))
                    (loads-done)
                    (let* ((err (enter "nnmaildir+gmail:inbox"))
                           (article (article-of "synced-star")))
                      (record "a sync that changed the inbox reloads it, and RET shows the phone's star"
                              (and article (equal (line-star article) star))
                              :got (format "article %S, column %S, %S" article
                                           (and article (line-star article)) err)))
                    (leave))
                  ;; the live search over a thread of its own in the inbox.
                  ;; A timer types as a person would: a keyboard macro that
                  ;; ends inside the minibuffer leaves it with what it holds
                  ;; Gnus starts a summary on its first unread message, so
                  ;; only the root is unread and RET has to move to Ann's
                  (let* ((files (mapcar (lambda (name) (expand-file-name name inbox))
                                        '("cur/1700000060.60.fixture:2,"
                                          "cur/1700000061.61.fixture:2,S"
                                          "cur/1700000062.62.fixture:2,S")))
                         (query "live plan")
                         (in-thread "thread:fixture-live and (live plan)")
                         (stage 'typing)
                         (feed (lambda (keys)
                                 (setq unread-command-events
                                       (append unread-command-events
                                               (listify-key-sequence (kbd keys))))))
                         listed previewed beside timer)
                    (email-e2e--write-message (nth 0 files) "Ann <ann@example.com>" "live plan"
                                              "live-plan" "Wed, 23 Sep 2026 09:00:00 +0000")
                    (email-e2e--write-message (nth 1 files) "Bob <bob@example.com>"
                                              "Re: live plan (Bob)" "live-plan-bob"
                                              "Wed, 23 Sep 2026 10:00:00 +0000"
                                              "<live-plan@fixture.example>")
                    (email-e2e--write-message (nth 2 files) "Ann <ann@example.com>"
                                              "Re: live plan (Ann)" "live-plan-ann"
                                              "Wed, 23 Sep 2026 11:00:00 +0000"
                                              (concat "<live-plan@fixture.example> "
                                                      "<live-plan-bob@fixture.example>"))
                    (with-temp-file (expand-file-name query summaries)
                      (insert "((:thread \"fixture-live\" :timestamp 1790000000"
                              " :date_relative \"x\" :matched 3 :total 3 :authors \"Ann, Bob\""
                              " :subject \"live plan\" :query (\"id:live-plan@fixture.example\" nil)"
                              " :tags (\"inbox\")))\n"))
                    (with-temp-file (expand-file-name in-thread newest)
                      (insert "id:live-plan-ann@fixture.example\n"))
                    (with-temp-file (expand-file-name in-thread whole)
                      (insert (nth 2 files) "\n"))
                    ;; RET asks for the thread's messages, then searches them
                    (let ((ids '("id:live-plan-ann@fixture.example"
                                 "id:live-plan-bob@fixture.example"
                                 "id:live-plan@fixture.example")))
                      (with-temp-file (expand-file-name "thread:fixture-live" newest)
                        (insert (mapconcat (lambda (id) (concat id "\n")) ids "")))
                      (with-temp-file (expand-file-name (string-join ids " or ") whole)
                        (insert (mapconcat (lambda (file) (concat file "\n")) files ""))))
                    (delete-other-windows)
                    (switch-to-buffer gnus-group-buffer)
                    (execute-kbd-macro (kbd "gR"))
                    (loads-done)
                    (setq beside (split-window-right)
                          timer (run-at-time
                                 0.05 0.05
                                 (lambda ()
                                   (when-let* ((mini (active-minibuffer-window)))
                                     (with-current-buffer (window-buffer mini)
                                       (pcase stage
                                         ('typing
                                          (funcall feed "live SPC plan")
                                          (setq stage 'listing))
                                         ('listing
                                          (when (< 0 (or (bound-and-true-p vertico--total) 0))
                                            (setq listed (mapcar #'substring-no-properties
                                                                 vertico--candidates))
                                            (funcall feed "C-SPC")
                                            (setq stage 'previewing)))
                                         ('previewing
                                          (when (eq (window-buffer beside)
                                                    (get-buffer mail-live-preview-name))
                                            (setq previewed (with-current-buffer (window-buffer beside)
                                                              (buffer-substring-no-properties
                                                               (point-min) (point-max))))
                                            (funcall feed "RET")
                                            (setq stage 'done)))))))))
                    (unwind-protect
                        (with-timeout (20 (setq stage (list 'timed-out stage)))
                          (call-interactively #'search-mail-live))
                      (cancel-timer timer))
                    (record "the live search lists the thread the typed query matches"
                            (seq-some (lambda (line)
                                        ;; its labels before the subject, as on summary lines
                                        (string-match-p "Ann, Bob +3/3  Inbox  live plan\\'" line))
                                      listed)
                            :got (format "%S, stage %S" listed stage))
                    (record "C-SPC previews the thread's newest match beside the search"
                            (and previewed
                                 (string-match-p "^Subject: Re: live plan (Ann)$" previewed)
                                 (string-match-p "body of Re: live plan (Ann)" previewed))
                            :got (format "%S" previewed))
                    (let ((view (get-buffer mail-thread-buffer-name)))
                      (record "RET reads the thread in the thread view at its newest match"
                              (and (eq stage 'done)
                                   view (get-buffer-window view)
                                   (equal (with-current-buffer view (subject-at-point))
                                          "Re: live plan (Ann)")
                                   (null (get-buffer mail-live-preview-name)))
                              :got (format "stage %S, view %S, at %S" stage
                                           (and view (get-buffer-window view))
                                           (and view (with-current-buffer view
                                                       (ignore-errors (subject-at-point))))))
                      (when-let* ((window (and view (get-buffer-window view))))
                        (select-window window)
                        (execute-kbd-macro "q"))
                      (set-buffer (window-buffer (selected-window)))
                      (when (derived-mode-p 'gnus-summary-mode)
                        (gnus-summary-exit-no-update t))))
                  ;; the live search's lines under embark: a collect buffer
                  ;; acts on each as its thread, and an export reads the
                  ;; threads whole in a summary that keeps the name Gnus gave it
                  (let* ((live-files (mapcar (lambda (name) (expand-file-name name inbox))
                                             '("cur/1700000060.60.fixture:2,"
                                               "cur/1700000061.61.fixture:2,S"
                                               "cur/1700000062.62.fixture:2,S")))
                         (export-files (mapcar (lambda (name) (expand-file-name name inbox))
                                               '("cur/1700000063.63.fixture:2,S"
                                                 "cur/1700000064.64.fixture:2,S")))
                         (live-ids '("id:live-plan-ann@fixture.example"
                                     "id:live-plan-bob@fixture.example"
                                     "id:live-plan@fixture.example"))
                         ;; newest first, as notmuch names them
                         (export-ids '("id:export-plan-ann@fixture.example"
                                       "id:export-plan@fixture.example"))
                         (message-ids (lambda (ids)
                                        (sort (mapcar (lambda (id)
                                                        (concat "<" (string-remove-prefix "id:" id) ">"))
                                                      ids)
                                              #'string<))))
                    (email-e2e--write-message (nth 0 export-files) "Cy <cy@example.com>" "export plan"
                                              "export-plan" "Thu, 24 Sep 2026 09:00:00 +0000")
                    (email-e2e--write-message (nth 1 export-files) "Ann <ann@example.com>"
                                              "Re: export plan" "export-plan-ann"
                                              "Thu, 24 Sep 2026 10:00:00 +0000"
                                              "<export-plan@fixture.example>")
                    ;; the export thread is the newer, so notmuch lists it first
                    (with-temp-file (expand-file-name "live export" summaries)
                      (insert "((:thread \"fixture-export\" :timestamp 1790100000"
                              " :matched 1 :total 2 :authors \"Ann| Cy\" :subject \"export plan\""
                              " :tags (\"inbox\"))\n"
                              "(:thread \"fixture-live\" :timestamp 1790000000"
                              " :matched 3 :total 3 :authors \"Ann, Bob\" :subject \"live plan\""
                              " :tags (\"inbox\")))\n"))
                    (pcase-dolist (`(,threads ,ids ,files)
                                   `(("thread:fixture-export" ,export-ids ,export-files)
                                     ("thread:fixture-export or thread:fixture-live"
                                      ,(append export-ids live-ids)
                                      ,(append export-files live-files))))
                      (with-temp-file (expand-file-name threads newest)
                        (insert (mapconcat (lambda (id) (concat id "\n")) ids "")))
                      (with-temp-file (expand-file-name (string-join ids " or ") whole)
                        (insert (mapconcat (lambda (file) (concat file "\n")) files ""))))
                    (delete-other-windows)
                    (switch-to-buffer gnus-group-buffer)
                    (execute-kbd-macro (kbd "gR"))
                    (loads-done)
                    (cl-flet* ((live (query count keys)
                                 ;; QUERY typed into the live search, then KEYS once
                                 ;; it lists COUNT threads; embark's collect and
                                 ;; export end the search with a quit
                                 (let* ((stage 'typing)
                                        (mail-live-history nil)
                                        (timer (run-at-time
                                                0.05 0.05
                                                (lambda ()
                                                  (when (active-minibuffer-window)
                                                    (pcase stage
                                                      ('typing
                                                       (setq unread-command-events
                                                             (append unread-command-events
                                                                     (listify-key-sequence (kbd query)))
                                                             stage 'listing))
                                                      ('listing
                                                       (when (<= count (or (bound-and-true-p vertico--total) 0))
                                                         (setq unread-command-events
                                                               (append unread-command-events
                                                                       (listify-key-sequence (kbd keys)))
                                                               stage 'acted)))))))))
                                   (unwind-protect
                                       (condition-case nil
                                           (with-timeout (20 (setq stage (list 'timed-out stage)))
                                             (call-interactively #'search-mail-live))
                                         (quit nil))
                                     (cancel-timer timer))
                                   stage))
                               (search-summaries ()
                                 (seq-filter (lambda (buffer)
                                               (with-current-buffer buffer
                                                 (and (derived-mode-p 'gnus-summary-mode)
                                                      (gnus-nnselect-group-p gnus-newsgroup-name))))
                                             (buffer-list)))
                               (new-summary (before)
                                 (car (seq-difference (search-summaries) before)))
                               ;; the export's summary, once the buffer standing in
                               ;; for it is gone: the summary exists before Gnus
                               ;; has filled it, and the idle poll would throw out
                               ;; of the half-built one
                               (exported (before)
                                 (and (not (seq-some (lambda (buffer)
                                                       (buffer-local-value 'mail-live-export buffer))
                                                     (buffer-list)))
                                      (new-summary before)))
                               (summary-ids (summary)
                                 (and summary
                                      (with-current-buffer summary
                                        (sort (mapcar (lambda (article)
                                                        (mail-header-id (gnus-summary-article-header article)))
                                                      gnus-newsgroup-articles)
                                              #'string<))))
                               (leave-summaries (before)
                                 (dolist (summary (seq-difference (search-summaries) before))
                                   (with-current-buffer summary
                                     (gnus-summary-exit-no-update t))))
                               (collected ()
                                 (seq-find (lambda (buffer)
                                             (eq (buffer-local-value 'major-mode buffer) 'embark-collect-mode))
                                           (buffer-list)))
                               ;; what the label region of the collect buffer's
                               ;; first line shows, drawn again or as collected
                               (collect-labels (collect)
                                 (with-current-buffer collect
                                   (when-let* ((start (text-property-not-all
                                                       (point-min) (point-max) 'mail-labels nil)))
                                     (substring-no-properties
                                      (or (get-text-property start 'display)
                                          (buffer-substring
                                           start (next-single-property-change
                                                  start 'mail-labels nil (point-max))))))))
                               ;; what the label slot of each message line shows
                               (thread-labels ()
                                 (when-let* ((view (get-buffer mail-thread-buffer-name)))
                                   (with-current-buffer view
                                     (mapcar (lambda (message)
                                               (save-excursion
                                                 (goto-char (mail-thread-message-marker message))
                                                 (when-let* ((slot (text-property-not-all
                                                                    (point) (line-end-position)
                                                                    'mail-labels nil)))
                                                   (substring-no-properties
                                                    (or (get-text-property slot 'display) "undrawn")))))
                                             mail-thread-messages))))
                               ;; KEYS on the collect buffer's first line; a key
                               ;; that opens a minibuffer would wait for input
                               (on-line (collect keys)
                                 (pop-to-buffer collect)
                                 (goto-char (point-min))
                                 (with-timeout (10 'timed-out)
                                   (condition-case err
                                       (progn (execute-kbd-macro (kbd keys)) nil)
                                     ((error quit) err)))))
                      (let* ((stage (live "live SPC plan" 1 "C-c C-s"))
                             (collect (email-e2e--idle 10 #'collected)))
                        (record "C-c C-s in the live search collects its lines as threads"
                                (and collect
                                     (eq (buffer-local-value 'embark--type collect) 'mail-thread)
                                     (= 1 (length (buffer-local-value 'tabulated-list-entries collect))))
                                :got (format "stage %S, %S" stage collect))
                        (when collect
                          (record "a collected line shows its thread's labels, and , answers with the mail keys"
                                  (and (buffer-local-value 'mail-live-collect-mode collect)
                                       (equal (collect-labels collect) "  Inbox  ")
                                       (with-current-buffer collect
                                         (and (eq (key-binding (kbd ", /")) 'search-mail-live)
                                              (functionp (key-binding (kbd ", l"))))))
                                  :got (format "mode %S, labels %S, , / %S"
                                               (buffer-local-value 'mail-live-collect-mode collect)
                                               (collect-labels collect)
                                               (with-current-buffer collect (key-binding (kbd ", /")))))
                          (let* ((before (search-summaries))
                                 (err (on-line collect "RET"))
                                 (view (get-buffer mail-thread-buffer-name)))
                            (record "RET on a collected line reads its thread in the thread view, at its newest match"
                                    (and (null err) view (get-buffer-window view)
                                         (equal (with-current-buffer view (subject-at-point))
                                                "Re: live plan (Ann)"))
                                    :got (format "%S, view %S at %S" err (and view (get-buffer-window view))
                                                 (and view (with-current-buffer view
                                                             (ignore-errors (subject-at-point))))))
                            (record "each message of the thread view shows its labels on its line"
                                    (equal (thread-labels) '("   Inbox " "   Inbox " "   Inbox "))
                                    :got (format "%S" (thread-labels)))
                            ;; a label added elsewhere: Gnus files a copy into it
                            (when-let* ((root (seq-find (lambda (file)
                                                          (string-match-p "/1700000060\\.60\\." file))
                                                        (messages-in inbox))))
                              (with-temp-buffer
                                (insert-file-contents root)
                                (gnus-request-accept-article "nnmaildir+gmail:labelled" nil t t)))
                            (email-e2e--idle 5 (lambda () (member "   Inbox   labelled " (thread-labels))))
                            (record "a label added to a message shows at once on its thread view line and its collected line"
                                    (and (equal (thread-labels)
                                                '("   Inbox   labelled " "   Inbox " "   Inbox "))
                                         (equal (collect-labels collect) "  Inbox   labelled  "))
                                    :got (format "%S, collected %S" (thread-labels)
                                                 (collect-labels collect)))
                            (when-let* ((window (and view (get-buffer-window view))))
                              (with-selected-window window
                                (execute-kbd-macro "q")))
                            (leave-summaries before))
                          (let* ((before (search-summaries))
                                 (err (on-line collect "C-; s"))
                                 (summary (new-summary before)))
                            (record "C-; s on a collected line reads every message of its thread in a summary"
                                    (and (null err)
                                         (equal (summary-ids summary) (funcall message-ids live-ids)))
                                    :got (format "%S, %S" err (summary-ids summary)))
                            (leave-summaries before))
                          (let* ((browsed nil)
                                 (err (cl-letf (((symbol-function 'browse-url)
                                                 (lambda (url &rest _) (setq browsed url))))
                                        (on-line collect "C-; g"))))
                            (record "C-; g on a collected line opens its newest match in Gmail"
                                    (and (null err)
                                         (equal browsed (concat "https://mail.google.com/mail/u/0/#search/"
                                                                "rfc822msgid%3Alive-plan-ann%40fixture.example")))
                                    :got (format "%S, %S" err browsed)))
                          (let* ((browsed nil)
                                 (before (search-summaries))
                                 (err (cl-letf (((symbol-function 'browse-url)
                                                 (lambda (url &rest _) (setq browsed url))))
                                        (on-line collect ", o g"))))
                            (record ", o g on a collected line opens its newest match in Gmail, reading nothing"
                                    (and (null err)
                                         (string-suffix-p "rfc822msgid%3Alive-plan-ann%40fixture.example"
                                                          (or browsed ""))
                                         (null (new-summary before)))
                                    :got (format "%S, %S, summary %S" err browsed (new-summary before))))
                          (let* ((before (search-summaries))
                                 (composing (buffer-list))
                                 (err (on-line collect ", f"))
                                 (forward (seq-find (lambda (buffer)
                                                      (and (not (memq buffer composing))
                                                           (eq (buffer-local-value 'major-mode buffer)
                                                               'message-mode)))
                                                    (buffer-list))))
                            (record ", f on a collected line reads its thread, then forwards the newest match"
                                    (and (null err) forward
                                         (string-match-p "live plan (Ann)"
                                                         (with-current-buffer forward
                                                           (or (message-fetch-field "Subject") "")))
                                         (new-summary before))
                                    :got (format "%S, %S" err
                                                 (and forward (with-current-buffer forward
                                                                (message-fetch-field "Subject")))))
                            (when forward
                              (with-current-buffer forward
                                (set-buffer-modified-p nil)
                                (let ((kill-buffer-query-functions nil))
                                  (kill-buffer forward))))
                            (when-let* ((window (get-buffer-window mail-thread-buffer-name)))
                              (with-selected-window window
                                (execute-kbd-macro "q")))
                            (leave-summaries before))
                          (kill-buffer collect)))
                      (delete-other-windows)
                      (switch-to-buffer gnus-group-buffer)
                      ;; M-m selects the first line, the export thread
                      (let* ((before (search-summaries))
                             (stage (live "live SPC export" 2 "M-m C-c C-;"))
                             (summary (email-e2e--idle 10 (lambda () (exported before)))))
                        (record "C-c C-; on the line M-m selected reads that thread alone, whole"
                                (equal (summary-ids summary) (funcall message-ids export-ids))
                                :got (format "stage %S, %S" stage (summary-ids summary)))
                        (leave-summaries before)
                        ;; a stand-in left behind would hold the next case back
                        (dolist (buffer (buffer-list))
                          (when (buffer-local-value 'mail-live-export buffer)
                            (kill-buffer buffer))))
                      (delete-other-windows)
                      (switch-to-buffer gnus-group-buffer)
                      (let* ((before (search-summaries))
                             (stage (live "live SPC export" 2 "C-c C-;"))
                             (summary (email-e2e--idle 10 (lambda () (exported before))))
                             (renamed (seq-filter (lambda (buffer)
                                                    (string-prefix-p "*Embark Export" (buffer-name buffer)))
                                                  (buffer-list))))
                        (record "C-c C-; reads every listed thread, whole, in one summary shown under Gnus's name"
                                (and summary
                                     (equal (summary-ids summary)
                                            (funcall message-ids (append export-ids live-ids)))
                                     (equal (buffer-name summary)
                                            (with-current-buffer summary
                                              (gnus-summary-buffer-name gnus-newsgroup-name)))
                                     (get-buffer-window summary)
                                     (null renamed))
                                :got (format "stage %S, %S named %S, shown %S, left %S" stage
                                             (summary-ids summary) (and summary (buffer-name summary))
                                             (and summary (get-buffer-window summary)) renamed))
                        (leave-summaries before)
                        (mapc #'kill-buffer renamed)))))
                ;; the group buffer's own localleader over topics and
                ;; subscriptions; moved is a group mail-topics leaves out
                (delete-other-windows)
                (switch-to-buffer gnus-group-buffer)
                (gnus-group-list-groups)
                (gnus-group-jump-to-group "nnmaildir+gmail:moved")
                (execute-kbd-macro (kbd ", m Labels RET"))
                (gnus-group-jump-to-group "nnmaildir+gmail:inbox")
                (execute-kbd-macro (kbd ", m Lists RET"))
                (record ", m moves the group at point into another topic"
                        (equal (list (gnus-group-topic "nnmaildir+gmail:moved")
                                     (gnus-group-topic "nnmaildir+gmail:inbox"))
                               '("Labels" "Lists"))
                        :got (format "%S" gnus-topic-alist))
                ;; what every start runs
                (apply-mail-topics)
                (record "the next start takes back a group mail-topics names, and keeps one it leaves out"
                        (and (equal (cdr (assoc "Gmail" gnus-topic-alist))
                                    '("nnmaildir+gmail:inbox" "nnmaildir+gmail:archive"
                                      "nnmaildir+gmail:trash"))
                             ;; html and travel went under Labels when subscribed
                             (equal (cdr (assoc "Labels" gnus-topic-alist))
                                    '("nnmaildir+gmail:starred" "nnmaildir+gmail:labelled"
                                      "nnmaildir+gmail:html" "nnmaildir+gmail:travel"
                                      "nnmaildir+gmail:moved")))
                        :got (format "%S" gnus-topic-alist))
                (gnus-group-jump-to-group "nnmaildir+gmail:moved")
                (execute-kbd-macro (kbd ", t n Scratch RET"))
                (record ", t n makes a topic under the one at point"
                        (equal (gnus-topic-parent-topic "Scratch") "Labels")
                        :got (format "%S" gnus-topic-topology))
                ;; the prompt starts from the old name
                (gnus-topic-goto-topic "Scratch")
                (execute-kbd-macro (kbd ", t r Later RET"))
                (record ", t r renames the topic at point"
                        (and (gnus-topic-find-topology "ScratchLater")
                             (not (gnus-topic-find-topology "Scratch")))
                        :got (format "%S" gnus-topic-topology))
                (gnus-topic-goto-topic "ScratchLater")
                (execute-kbd-macro (kbd ", t d"))
                (record ", t d deletes the empty topic at point"
                        (not (gnus-topic-find-topology "ScratchLater"))
                        :got (format "%S" gnus-topic-topology))
                (gnus-group-jump-to-group "nnmaildir+gmail:moved")
                (execute-kbd-macro (kbd ", d"))
                (record ", d unsubscribes the group at point"
                        (< gnus-level-subscribed (gnus-group-level "nnmaildir+gmail:moved"))
                        :got (format "level %S" (gnus-group-level "nnmaildir+gmail:moved")))
                ;; the line point is on decides the topic
                (gnus-group-jump-to-group "nnmaildir+gmail:lists")
                (execute-kbd-macro (kbd ", a moved RET"))
                (record ", a subscribes a label found by a word of its name, into the topic at point"
                        (and (<= (gnus-group-level "nnmaildir+gmail:moved") gnus-level-subscribed)
                             (equal (gnus-group-topic "nnmaildir+gmail:moved") "Lists")
                             (equal (gnus-group-group-name) "nnmaildir+gmail:moved"))
                        :got (format "level %S in %S, point on %S"
                                     (gnus-group-level "nnmaildir+gmail:moved")
                                     (gnus-group-topic "nnmaildir+gmail:moved")
                                     (gnus-group-group-name)))
                ;; the saved group list offers gmane.other; the stand-in
                ;; holds the fetch that subscribing starts
                (delete-file news-go)
                (gnus-group-jump-to-group "nnmaildir+gmail:lists")
                (execute-kbd-macro (kbd ", a gmane.other RET"))
                ;; the fetch waits for the batch reading the new group
                (loads-done)
                (let ((other "nnmaildir+news:gmane.other"))
                  (record ", a subscribes a news group of the saved list, makes its folder and fetches"
                          (and (eql (gnus-group-level other) (1+ gnus-activate-level))
                               (equal (gnus-group-topic other) "Lists")
                               (file-directory-p (expand-file-name "gmane.other/cur" news-root))
                               (process-live-p news-fetch-process))
                          :got (format "level %S in %S, folder %S, fetching %S"
                                       (gnus-group-level other) (gnus-group-topic other)
                                       (file-directory-p
                                        (expand-file-name "gmane.other/cur" news-root))
                                       (process-live-p news-fetch-process))))
                ;; a label that appeared after the start, its batch held
                (let ((fresh (expand-file-name "fresh/" root)))
                  (dolist (sub '("cur" "new" "tmp"))
                    (make-directory (expand-file-name sub fresh) t))
                  (write-region "" nil load-hold)
                  ;; what a search does first
                  (subscribe-mail-groups)
                  (load-unloaded-mail-groups)
                  (gnus-group-jump-to-group "nnmaildir+gmail:lists")
                  (execute-kbd-macro (kbd "RET"))
                  (gnus-summary-goto-subject (email-e2e--article "list plan"))
                  (let ((said (cl-letf (((symbol-function 'gnus-read-move-group-name)
                                         (lambda (&rest _) "nnmaildir+gmail:fresh")))
                                (saying ", m"))))
                    (record ", m into a label still loading moves nothing and says why"
                            (and (null (messages-in fresh))
                                 (member "<list-plan@fixture.example>"
                                         (message-ids (messages-in lists)))
                                 (seq-some (lambda (line) (string-match-p "still loading" line))
                                           said))
                            :got (format "fresh %S, said %S" (messages-in fresh) said)))
                  (when (derived-mode-p 'gnus-summary-mode)
                    (gnus-summary-exit-no-update))
                  (delete-file load-hold)
                  (loads-done)
                  (record "a label that appeared after the start loads once its batch runs"
                          (member "fresh" (read-groups))
                          :got (format "%S" (read-groups))))
                ;; a sync while Gnus runs: a label made in Gmail arrives as
                ;; a folder, and the sync gives it a group
                (let ((arrived (expand-file-name "arrived/" root))
                      (said nil))
                  (dolist (sub '("cur" "new" "tmp"))
                    (make-directory (expand-file-name sub arrived) t))
                  (email-e2e--write-message (expand-file-name "cur/1700000080.80.fixture:2," arrived)
                                            "Ivy <ivy@example.com>" "arrived" "arrived")
                  (let ((err (condition-case err (progn (synced) nil) (error err))))
                    (loads-done)
                    (with-current-buffer gnus-group-buffer
                      (gnus-group-list-groups))
                    (record "a label made in Gmail gets its group under Labels at the next sync, read"
                            (and (null err)
                                 (equal (gnus-group-topic "nnmaildir+gmail:arrived") "Labels")
                                 (member "arrived" (read-groups))
                                 (eql (gnus-group-unread "nnmaildir+gmail:arrived") 1)
                                 (assoc "nnmaildir+gmail:arrived" (seq-remove #'stringp (group-lines))))
                            :got (format "err %S, topic %S, read %S, unread %S" err
                                         (gnus-group-topic "nnmaildir+gmail:arrived")
                                         (read-groups)
                                         (gnus-group-unread "nnmaildir+gmail:arrived"))))
                  ;; Gmail dropped it again, and its folder went aside
                  (rename-file (directory-file-name arrived) (expand-file-name "arrived" removed))
                  (let* ((note (lambda (format &rest args)
                                 (when format
                                   (push (apply #'format-message format args) said))))
                         (err (progn
                                (advice-add 'message :before note)
                                (unwind-protect
                                    (condition-case err (progn (synced) nil) (error err))
                                  (advice-remove 'message note)))))
                    (record "a label dropped in Gmail leaves the group buffer and the chips at the next sync"
                            (and (null err)
                                 (not (gnus-group-entry "nnmaildir+gmail:arrived"))
                                 (not (assoc "nnmaildir+gmail:arrived"
                                             (seq-remove #'stringp (group-lines))))
                                 (not (member "arrived" (read-groups)))
                                 (not (assoc "nnmaildir+gmail:arrived" (mail-label-tables)))
                                 (member "Groups gone with their folders: arrived" said)
                                 (not (seq-some (lambda (line) (string-match-p "Could not" line))
                                                said)))
                            :got (format "err %S, entry %S, read %S, said %S" err
                                         (and (gnus-group-entry "nnmaildir+gmail:arrived") t)
                                         (read-groups) said))))
                ;; labels made, renamed and deleted from the group buffer,
                ;; each change in Gmail through the curl stand-in
                (let ((mail-sync-config label-config))
                  (delete-other-windows)
                  (switch-to-buffer gnus-group-buffer)
                  (gnus-group-jump-to-group "nnmaildir+gmail:lists")
                  (let ((minibuffer-history nil))
                    (execute-kbd-macro (kbd ", l n made RET")))
                  (loads-done)
                  (record ", l n makes a label's folder, and its group under Labels with point on it"
                          (and (file-directory-p (expand-file-name "made/cur" root))
                               (equal (gnus-group-topic "nnmaildir+gmail:made") "Labels")
                               (equal (gnus-group-group-name) "nnmaildir+gmail:made")
                               ;; Gmail gets it at the next sync, not from Emacs
                               (null (imap-requests)))
                          :got (format "folder %S, topic %S, point on %S, imap %S"
                                       (file-directory-p (expand-file-name "made" root))
                                       (gnus-group-topic "nnmaildir+gmail:made")
                                       (gnus-group-group-name) (imap-requests)))
                  ;; a sync took it to Gmail, which numbers its messages
                  ;; under UIDVALIDITY 7
                  (write-region "made\n" nil gmail-boxes t)
                  (write-region "FarUidValidity 7\n" nil (expand-file-name "made/.mbsyncstate" root))
                  ;; in a topic of its own, which the rename keeps
                  (execute-kbd-macro (kbd ", m Lists RET"))
                  (gnus-group-jump-to-group "nnmaildir+gmail:made")
                  ;; Gnus's prompt starts from the old name, point before it
                  (let ((minibuffer-history nil)
                        (gnus-group-history nil))
                    (execute-kbd-macro (kbd "r 2 RET")))
                  (label-change-done)
                  (loads-done)
                  (record "r renames a label in Gmail, keeps its synced folder, and its group its topic"
                          (and (equal (imap-requests)
                                      '("RENAME \"made\" \"2made\"" "STATUS \"2made\" (UIDVALIDITY)"))
                               (file-exists-p (expand-file-name "2made/.mbsyncstate" root))
                               (file-directory-p (expand-file-name "2made/cur" root))
                               (not (file-exists-p (expand-file-name "made" root)))
                               (not (gnus-group-entry "nnmaildir+gmail:made"))
                               (equal (gnus-group-topic "nnmaildir+gmail:2made") "Lists"))
                          :got (format "imap %S, folders %S, made %S, 2made in %S"
                                       (imap-requests) (directory-files root nil "made")
                                       (and (gnus-group-entry "nnmaildir+gmail:made") t)
                                       (gnus-group-topic "nnmaildir+gmail:2made")))
                  (gnus-group-jump-to-group "nnmaildir+gmail:2made")
                  (let ((use-short-answers t))
                    (execute-kbd-macro (kbd ", l d y")))
                  (label-change-done)
                  (loads-done)
                  (let ((aside (expand-file-name "removed/" mail-sync-directory)))
                    (record ", l d deletes a label in Gmail once asked, and its group goes"
                            (and (member "DELETE \"2made\"" (imap-requests))
                                 (file-directory-p aside)
                                 (directory-files aside nil "\\`2made-")
                                 (not (file-exists-p (expand-file-name "2made" root)))
                                 (not (gnus-group-entry "nnmaildir+gmail:2made"))
                                 (not (gnus-group-topic "nnmaildir+gmail:2made")))
                            :got (format "imap %S, aside %S, entry %S, topic %S"
                                         (imap-requests)
                                         (and (file-directory-p aside) (directory-files aside))
                                         (and (gnus-group-entry "nnmaildir+gmail:2made") t)
                                         (gnus-group-topic "nnmaildir+gmail:2made")))))
                ;; Org in the body, sent through real keys
                (email-e2e--org-compose #'record)
                ;; C-c C-c queues, the queue sends, e takes a message back
                (email-e2e--delayed-send #'record)
                ;; , q leaves mail: an unsent reply goes to the drafts, the
                ;; summary closes the way q closes it, and Gnus exits.  The
                ;; drafts server is open by now, so binding its folder moves it
                (let* ((drafts (file-name-as-directory (expand-file-name "drafts" e2e-work-dir)))
                       (nndraft-directory drafts)
                       (nnmh-directory drafts)
                       (nndraft-current-group nil))
                  (make-directory (expand-file-name "drafts" drafts) t)
                  (delete-other-windows)
                  (switch-to-buffer gnus-group-buffer)
                  (gnus-group-jump-to-group "nnmaildir+gmail:inbox")
                  (execute-kbd-macro (kbd "RET"))
                  (let ((summary (current-buffer)))
                    (goto-char (point-min))
                    (execute-kbd-macro (kbd "RET"))
                    (execute-kbd-macro "r")
                    (let ((composed (derived-mode-p 'message-mode))
                          start elapsed)
                      (goto-char (point-max))
                      (insert "a reply left unsent")
                      (switch-to-buffer summary)
                      (setq start (float-time))
                      (execute-kbd-macro (kbd ", q"))
                      (setq elapsed (- (float-time) start))
                      (let ((left (seq-filter (lambda (buffer)
                                                (with-current-buffer buffer
                                                  (derived-mode-p '(gnus-group-mode gnus-summary-mode
                                                                    gnus-article-mode mail-thread-mode
                                                                    message-mode))))
                                              (buffer-list)))
                            (saved (directory-files (expand-file-name "drafts" drafts) t
                                                    "\\`[0-9]+\\'")))
                        (record ", q saves the unsent reply as a draft, then leaves no Gnus buffer behind"
                                (and composed
                                     (not (gnus-alive-p))
                                     (null left)
                                     (= 1 (length saved))
                                     (string-match-p "a reply left unsent"
                                                     (with-temp-buffer
                                                       (insert-file-contents (car saved))
                                                       (buffer-string)))
                                     (< elapsed 0.5))
                                :got (format "composed %S, alive %S, left %S, drafts %S, %.2f s"
                                             composed (gnus-alive-p) left saved elapsed)))))))
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
        ;; a batch left reading, and the work waiting on it
        (when (fboundp 'stop-mail-load)
          (stop-mail-load))
        (when (file-exists-p load-hold)
          (delete-file load-hold))
        ;; a feed download or a news fetch left waiting for go, and the
        ;; periodic download
        (dolist (process (process-list))
          (when (or (string-prefix-p "feed " (process-name process))
                    (string-prefix-p "news-fetch" (process-name process))
                    (string-prefix-p "reddit-fetch" (process-name process)))
            (delete-process process)))
        (when (timerp (bound-and-true-p feed-fetch-timer))
          (cancel-timer feed-fetch-timer)
          (setq feed-fetch-timer nil))
        (when (gnus-alive-p)
          ;; a search summary a case left behind, which returns to the
          ;; summary it was opened from
          (dolist (summary (buffer-list))
            (when (string-prefix-p "*Summary nnselect:" (buffer-name summary))
              (with-current-buffer summary
                (gnus-summary-exit-no-update t))))
          ;; a live summary makes gnus-group-exit ask whether to update it
          (dolist (name '("*Summary nnmaildir+gmail:inbox*" "*Summary nnmaildir+gmail:html*"
                          "*Summary nnmaildir+gmail:starred*" "*Summary nnmaildir+gmail:lists*"))
            (when-let* ((summary (get-buffer name)))
              (with-current-buffer summary
                (gnus-summary-exit-no-update))))
          (dolist (group (list feed-group news-group reddit-group))
            (when-let* ((summary (get-buffer (gnus-summary-buffer-name group))))
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
