;;; modules/email/autoload/similar.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Find mail like the message on screen, wider than Gmail's "filter
;; messages like these": notmuch queries built from its headers, counted
;; in one notmuch process, offered with their counts, and opened as the
;; ephemeral search group `search-mail' reads.
;;; Code:

(require 'gnus)
(require 'gnus-search)
(require 'gnus-sum)
(require 'mail-extr)
(require 'mail-utils)
(require 'rfc2047)
(require 'seq)
(require 'url-util)

(defvar gmail-maildir)
(defvar mail-from-address)
(defvar mail-inbox-address)
(defvar mail-inbox-group)
(defvar mail-archive-group)
(defvar mail-trash-group)
(defvar gnus-message-archive-group)

(defvar webmail-domains
  '("gmail.com" "googlemail.com" "yahoo.com" "hotmail.com" "outlook.com"
    "live.com" "icloud.com" "me.com" "mac.com" "aol.com" "proton.me"
    "protonmail.com" "gmx.com" "gmx.de" "mail.ru" "yandex.ru")
  "Domains shared by strangers, too broad to find mail by.")

;;; The queries

(defun notmuch-term (prefix value)
  "PREFIX:VALUE for a notmuch query, VALUE quoted and on one line.
notmuch counts a batch of queries one line each."
  (format "%s:\"%s\"" prefix
          (string-replace "\"" "\"\"" (replace-regexp-in-string "[ \t\n\r]+" " " value))))

(defun own-address-p (address)
  "Non-nil when ADDRESS is one of the account's own."
  (member (downcase address) (list mail-from-address mail-inbox-address)))

(defun no-reply-address-p (address)
  "Non-nil when ADDRESS reads as one nobody answers."
  (string-match-p "no[-._]?reply\\|do[-._]?not[-._]?reply\\|mailer-daemon"
                  (car (split-string address "@"))))

(defun other-party (from to)
  "Address of whoever the account corresponds with in a message FROM, TO.
That is the sender, or the first recipient when the account sent it."
  (let ((sender (cadr (mail-extract-address-components (or from "")))))
    (if (and sender (own-address-p sender))
        (seq-some (lambda (recipient)
                    (unless (own-address-p (cadr recipient))
                      (cadr recipient)))
                  (mail-extract-address-components (or to "") t))
      sender)))

(defun address-domain (address)
  "The part of ADDRESS's domain a company's mail shares."
  (when-let* ((host (cadr (split-string address "@"))))
    (or (url-domain (url-generic-parse-url (concat "http://" host)))
        host)))

(defun simplified-subject (subject)
  "SUBJECT without its reply and forward prefixes and its quotes.
A quote would end a notmuch phrase early."
  (let ((case-fold-search t))
    (string-trim
     (replace-regexp-in-string
      "\"" ""
      (replace-regexp-in-string
       "\\`\\(?:[ \t]*\\(?:re\\|fwd?\\|aw\\|sv\\)\\(?:\\[[0-9]+\\]\\)?[ \t]*:\\)+"
       "" subject)))))

(defun list-id-value (list-id)
  "The identifier in LIST-ID's angle brackets, or LIST-ID when it has none."
  (if (string-match "<\\([^>]+\\)>" list-id)
      (match-string 1 list-id)
    (string-trim list-id)))

(defun similar-mail-queries (message)
  "Notmuch queries for mail like MESSAGE, as (KIND . QUERY), likeliest first.
MESSAGE is a plist of :from, :to, :subject, :list-id, :bulk and :folders.
A list post leads with its list, bulk mail with the sender's domain,
anything else with the correspondent."
  (pcase-let* (((map :from :to :subject :list-id :bulk :folders) message)
               (party (other-party from to))
               (domain (and party (address-domain party)))
               (subject (simplified-subject (or subject "")))
               (first (cond (list-id 'list)
                            ((or bulk (and party (no-reply-address-p party))) 'domain)
                            (t 'both-ways)))
               (candidates
                (delq nil
                      (list
                       ;; ahead of both-ways, so a tie keeps the simpler query
                       (and party (cons 'from (notmuch-term "from" party)))
                       (and party
                            (cons 'both-ways (format "%s or %s" (notmuch-term "from" party)
                                                     (notmuch-term "to" party))))
                       (and list-id (cons 'list (notmuch-term "List" (list-id-value list-id))))
                       ;; a list host's domain spans every list it hosts
                       (and domain (not list-id) (not (member domain webmail-domains))
                            (cons 'domain (notmuch-term "from" domain)))
                       (and (not (string-empty-p subject))
                            (cons 'subject (notmuch-term "subject" subject)))
                       (and party
                            (cons 'attachments (concat (notmuch-term "from" party)
                                                       " and tag:attachment")))))))
    (append (seq-filter (lambda (candidate) (eq (car candidate) first)) candidates)
            (seq-remove (lambda (candidate) (eq (car candidate) first)) candidates)
            (mapcar (lambda (folder) (cons 'label (notmuch-term "folder" folder)))
                    folders))))

(defun useful-mail-queries (candidates counts)
  "CANDIDATES that find other mail, as (QUERY KIND COUNT), one per count.
COUNTS holds each candidate's count.  Two queries with the same count
nearly always find the same messages, so only the first stays."
  (let (seen choices)
    (seq-mapn (lambda (candidate count)
                (unless (or (< count 2) (memql count seen))
                  (push count seen)
                  (push (list (cdr candidate) (car candidate) count) choices)))
              candidates counts)
    (nreverse choices)))

;;; notmuch

(defun notmuch-args (&rest args)
  "ARGS for the notmuch that `gnus-search' runs, after its config switch."
  (cons (format "--config=%s" gnus-search-notmuch-config-file) args))

(defun count-mail (queries)
  "How many messages notmuch finds for each of QUERIES, from one process."
  (with-temp-buffer
    (insert (string-join queries "\n") "\n")
    (let ((status (apply #'call-process-region (point-min) (point-max)
                         gnus-search-notmuch-program t '(t nil) nil
                         (notmuch-args "count" "--batch"))))
      (unless (eql status 0)
        (error "Counting mail with notmuch failed with status %s" status))
      (mapcar #'string-to-number (split-string (buffer-string) "\n" t)))))

(defun mail-folders (message-id)
  "Label folders holding a copy of MESSAGE-ID, as notmuch names them.
Inbox, archive, sent and trash are left out: every message sits in one."
  (let* ((root (file-name-as-directory (expand-file-name gmail-maildir)))
         (store (file-name-nondirectory (directory-file-name root)))
         (skip (mapcar (lambda (group) (cadr (split-string group ":")))
                       (list mail-inbox-group mail-archive-group mail-trash-group
                             gnus-message-archive-group))))
    (seq-uniq
     (seq-keep (lambda (file)
                 (when (string-prefix-p root file)
                   (let ((label (car (split-string (substring file (length root)) "/"))))
                     (unless (member label skip)
                       (concat store "/" label)))))
               (apply #'process-lines gnus-search-notmuch-program
                      (notmuch-args "search" "--output=files"
                                    (notmuch-term "id" (string-trim message-id "<" ">"))))))))

;;; The command

(defun similar-mail-message ()
  "The message on screen, as `similar-mail-queries' reads it."
  (pcase-let* ((`(,summary . ,article) (mail-on-screen))
               (header (with-current-buffer summary
                         (gnus-summary-article-header article))))
    (with-temp-buffer
      (when (with-current-buffer summary
              (gnus-request-head article gnus-newsgroup-name))
        (insert-buffer-substring nntp-server-buffer))
      (list :from (rfc2047-decode-string (mail-header-from header))
            :to (or (mail-fetch-field "To")
                    (cdr (assq 'To (mail-header-extra header))))
            :subject (rfc2047-decode-string (mail-header-subject header))
            :list-id (mail-fetch-field "List-Id")
            :bulk (and (mail-fetch-field "List-Unsubscribe") t)
            :folders (mail-folders (mail-header-id header))))))

(defun read-similar-mail-query (choices)
  "Read a query, offering CHOICES of (QUERY KIND COUNT) in their order."
  (let* ((width (apply #'max (mapcar (lambda (choice) (string-width (car choice)))
                                     choices)))
         (annotate (lambda (query)
                     (pcase-let ((`(,_ ,kind ,count) (assoc query choices)))
                       (concat (propertize " " 'display `(space :align-to ,(+ width 3)))
                               (format "%-12s %6d" kind count))))))
    (completing-read "Mail like this: "
                     (lambda (string predicate action)
                       (if (eq action 'metadata)
                           `(metadata (display-sort-function . identity)
                                      (cycle-sort-function . identity)
                                      (annotation-function . ,annotate))
                         (complete-with-action action choices string predicate)))
                     nil nil nil nil (caar choices))))

;;;###autoload
(defun search-mail-like-this (&optional edit)
  "Search all mail for messages like the one on screen, picked from a list.
With EDIT, the picked query goes to the minibuffer first."
  (interactive "P" gnus-summary-mode mail-thread-mode gnus-article-mode)
  (let* ((candidates (similar-mail-queries (similar-mail-message)))
         (choices (useful-mail-queries candidates
                                       (count-mail (mapcar #'cdr candidates)))))
    (unless choices
      (user-error "No other mail is like this message"))
    (let ((query (read-similar-mail-query choices)))
      (search-mail (if edit (read-string "Search mail: " query) query)))))

;;; similar.el ends here
