;;; modules/email/autoload/live.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Search all mail as you type: notmuch lists the threads matching the
;; query from a process of its own, C-SPC previews a thread's newest
;; match, and RET reads the thread in the thread view through a search
;; group, so every key there acts on the inbox copy.
;;; Code:

(require 'consult)
(require 'gnus)
(require 'gnus-search)
(require 'gnus-sum)
(require 'mail-parse)
(require 'mail-utils)
(require 'mm-decode)
(require 'rfc2047)

(defvar mail-inbox-group)
(defvar mail-search-limit-max)

(defvar mail-live-limit 100
  "Most threads a live search lists.")

(defvar mail-live-history nil
  "Queries of past live searches.")

(defvar mail-live-query nil
  "The query the live search sent notmuch last.")

(defvar mail-live-preview-name " *mail preview*"
  "Buffer the live search previews a message in.")

;;; Listing threads

(defun mail-live-notmuch (&rest args)
  "Command line of the notmuch gnus-search runs, with ARGS."
  (append (list gnus-search-notmuch-program
                (format "--config=%s" gnus-search-notmuch-config-file))
          args))

(defun mail-live-command (input)
  "Command line listing the threads INPUT matches, newest first, or nil."
  (unless (string-blank-p input)
    (setq mail-live-query input)
    (mail-live-notmuch "search" "--format=sexp" "--output=summary"
                       (format "--limit=%d" mail-live-limit) input)))

(defun mail-live-thread (line)
  "The thread summary on LINE of notmuch's sexp output, or nil.
The output is one list with a thread per line, so the first line opens
the list as well."
  (let ((thread (condition-case nil
                    (car (read-from-string line (if (string-prefix-p "((" line) 1 0)))
                  ((invalid-read-syntax end-of-file) nil))))
    (and (consp thread) (plist-get thread :thread) thread)))

(defun mail-live-date (time)
  "TIME, in seconds since the epoch, as summary lines show a date."
  (gnus-user-date (format-time-string "%a, %d %b %Y %T %z" time)))

(defun mail-live-candidate (thread query)
  "THREAD, a notmuch thread summary QUERY found, as a line that carries both."
  (let ((tags (plist-get thread :tags))
        (total (or (plist-get thread :total) 1)))
    (propertize
     (concat (if (member "unread" tags) "●" " ")
             (if (member "flagged" tags) "∗" " ")
             " "
             (propertize (truncate-string-to-width
                          (mail-live-date (plist-get thread :timestamp)) 16 nil ?\s)
                         'face 'shadow)
             " "
             ;; notmuch sets the authors of matching messages apart with |
             (truncate-string-to-width
              (string-replace "|" "," (or (plist-get thread :authors) "")) 24 nil ?\s)
             " "
             (if (< 1 total)
                 (propertize (format "%d/%d " (plist-get thread :matched) total) 'face 'shadow)
               "")
             (or (plist-get thread :subject) ""))
     'mail-thread thread 'mail-query query)))

(defun mail-live-candidates (lines)
  "Candidates from LINES of notmuch's output, one per thread."
  (seq-keep (lambda (line)
              (when-let* ((thread (mail-live-thread line)))
                (mail-live-candidate thread mail-live-query)))
            lines))

(defun mail-live-candidate-threads (candidates)
  "The threads CANDIDATES, lines of the live search, carry."
  (seq-keep (lambda (candidate) (get-text-property 0 'mail-thread candidate))
            candidates))

;;; The newest match of a thread

(defun mail-live-in-thread (thread query)
  "The notmuch query for the messages of THREAD that QUERY matches."
  (format "thread:%s and (%s)" (plist-get thread :thread) query))

(defun mail-live-newest-id (thread query)
  "Message-ID of THREAD's newest message QUERY matches, or nil."
  (when-let* ((line (car (apply #'process-lines-ignore-status
                                (mail-live-notmuch "search" "--output=messages"
                                                   "--sort=newest-first" "--limit=1"
                                                   (mail-live-in-thread thread query))))))
    (concat "<" (string-trim (string-remove-prefix "id:" line) "\"" "\"") ">")))

(defun mail-live-thread-ids (threads)
  "The id: term of each message of THREADS, newest first.
A search shows no more than `mail-search-limit-max' messages, and the
cap keeps the query of their ids within one argument's length."
  (apply #'process-lines-ignore-status
         (mail-live-notmuch "search" "--output=messages" "--sort=newest-first"
                            (format "--limit=%d" mail-search-limit-max)
                            (mapconcat (lambda (thread) (concat "thread:" (plist-get thread :thread)))
                                       threads " or "))))

(defun mail-live-newest-file (thread query)
  "File of THREAD's newest message QUERY matches, under its name now, or nil."
  (when-let* ((file (car (apply #'process-lines-ignore-status
                                (mail-live-notmuch "search" "--output=files" "--duplicate=1"
                                                   "--sort=newest-first" "--limit=1"
                                                   (mail-live-in-thread thread query))))))
    (current-mail-file file (make-hash-table :test #'equal))))

;;; Preview

(defun mail-live-preview-text (file)
  "The message in FILE as the preview shows it: its headers, then its body."
  (with-temp-buffer
    (let ((coding-system-for-read mm-text-coding-system))
      (mm-insert-file-contents file))
    (let ((headers (save-restriction
                     (mail-narrow-to-head)
                     (mapconcat (lambda (field)
                                  (if-let* ((value (mail-fetch-field field)))
                                      (concat (propertize (concat field ": ") 'face 'gnus-header-name)
                                              (rfc2047-decode-string
                                               (replace-regexp-in-string "\n[ \t]+" " " value))
                                              "\n")
                                    ""))
                                '("From" "To" "Cc" "Date" "Subject") "")))
          ;; group parameters are read through the newsrc, which a Gnus
          ;; that never started lacks
          (gnus-newsrc-hashtb (or gnus-newsrc-hashtb (gnus-make-hashtable))))
      (concat headers "\n"
              (mail-render-message (current-buffer)
                                   (or (mail-file-group file) mail-inbox-group))))))

(defun mail-live-preview-buffer (thread query)
  "Buffer showing the newest message of THREAD that QUERY matches."
  (with-current-buffer (get-buffer-create mail-live-preview-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (if-let* ((file (mail-live-newest-file thread query)))
                  (mail-live-preview-text file)
                "This message is no longer in the store."))
      (goto-char (point-min)))
    (setq buffer-read-only t)
    (visual-line-mode)
    (current-buffer)))

(defun mail-live-preview-window ()
  "The window beside the one the search started from, or that one alone."
  (let ((origin (or (minibuffer-selected-window) (selected-window))))
    (or (seq-find (lambda (window) (not (eq window origin)))
                  (window-list (window-frame origin) 'nomini origin))
        origin)))

(defun mail-live-preview-state ()
  "Consult state that previews a thread beside where the search started.
The window gets its buffer back when the search ends."
  (let (window before prev next)
    (lambda (action cand)
      (pcase action
        ('preview
         (when-let* ((thread (and cand (get-text-property 0 'mail-thread cand))))
           (unless (window-live-p window)
             (setq window (mail-live-preview-window)
                   before (window-buffer window)
                   prev (copy-sequence (window-prev-buffers window))
                   next (copy-sequence (window-next-buffers window))))
           (set-window-buffer window (mail-live-preview-buffer thread mail-live-query))))
        ('exit
         (when (window-live-p window)
           (set-window-buffer window before)
           (set-window-prev-buffers window prev)
           (set-window-next-buffers window next))
         (when-let* ((buffer (get-buffer mail-live-preview-name)))
           (kill-buffer buffer)))))))

;;; Reading threads

(defun read-mail-live-threads (threads then)
  "Read every message of THREADS in a search group, then call THEN.
THEN gets the group's summary, or nil when none of THREADS is in the store."
  (when-mail-loaded
   (if (cdr threads) "for the threads" "for the thread")
   (lambda ()
     (funcall then
              ;; gnus-search takes thread:X in a query for a key of its own
              (when-let* ((terms (mail-live-thread-ids threads))
                          (group (search-mail-now (string-join terms " or ") nil)))
                (get-buffer (gnus-summary-buffer-name group)))))))

(defun open-mail-live-thread (thread query)
  "Read THREAD in the thread view, at its newest message QUERY matches."
  (let ((id (mail-live-newest-id thread query)))
    (read-mail-live-threads
     (list thread)
     (lambda (summary)
       (when summary
         (with-current-buffer summary
           (when-let* ((article (and id (gnus-id-to-article id))))
             (gnus-summary-goto-subject article))
           (open-mail-thread)))))))

;;;###autoload
(defun search-mail-live ()
  "Search all mail as you type, and read the picked thread in the thread view.
C-SPC previews a thread's newest match beside the search.  Embark's
collect lists the threads to act on, and its export reads them whole."
  (interactive)
  (when-let* ((picked (consult--read
                       (consult--process-collection #'mail-live-command
                         :transform (consult--async-transform #'mail-live-candidates))
                       :prompt "Search mail: "
                       :lookup #'consult--lookup-member
                       :state (mail-live-preview-state)
                       :require-match t
                       :category 'mail-thread
                       :history '(:input mail-live-history)
                       :sort nil)))
    (open-mail-live-candidate picked)))

;;; Embark actions on the listed threads

;;;###autoload
(defun open-mail-live-candidate (candidate)
  "Read the thread of CANDIDATE, a line of the live search, in the thread view."
  (when-let* ((thread (get-text-property 0 'mail-thread candidate)))
    (open-mail-live-thread thread (get-text-property 0 'mail-query candidate))))

;;;###autoload
(defun open-mail-live-summary (candidates)
  "Read every message of the threads of CANDIDATES in one summary."
  (read-mail-live-threads (mail-live-candidate-threads candidates) #'ignore))

;;;###autoload
(defun open-mail-live-in-gmail (candidate)
  "Open in Gmail the newest message of CANDIDATE's thread its query matches."
  (when-let* ((thread (get-text-property 0 'mail-thread candidate))
              (id (mail-live-newest-id thread (get-text-property 0 'mail-query candidate))))
    (browse-url (gmail-message-url id))))

(defvar-local mail-live-export nil
  "Lines of the live search whose threads this buffer stands in for.")

;;;###autoload
(defun export-mail-live-search (candidates)
  "Leave current a buffer that stands in for the summary of CANDIDATES.
Embark renames the buffer an exporter leaves current, and nnselect finds
a summary by its name, so `open-mail-live-export-h' opens the summary."
  (set-buffer (generate-new-buffer "*mail export*"))
  (special-mode)
  (setq mail-live-export candidates)
  (let ((inhibit-read-only t))
    (insert (format "Reading %d threads in a summary..." (length candidates)))))

;;;###autoload
(defun open-mail-live-export-h ()
  "Replace the buffer an export of the live search left with its summary."
  (when-let* ((candidates mail-live-export))
    (let ((export (current-buffer)))
      (read-mail-live-threads (mail-live-candidate-threads candidates)
                              (lambda (_summary)
                                (when (buffer-live-p export)
                                  (kill-buffer export)))))))

;;; live.el ends here
