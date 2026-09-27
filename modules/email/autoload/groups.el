;;; modules/email/autoload/groups.el -*- lexical-binding: t; -*-

(require 'gnus)
(require 'gnus-group)
(require 'gnus-topic)
(require 'gnus-srvr)
(require 'nntp)

(defvar mail-topics)
(defvar mail-bulk-groups)
(defvar gnus-tmp-group)

;;; Lines

(defun mail-group-description (group)
  "Description `mail-topics' gives GROUP, or nil."
  (seq-some (lambda (topic) (cdr (assoc group (cdr topic)))) mail-topics))

;;;###autoload
(defun gnus-user-format-function-C (_header)
  "Description of the group whose line the group buffer draws."
  (or (mail-group-description gnus-tmp-group) ""))

;;; Topics

(defun mail-topic-groups ()
  "Every group `mail-topics' names."
  (mapcan (lambda (topic) (mapcar #'car (cdr topic))) mail-topics))

(defun mail-topics-regexp ()
  "Regexp matching exactly the groups `mail-topics' names."
  (concat "\\`" (regexp-opt (mail-topic-groups)) "\\'"))

(defun topic-subtree (name topology)
  "The subtree of topic NAME in TOPOLOGY, shaped like `gnus-topic-topology'."
  (if (equal (caar topology) name)
      topology
    (seq-some (lambda (child) (topic-subtree name child)) (cdr topology))))

(defun prune-topics (names topology)
  "TOPOLOGY without the subtopics NAMES, at any depth."
  (cons (car topology)
        (mapcar (lambda (child) (prune-topics names child))
                (seq-remove (lambda (child) (member (caar child) names))
                            (cdr topology)))))

(defun arrange-mail-topics (topology alist topics known)
  "Gnus's TOPOLOGY and ALIST rearranged by TOPICS, as (TOPOLOGY . ALIST).
TOPICS is shaped like `mail-topics', and KNOWN tells the groups Gnus knows.
Its topics come first under the root, each holding its groups in order
ahead of any other; every other topic loses only the groups TOPICS names."
  (let ((names (mapcar #'car topics))
        (named (mapcan (lambda (topic) (mapcar #'car (cdr topic))) topics)))
    (cons (cons (car topology)
                (append (mapcar (lambda (name)
                                  (prune-topics names (or (topic-subtree name topology)
                                                          (list (list name 'visible)))))
                                names)
                        (cdr (prune-topics names topology))))
          (append (mapcar (lambda (topic)
                            (cons (car topic)
                                  (append (seq-filter known (mapcar #'car (cdr topic)))
                                          (seq-difference (cdr (assoc (car topic) alist))
                                                          named))))
                          topics)
                  (mapcar (lambda (entry)
                            (cons (car entry) (seq-difference (cdr entry) named)))
                          (seq-remove (lambda (entry) (member (car entry) names))
                                      alist))))))

;;;###autoload
(defun apply-mail-topics ()
  "Put the groups `mail-topics' names into its topics, list them always, redraw."
  (interactive nil gnus-group-mode)
  (pcase-let ((`(,topology . ,alist)
               (arrange-mail-topics gnus-topic-topology gnus-topic-alist mail-topics
                                    (lambda (group) (gnus-group-entry group)))))
    (setq gnus-topic-topology topology
          gnus-topic-alist alist))
  (setq gnus-permanently-visible-groups (mail-topics-regexp))
  (when (gnus-buffer-live-p gnus-group-buffer)
    (with-current-buffer gnus-group-buffer
      (gnus-group-list-groups))))

(defun put-group-in-topic (group topic)
  "Move GROUP out of every topic and into TOPIC, last."
  (dolist (entry gnus-topic-alist)
    (setcdr entry (delete group (cdr entry))))
  (nconc (assoc topic gnus-topic-alist) (list group)))

;;; Finding groups

(defun words-wildmat (words)
  "NNTP wildmat for the names holding WORDS, in order."
  (concat "*" (string-join (split-string words) "*") "*"))

(defun words-regexp (words)
  "Regexp for the names holding WORDS, in order."
  (mapconcat #'regexp-quote (split-string words) ".*"))

(defun news-servers ()
  "The NNTP servers among `gnus-secondary-select-methods'."
  (seq-filter (lambda (method) (eq (car method) 'nntp)) gnus-secondary-select-methods))

(defun news-groups-matching (words method)
  "Groups on news server METHOD whose names hold WORDS, as (GROUP . ARTICLES)."
  (when (and (gnus-check-server method)
             (nntp-list-active-group (words-wildmat words) (cadr method)))
    (with-current-buffer nntp-server-buffer
      (goto-char (point-min))
      (let (groups)
        (while (re-search-forward "^\\([^ \n]+\\) +\\([0-9]+\\) +\\([0-9]+\\)" nil t)
          (push (cons (gnus-group-prefixed-name (match-string 1) method)
                      (- (1+ (string-to-number (match-string 2)))
                         (string-to-number (match-string 3))))
                groups))
        (nreverse groups)))))

(defun unsubscribed-p (group)
  "Non-nil when GROUP is not subscribed."
  (< gnus-level-subscribed (gnus-group-level group)))

(defun read-group-to-add ()
  "Read an unsubscribed label or news group whose name holds the words typed."
  (let* ((words (read-string "Add groups matching: "))
         (regexp (words-regexp words))
         (candidates
          (seq-filter
           (lambda (candidate) (unsubscribed-p (car candidate)))
           (append (mapcar #'list
                           (seq-filter (lambda (group)
                                         (string-match-p regexp (gnus-group-real-name group)))
                                       (maildir-groups)))
                   (mapcan (lambda (method) (news-groups-matching words method))
                           (news-servers)))))
         (completion-extra-properties
          (list :annotation-function
                (lambda (group)
                  (if-let* ((articles (cdr (assoc group candidates))))
                      (format "  %d articles" articles)
                    "  label")))))
    (unless candidates
      (user-error "No group to add matches \"%s\"" words))
    (completing-read "Add group: " candidates nil t)))

;;; Commands

;;;###autoload
(defun add-mail-group (group)
  "Subscribe to GROUP, a label or a news group, into the topic at point.
A news group or a bulk label sits above the routine scan."
  (interactive (list (read-group-to-add)) gnus-group-mode)
  (let* ((topic (gnus-current-topic))
         (label (eq (car (gnus-find-method-for-group group)) 'nnmaildir))
         (routine (and label (not (member group mail-bulk-groups)))))
    (gnus-group-change-level group
                             (if routine gnus-level-default-subscribed
                               (1+ gnus-activate-level))
                             (gnus-group-level group))
    (when topic
      (put-group-in-topic group topic))
    (when routine
      (refresh-mail-group group))
    (apply-mail-topics)
    ;; shows the line even when the group would not be listed
    (gnus-group-jump-to-group group)))

(defun read-news-server ()
  "The news server to browse, asked for only when there are several."
  (let ((servers (news-servers)))
    (cond ((cdr servers)
           (gnus-server-to-method
            (completing-read "Browse news server: "
                             (mapcar #'gnus-method-to-server servers) nil t)))
          (servers (car servers))
          (t (user-error "No news server")))))

;;;###autoload
(defun browse-news-groups (method words)
  "Browse the groups on news server METHOD whose names hold WORDS.
The whole list of news.gmane.io takes about 18 s.  In the browse buffer
`u' subscribes and `q' returns to the group buffer."
  (interactive (list (read-news-server) (read-string "Browse news groups matching: "))
               gnus-group-mode)
  (cl-letf (((symbol-function 'nntp-request-list)
             (lambda (&optional server)
               (when (nntp-list-active-group (words-wildmat words) server)
                 (with-current-buffer nntp-server-buffer
                   (nntp-decode-text))
                 t))))
    (gnus-browse-foreign-server method)))

;;;###autoload
(defun defer-news-group-h (group)
  "Put GROUP, subscribed just now, above `gnus-activate-level' if it is news.
Gnus would otherwise ask its server for it at every start."
  (when (eq (car (gnus-find-method-for-group group)) 'nntp)
    (gnus-group-change-level group (1+ gnus-activate-level) (gnus-group-level group))))

;;; groups.el ends here
