;;; modules/email/autoload/groups.el -*- lexical-binding: t; -*-

(require 'gnus)
(require 'gnus-group)
(require 'gnus-topic)
(require 'nnatom)

(defvar doom-cache-dir)
(defvar mail-topics)
(defvar mail-bulk-groups)
(defvar mail-remote-backends)
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

(defun unsubscribed-p (group)
  "Non-nil when GROUP is not subscribed."
  (< gnus-level-subscribed (gnus-group-level group)))

(defun groups-to-add ()
  "Unsubscribed labels and news groups as (GROUP . ARTICLES), a label's nil."
  (seq-filter (lambda (candidate) (unsubscribed-p (car candidate)))
              (append (mapcar #'list (maildir-groups)) (news-active-groups))))

(defun read-group-to-add ()
  "Read an unsubscribed label or news group.
The news groups are the server's list as the last news fetch saved it."
  (let* ((candidates (groups-to-add))
         (articles (make-hash-table :test #'equal :size (length candidates)))
         (completion-extra-properties
          (list :annotation-function
                (lambda (group)
                  (if-let* ((count (gethash group articles)))
                      (format "  %d articles" count)
                    "  label")))))
    (unless candidates
      (user-error "No group to add"))
    (pcase-dolist (`(,group . ,count) candidates)
      (when count
        (puthash group count articles)))
    (completing-read (if (news-active-groups)
                         "Add group: "
                       "Add label (news groups arrive with the next news fetch): ")
                     candidates nil t)))

;;; Commands

;;;###autoload
(defun add-mail-group (group)
  "Subscribe to GROUP, a label or a news group, into the topic at point.
A news group or a bulk label sits above the routine scan, and a news
group gets its folder, which a news fetch starts filling right away."
  (interactive (list (read-group-to-add)) gnus-group-mode)
  (let* ((topic (gnus-current-topic))
         (news (news-group-p group))
         (routine (and (not news)
                       (eq (car (gnus-find-method-for-group group)) 'nnmaildir)
                       (not (member group mail-bulk-groups)))))
    (when news
      (make-news-folder group))
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
    (gnus-group-jump-to-group group)
    (when news
      (fetch-news))))

;;; Feeds

(defvar feed-user-agent "emacs:gnus-nnatom:32.0 (personal feed reader)"
  "User-Agent `fetch-feeds' sends; Reddit blocks generic ones.")

(defvar feed-fetch-program "curl"
  "Program `fetch-feeds' downloads with; Reddit answers url.el with 403.")

(defvar feed-directory (expand-file-name "feeds/" doom-cache-dir)
  "Directory `fetch-feeds' saves each feed into, for `read-atom-feed'.")

(defvar feed-fetch-interval 1800
  "Seconds between feed downloads while Gnus runs.")

(defvar feed-fetch-timer nil
  "Timer of the periodic feed download, or nil.")

(defun feed-file (feed)
  "File the download of FEED, an nnatom server address, is saved as."
  (expand-file-name (concat (gnus-newsgroup-savable-name feed) ".atom") feed-directory))

(defun feed-methods ()
  "The nnatom methods among `gnus-secondary-select-methods'."
  (seq-filter (lambda (method) (eq (car method) 'nnatom)) gnus-secondary-select-methods))

(defun feed-groups (method)
  "Groups Gnus knows on feed METHOD."
  (let ((server (gnus-method-to-server method)))
    (seq-filter (lambda (group) (equal (gnus-group-server group) server)) gnus-group-list)))

;;;###autoload
(defun read-atom-feed (feed group)
  "Read GROUP of Atom FEED the way nnatom does, from its downloaded copy.
Fetching it here would hold Emacs until the server answers."
  (let ((file (feed-file feed)))
    (when (file-exists-p file)
      (nnatom--read-feed file group))))

(defun fetch-feed-sentinel (method part)
  "Sentinel that saves PART as METHOD's feed and reads its groups again."
  (lambda (proc event)
    (when (memq (process-status proc) '(exit signal))
      (if (and (eq (process-status proc) 'exit)
               (zerop (process-exit-status proc)))
          (progn
            (rename-file part (feed-file (cadr method)) t)
            (when (gnus-alive-p)
              (dolist (group (feed-groups method))
                (refresh-mail-group group)
                (gnus-group-update-group group t))))
        (when (file-exists-p part)
          (delete-file part))
        (message "Feed %s not fetched: %s" (cadr method) (string-trim event))))))

;;;###autoload
(defun fetch-feeds (&optional methods)
  "Download METHODS' feeds in the background, every nnatom method by default.
Each download replaces the saved copy, and its groups are read again."
  (make-directory feed-directory t)
  (dolist (method (or methods (feed-methods)))
    (let ((part (make-temp-file (expand-file-name "part-" feed-directory))))
      (make-process :name (concat "feed " (cadr method))
                    :command (list feed-fetch-program "--silent" "--fail" "--location"
                                   "--max-time" "20" "--user-agent" feed-user-agent
                                   "--output" part (concat "https://" (cadr method)))
                    :noquery t
                    :sentinel (fetch-feed-sentinel method part)))))

(defun feed-stale-p (method)
  "Non-nil when METHOD's saved feed is missing or older than the interval."
  (let ((file (feed-file (cadr method))))
    (or (not (file-exists-p file))
        (< feed-fetch-interval
           (float-time (time-since (file-attribute-modification-time
                                    (file-attributes file))))))))

(defun fetch-feeds-while-gnus-runs ()
  "Download the feeds, or stop downloading them once Gnus is gone."
  (if (gnus-alive-p)
      (fetch-feeds)
    (cancel-timer feed-fetch-timer)
    (setq feed-fetch-timer nil)))

;;;###autoload
(defun start-feed-fetches ()
  "Download the stale feeds now, then every feed each `feed-fetch-interval'."
  (when-let* ((stale (seq-filter #'feed-stale-p (feed-methods))))
    (fetch-feeds stale))
  (unless (timerp feed-fetch-timer)
    (setq feed-fetch-timer (run-with-timer feed-fetch-interval feed-fetch-interval
                                           #'fetch-feeds-while-gnus-runs))))

;;;###autoload
(defun defer-news-group-h (group)
  "Put GROUP, subscribed just now, above `gnus-activate-level' if it is remote.
Every start would read a year of a news group or fetch a web feed."
  (when (or (news-group-p group)
            (memq (car (gnus-find-method-for-group group)) mail-remote-backends))
    (gnus-group-change-level group (1+ gnus-activate-level) (gnus-group-level group))))

;;; groups.el ends here
