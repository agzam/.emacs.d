;;; modules/email/autoload/labels.el -*- lexical-binding: t; -*-
;;; Commentary:
;; A label is a Gmail folder mbsync mirrors as an nnmaildir group: adding
;; one copies the message into its group, removing one deletes that copy.
;;; Code:

(require 'cl-lib)
(require 'gnus)
(require 'gnus-group)
(require 'gnus-search)
(require 'gnus-sum)
(require 'nnmaildir)
(require 'seq)
(require 'subr-x)
(require 'transient)

(defvar doom-state-dir)
(defvar gmail-maildir)
(defvar mail-inbox-group)
(defvar mail-trash-group)
(defvar mail-archive-group)
(defvar gnus-message-archive-group)

(defvar mail-label-keys nil
  "Keys pinned to labels, as (NAME . KEY); other labels get theirs by themselves.")

(defvar mail-label-keys-file (expand-file-name "mail-label-keys.eld" doom-state-dir)
  "File keeping the key each label got, so a new label moves none.")

(defvar mail-label-rows 3
  "Rows of labels the menu keeps to while its columns fit the window.")

(defvar mail-label-slice 0.03
  "Seconds of label changes per timer turn.")

;;; Labels and their keys

(defun mail-label-name (group)
  "Name GROUP shows under in the label menu."
  (if (equal group mail-inbox-group)
      "Inbox"
    (gnus-group-real-name group)))

(defun mail-label-groups ()
  "Groups the label menu offers: the inbox, then every Gmail label by name."
  (cons mail-inbox-group
        (sort (seq-difference (maildir-groups)
                              (list mail-inbox-group mail-trash-group mail-archive-group
                                    gnus-message-archive-group))
              (lambda (a b)
                (string-lessp (downcase (mail-label-name a))
                              (downcase (mail-label-name b)))))))

(defun mail-label-key-candidates (name)
  "Keys for the label NAME, best first.
The first letters of its words, the same shifted, its other letters,
then any letter or digit.  q quits every menu."
  (let* ((words (split-string (downcase name) "[^[:alnum:]]+" t))
         (initials (mapcar #'string-to-char words))
         (rest (cdr (string-to-list (apply #'concat words))))
         (vowel-p (lambda (char) (memq char '(?a ?e ?i ?o ?u))))
         (inner (append (seq-remove vowel-p rest) (seq-filter vowel-p rest))))
    (mapcar #'string
            (delq ?q (seq-uniq (append initials (mapcar #'upcase initials)
                                       inner (mapcar #'upcase inner)
                                       (number-sequence ?a ?z) (number-sequence ?A ?Z)
                                       (number-sequence ?0 ?9)))))))

(defun mail-label-assign-keys (names kept)
  "A key for each of NAMES, as (NAME . KEY), with the keys KEPT gives kept.
Every free first choice goes out before any name takes a later one."
  (let* ((keys (seq-filter (lambda (pair) (member (car pair) names)) kept))
         (used (mapcar #'cdr keys)))
    (dolist (first-only '(t nil))
      (dolist (name names)
        (unless (assoc name keys)
          (let ((candidates (mail-label-key-candidates name)))
            (when-let* ((key (seq-find (lambda (key) (not (member key used)))
                                       (if first-only (list (car candidates)) candidates))))
              (push (cons name key) keys)
              (push key used))))))
    keys))

(defun mail-label-remembered-keys ()
  "Keys labels got before, as (NAME . KEY)."
  (when (file-exists-p mail-label-keys-file)
    (with-temp-buffer
      (insert-file-contents mail-label-keys-file)
      (read (current-buffer)))))

(defun mail-label-keys-for (names)
  "Keys for the labels NAMES: pinned, else given before, else new and kept."
  (let* ((pins (seq-filter (lambda (pin) (member (car pin) names)) mail-label-keys))
         (before (mail-label-remembered-keys))
         (kept (seq-remove (lambda (pair)
                             (or (assoc (car pair) pins) (rassoc (cdr pair) pins)))
                           before))
         (keys (mail-label-assign-keys names (append pins kept)))
         (remembered (seq-remove (lambda (pair) (assoc (car pair) pins)) keys)))
    (unless (seq-set-equal-p remembered before)
      (make-directory (file-name-directory mail-label-keys-file) t)
      (with-temp-file mail-label-keys-file
        (prin1 remembered (current-buffer))))
    keys))

(defun mail-label-rows-for (widths width)
  "Rows that put labels of WIDTHS in columns no wider than WIDTH.
`mail-label-rows' unless that overflows; transient sets columns two apart."
  (let ((rows (min mail-label-rows (max 1 (length widths)))))
    (while (and (< rows (length widths))
                (< width (+ (apply #'+ (mapcar (lambda (column) (apply #'max column))
                                               (seq-partition widths rows)))
                            (* 2 (1- (ceiling (length widths) rows))))))
      (setq rows (1+ rows)))
    rows))

;;; What the menu acts on

(cl-defstruct (mail-label-scope (:constructor mail-label-scope-make) (:copier nil))
  "The messages the label menu acts on, and the labels it was set to."
  summary articles marked groups carriers want)

(defun mail-label-copies (message-ids)
  "Files notmuch knows for each of MESSAGE-IDS, as (MESSAGE-ID . FILES)."
  (when message-ids
    (with-temp-buffer
      (when (eql 0 (call-process gnus-search-notmuch-program nil '(t nil) nil
                                 (format "--config=%s" gnus-search-notmuch-config-file)
                                 "show" "--format=sexp" "--format-version=5" "--body=false"
                                 "--entire-thread=false" "--exclude=false"
                                 (mapconcat (lambda (id)
                                              (format "id:\"%s\"" (string-trim id "<" ">")))
                                            message-ids " or ")))
        (goto-char (point-min))
        (let (found)
          (named-let walk ((form (ignore-error end-of-file (read (current-buffer)))))
            (cond ((and (consp form) (keywordp (car form)) (plist-get form :id))
                   (push (cons (format "<%s>" (plist-get form :id))
                               (ensure-list (plist-get form :filename)))
                         found))
                  ((consp form)
                   (walk (car form))
                   (walk (cdr form)))))
          found)))))

(defun mail-label-carriers (groups articles)
  "For each of GROUPS, which of this summary's ARTICLES it holds a copy of.
nnmaildir knows the copies made this session, notmuch those mbsync
renamed since nnmaildir read the group."
  (let* ((ids (mapcar (lambda (article)
                        (cons article (mail-header-id (gnus-summary-article-header article))))
                      articles))
         (copies (mail-label-copies (seq-uniq (mapcar #'cdr ids))))
         (listings (make-hash-table :test #'equal)))
    (mapcar
     (lambda (group)
       (let ((known (and (mail-group-loaded-p group)
                         (mapcar #'car (group-articles-by-id group (seq-uniq (mapcar #'cdr ids)))))))
         (cons group
               (seq-keep (pcase-lambda (`(,article . ,id))
                           (when (or (equal (mail-article-group article) group)
                                     (member id known)
                                     (seq-some (lambda (file)
                                                 (when-let* ((now (current-mail-file file listings)))
                                                   (equal (mail-file-group now) group)))
                                               (cdr (assoc id copies))))
                             article))
                         ids))))
     groups)))

(defun mail-label-held (scope group)
  "How many of SCOPE's messages carry GROUP: `all', `some' or `none'."
  (let ((have (length (alist-get group (mail-label-scope-carriers scope) nil nil #'equal))))
    (cond ((zerop have) 'none)
          ((= have (length (mail-label-scope-articles scope))) 'all)
          (t 'some))))

(defun mail-label-cycle (scope group)
  "Set GROUP in SCOPE to the next state: on, off, and back to some."
  (let ((want (alist-get group (mail-label-scope-want scope) nil nil #'equal)))
    (setf (alist-get group (mail-label-scope-want scope) nil nil #'equal)
          (pcase want
            ('some 'all)
            ('all 'none)
            (_ (if (eq (mail-label-held scope group) 'some) 'some 'all))))))

(defun mail-label-select (whole-threads)
  "The menu's scope: the region, else the marked messages, else the one on screen.
WHOLE-THREADS takes their threads."
  (if (derived-mode-p 'gnus-summary-mode)
      (let* ((marked (unless (use-region-p) (mail-process-marked)))
             (covered (or marked (mail-articles-at-point-or-region))))
        (mail-label-scope-make :summary (current-buffer)
                               :marked (and marked t)
                               :articles (if whole-threads
                                             (mail-whole-threads covered)
                                           (mail-real-articles covered))))
    (pcase-let ((`(,summary . ,article) (mail-on-screen)))
      (unless (buffer-live-p (get-buffer summary))
        (user-error "The summary of this message is gone"))
      (with-current-buffer summary
        (mail-label-scope-make :summary (current-buffer)
                               :articles (if whole-threads
                                             (save-excursion
                                               (gnus-summary-goto-subject article)
                                               (thread-articles-at-point))
                                           (list article)))))))

(defun mail-label-open (scope)
  "Show the label menu for SCOPE, each label's state read afresh."
  (with-current-buffer (mail-label-scope-summary scope)
    (let* ((groups (mail-label-groups))
           (carriers (mail-label-carriers groups (mail-label-scope-articles scope))))
      (setf (mail-label-scope-groups scope) groups
            (mail-label-scope-carriers scope) carriers
            (mail-label-scope-want scope) nil)
      (dolist (group groups)
        (push (cons group (mail-label-held scope group)) (mail-label-scope-want scope)))))
  (transient-setup 'mail-label-menu nil nil :scope scope))

;;;###autoload
(defun label-mail (&optional whole-threads)
  "Add and remove labels on the selected messages, or on WHOLE-THREADS of them.
The region, else the marked messages, else the one on screen."
  (interactive nil gnus-summary-mode mail-thread-mode gnus-article-mode)
  ;; a label mbsync made since the start is a group Gnus lacks; a group
  ;; still loading answers through notmuch, and applying waits for it
  (subscribe-mail-groups)
  (mail-label-open (mail-label-select whole-threads)))

;;;###autoload
(defun label-mail-threads ()
  "Add and remove labels on every message of the selected threads."
  (interactive nil gnus-summary-mode mail-thread-mode gnus-article-mode)
  (label-mail t))

;;; The menu

(defclass mail-label-suffix (transient-suffix)
  ((group :initarg :group))
  "A label in the label menu.")

(defun mail-label-heading (&optional _)
  "What the label menu acts on."
  (let* ((scope (transient-scope 'mail-label-menu))
         (count (length (mail-label-scope-articles scope))))
    (format "Labels: %s"
            (cond ((mail-label-scope-marked scope)
                   (format "%d marked message%s" count (if (= count 1) "" "s")))
                  ((= count 1) "the message on screen")
                  (t (format "%d messages" count))))))

(defun mail-label-description (obj)
  "The mark and name of OBJ's label: a check on every message, a half circle on some."
  (let ((group (oref obj group)))
    (concat (pcase (alist-get group (mail-label-scope-want (transient-scope 'mail-label-menu))
                              nil nil #'equal)
              ('all (propertize "✓" 'face 'success))
              ('some (propertize "◐" 'face 'shadow))
              (_ " "))
            " " (mail-label-name group))))

(defun mail-label-children (_)
  "The labels in columns as low as the window allows, then the actions."
  (let* ((scope (transient-scope 'mail-label-menu))
         (groups (mail-label-scope-groups scope))
         (keys (mail-label-keys-for (mapcar #'mail-label-name groups)))
         (actions '(("RET" "apply" mail-label-apply) ("+" "new label" mail-label-create)))
         ;; " k ✓ name", as transient formats a suffix
         (widths (mapcar (lambda (group) (+ 5 (string-width (mail-label-name group)))) groups))
         (rows (mail-label-rows-for widths (- (window-body-width) 2 (length " + new label")))))
    (transient-parse-suffixes
     'mail-label-menu
     (append (mapcar #'vconcat
                     (seq-partition
                      (mapcar (lambda (group)
                                (list (cdr (assoc (mail-label-name group) keys)) 'mail-label-toggle
                                      :class 'mail-label-suffix :group group
                                      :description #'mail-label-description :transient t))
                              groups)
                      rows))
             (list (vconcat actions))))))

(transient-define-prefix mail-label-menu ()
  "Add and remove labels on the messages the menu was opened for."
  [:class transient-columns
   :description mail-label-heading
   :setup-children mail-label-children]
  ;; the Return key arrives as <return>, which evil binds in the summary
  [:hide always
   [("<return>" "apply" mail-label-apply)]])

(defun mail-label-toggle ()
  "Turn the label of the key pressed on, off, or back to how it was."
  (interactive)
  (mail-label-cycle (transient-scope 'mail-label-menu)
                    (oref (transient-suffix-object) group)))

(defun mail-label-create ()
  "Make a new label and turn it on for the menu's messages."
  (interactive)
  (let* ((scope (transient-scope 'mail-label-menu))
         (name (string-trim (read-string "New label: ")))
         (group (concat "nnmaildir+gmail:" name)))
    (unless (string-match-p "\\`[[:alnum:]_-]+\\'" name)
      (user-error "A label name takes letters, digits, - and _"))
    (when (seq-some (lambda (old) (string-equal-ignore-case (mail-label-name old) name))
                    (mail-label-groups))
      (user-error "Label %s exists" name))
    (dolist (sub '("cur" "new" "tmp"))
      (make-directory (expand-file-name sub (expand-file-name name gmail-maildir)) t))
    (subscribe-mail-group group)
    (load-mail-groups (list group) t)
    (setf (mail-label-scope-groups scope) (mail-label-groups)
          (alist-get group (mail-label-scope-carriers scope) nil nil #'equal) nil
          (alist-get group (mail-label-scope-want scope) nil nil #'equal) 'all)
    (transient-setup 'mail-label-menu nil nil :scope scope)))

;;; Applying

(cl-defstruct (mail-label-job (:constructor mail-label-job-make) (:copier nil))
  "Label changes on one summary's messages, and what got done."
  summary items done)

(defvar mail-label-jobs nil
  "Label changes still to run, the first job running.")

(defvar mail-label-timer nil
  "Timer of the next turn of label changes.")

(defun mail-label-items (scope)
  "The changes SCOPE asks for as (VERB GROUP ARTICLE), the additions first.
A removal can delete the file the summary shows, which a copy reads."
  (let (adds removes)
    (pcase-dolist (`(,group . ,want) (mail-label-scope-want scope))
      (let ((have (alist-get group (mail-label-scope-carriers scope) nil nil #'equal)))
        (pcase want
          ('all (dolist (article (seq-difference (mail-label-scope-articles scope) have))
                  (push (list 'add group article) adds)))
          ('none (dolist (article have)
                   (push (list 'remove group article) removes))))))
    (append (nreverse adds) (nreverse removes))))

(defun mail-label-copy-number (group message-id)
  "Number of GROUP's copy of the message with MESSAGE-ID, or nil."
  (or (cdr (assoc message-id (group-articles-by-id group (list message-id))))
      ;; a copy mbsync renamed since nnmaildir read the group
      (when-let* ((listings (make-hash-table :test #'equal))
                  (file (seq-some (lambda (file)
                                    (when-let* ((now (current-mail-file file listings)))
                                      (and (equal (mail-file-group now) group) now)))
                                  (cdr (assoc message-id (mail-label-copies (list message-id)))))))
        (nnmaildir-base-name-to-article-number
         (car (split-string (file-name-nondirectory file) ":"))
         (gnus-group-real-name group)
         (nth 1 (gnus-find-method-for-group group))))))

(defun mail-label-run (verb group article)
  "Add the label GROUP to this summary's ARTICLE, or remove it, per VERB.
Return `shown' when the summary's own copy went, nil when nothing changed."
  (let ((gnus-newsgroup-processable (list article))
        (gnus-newsgroup-process-stack nil)
        (mark-active nil)
        (gnus-verbose 4))
    (cond ((eq verb 'add)
           ;; Gnus would rescan GROUP after every copy
           (cl-letf (((symbol-function 'gnus-group-get-new-news-this-group) #'ignore))
             (gnus-summary-move-article nil group nil 'copy))
           t)
          ((equal (mail-article-group article) group)
           (gnus-summary-delete-article)
           'shown)
          ((when-let* ((copy (mail-label-copy-number
                              group (mail-header-id (gnus-summary-article-header article)))))
             (gnus-request-expire-articles (list copy) group t)
             (drop-gone-articles group (list copy))
             t)))))

(defun mail-label-report (done)
  "DONE, (VERB GROUP ARTICLE) items newest first, in words, oldest first."
  (let (counts)
    (dolist (item (reverse done))
      (let ((key (cons (car item) (cadr item))))
        (setf (alist-get key counts nil nil #'equal)
              (1+ (alist-get key counts 0 nil #'equal)))))
    (mapconcat (pcase-lambda (`((,verb . ,group) . ,count))
                 (format "%s %s %s %d" (if (eq verb 'add) "added" "removed")
                         (mail-label-name group) (if (eq verb 'add) "to" "from") count))
               (nreverse counts) ", ")))

(defun mail-label-finish (job)
  "Say what JOB changed, and take the lines whose copy went out of view."
  (let ((summary (mail-label-job-summary job))
        (done (mail-label-job-done job)))
    (when (and (buffer-live-p summary)
               (seq-some (lambda (item) (eq (nth 3 item) 'shown)) done))
      (with-current-buffer summary
        (gnus-summary-limit-to-marks (list gnus-canceled-mark) 'reverse)))
    (dolist (group (seq-uniq (mapcar #'cadr done)))
      ;; Gnus's range of the group ends below a copy until it reads the
      ;; group again, which takes nnmaildir's count, not a scan
      (gnus-activate-group group)
      (when (gnus-buffer-live-p gnus-group-buffer)
        (gnus-group-update-group group t)))
    (message "Labels: %s%s"
             (if done (mail-label-report done) "nothing changed")
             (if (mail-label-job-items job) ", the rest dropped with its summary" ""))))

(defun mail-label-step ()
  "Run label changes for `mail-label-slice' seconds, then yield to Emacs."
  (setq mail-label-timer nil)
  (let ((deadline (+ (float-time) mail-label-slice)))
    (while (and mail-label-jobs (< (float-time) deadline))
      (let ((job (car mail-label-jobs)))
        (if (and (mail-label-job-items job)
                 (buffer-live-p (mail-label-job-summary job)))
            (let* ((item (pop (mail-label-job-items job)))
                   (result (with-current-buffer (mail-label-job-summary job)
                             (apply #'mail-label-run item))))
              (when result
                (push (append item (list result)) (mail-label-job-done job))))
          (pop mail-label-jobs)
          (mail-label-finish job)))))
  (when mail-label-jobs
    (setq mail-label-timer (run-at-time 0 nil #'mail-label-step))))

(defun mail-label-start (job)
  "Queue JOB and run label changes unless a turn is due already."
  (setq mail-label-jobs (append mail-label-jobs (list job)))
  (unless mail-label-timer
    (mail-label-step)))

(defun mail-label-apply ()
  "Apply the labels the menu was set to; the marks are used up."
  (interactive)
  (let* ((scope (transient-scope 'mail-label-menu))
         (summary (mail-label-scope-summary scope))
         (items (mail-label-items scope))
         (job (mail-label-job-make :summary summary :items items))
         (unloaded (seq-remove #'mail-group-loaded-p (seq-uniq (mapcar #'cadr items)))))
    (with-current-buffer summary
      (save-excursion
        (apply #'gnus-summary-remove-process-mark
               (seq-intersection gnus-newsgroup-processable
                                 (mail-label-scope-articles scope)))))
    (cond ((null items) (message "Labels: nothing changed"))
          ;; nnmaildir takes no copy into a group it does not hold yet
          (unloaded
           (load-mail-groups unloaded t)
           (wait-for-mail-load 'labels "for the labels"
                               (lambda () (seq-every-p #'mail-group-loaded-p unloaded))
                               (lambda () (mail-label-start job))))
          (t (mail-label-start job)))))

;;; labels.el ends here
