;;; scripts/mail-load.el --- read nnmaildir groups for a Gnus session -*- lexical-binding: t; -*-
;;; Commentary:
;; Usage: emacs -Q --batch --init-directory DIR -l scripts/mail-load.el
;;        --eval '(mail-load-main "TABLE-DIR" (quote ((GROUP STORE FORCE) ...)))'
;; nnmaildir reads a group by opening one overview file per message.  This
;; reads it from its saved table plus what changed, for the session to apply.
;;; Code:

(require 'cl-lib)
(require 'gnus)
;; the header parser reads a decoder variable gnus-sum defines
(require 'gnus-sum)
(require 'nnmaildir)

(defconst mail-table-version 1
  "Format of the tables `mail-load-group' saves.")

;;; Tables, which the session reads too

(defun mail-table-group-parts (group)
  "(SERVER . NAME) of GROUP, such as nnmaildir+gmail:inbox."
  (if (string-match "\\`nnmaildir\\+\\([^:]+\\):\\(.+\\)\\'" group)
      (cons (match-string 1 group) (match-string 2 group))
    (error "Not an nnmaildir group: %s" group)))

(defun mail-table-file (directory group)
  "File under DIRECTORY holding the table of GROUP."
  (pcase-let ((`(,server . ,name) (mail-table-group-parts group)))
    (expand-file-name (concat server "/" name) directory)))

(defun mail-table-new-group (name header)
  "Empty nnmaildir group NAME, sized and dated by the table HEADER."
  (let ((count (plist-get header :count)))
    (make-nnmaildir--grp :name name :index 0
                         :new (plist-get header :new) :cur (plist-get header :cur)
                         :flist (gnus-make-hashtable count)
                         :mlist (gnus-make-hashtable count)
                         :mmth (make-hash-table)
                         :cache (make-vector (plist-get header :cache) nil))))

(defun mail-table-add-row (group row)
  "Put ROW, a table's (NUMBER PREFIX SUFFIX MESSAGE-ID), into GROUP's lookups.
Returns the (NUMBER . ARTICLE) entry for GROUP's number list."
  (pcase-let* ((`(,num ,prefix ,suffix ,msgid) row)
               (article (make-nnmaildir--art :prefix prefix :suffix suffix
                                             :num num :msgid msgid)))
    (puthash prefix article (nnmaildir--grp-flist group))
    (puthash msgid article (nnmaildir--grp-mlist group))
    (cons num article)))

(defun mail-table-finish-group (group nlist)
  "Give GROUP the number list NLIST, highest first, with its count and minimum."
  (setf (nnmaildir--grp-nlist group) nlist
        (nnmaildir--grp-count group) (length nlist)
        (nnmaildir--grp-min group) (if nlist (car (car (last nlist))) 1)))

;;; Reading a group

(defun mail-load-report (&rest args)
  "Tell the session ARGS on stderr, which a batch Emacs does not buffer."
  (message "%S" (cons 'mail-load args)))

(defun mail-load-inode (dir)
  "Inode number of DIR, or nil when it does not exist."
  (file-attribute-inode-number (file-attributes dir)))

(defun mail-load-read-table (file numdir)
  "The table in FILE as (HEADER . ROWS), or nil when it is missing or stale.
A table numbered through another NUMDIR, as after a rebuild, is stale."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents-literally file)
      (let ((header (read (current-buffer)))
            rows)
        (when (and (eql (plist-get header :version) mail-table-version)
                   (equal (plist-get header :num-dir) (mail-load-inode numdir)))
          (while (progn (skip-chars-forward "\n") (not (eobp)))
            (push (read (current-buffer)) rows))
          (cons header (nreverse rows)))))))

(defun mail-load-table-group (name table)
  "The nnmaildir group NAME, built from TABLE, a (HEADER . ROWS)."
  (let ((group (mail-table-new-group name (car table)))
        nlist)
    (dolist (row (cdr table))
      (push (mail-table-add-row group row) nlist))
    (mail-table-finish-group group nlist)
    group))

(defun mail-load-open-server (server store)
  "Make nnmaildir SERVER over STORE current, opening it first."
  (unless (alist-get server nnmaildir--servers nil nil #'equal)
    (let ((defs `((directory ,store) (get-new-mail nil))))
      (unless (nnmaildir-open-server server defs)
        (error "Could not open nnmaildir over %s: %s" store
               (nnmaildir--srv-error nnmaildir--cur-server)))
      ;; with two groups read, nnmaildir looks the method up in Gnus's
      ;; server tables, which no Gnus session filled here
      (setf (nnmaildir--srv-method nnmaildir--cur-server) `(nnmaildir ,server ,@defs))))
  (nnmaildir--prepare server nil))

(defun mail-load-prune (group dir)
  "Drop GROUP's articles whose file left maildir DIR; give the rest their flags.
nnmaildir forgets neither on its own.  Non-nil when anything changed."
  (let ((names (make-hash-table :test #'equal :size (nnmaildir--grp-count group)))
        (flist (nnmaildir--grp-flist group))
        (mlist (nnmaildir--grp-mlist group))
        changed kept)
    (dolist (file (directory-files (nnmaildir--cur dir) nil "\\`[^.]" t))
      (when (string-match "\\`\\([^:]*\\)\\(\\(:.*\\)?\\)\\'" file)
        (puthash (match-string 1 file) (match-string 2 file) names)))
    (dolist (entry (nnmaildir--grp-nlist group))
      (let* ((article (cdr entry))
             (prefix (nnmaildir--art-prefix article))
             (suffix (gethash prefix names)))
        (if suffix
            (progn
              (unless (equal suffix (nnmaildir--art-suffix article))
                (setf (nnmaildir--art-suffix article) suffix
                      changed t))
              (push entry kept))
          (setq changed t)
          (remhash prefix flist)
          (when (eq (gethash (nnmaildir--art-msgid article) mlist) article)
            (remhash (nnmaildir--art-msgid article) mlist)))))
    (when changed
      (mail-table-finish-group group (nreverse kept)))
    changed))

(defun mail-load-marks (name server)
  "Read ranges, marks and mark times of group NAME on SERVER, from its files."
  (let ((info (list name gnus-level-default-subscribed nil))
        (group (nnmaildir--prepare server name))
        times)
    (setf (nnmaildir--grp-mmth group) (make-hash-table))
    (nnmaildir-request-update-info name info server)
    (maphash (lambda (mark time) (push (cons mark time) times))
             (nnmaildir--grp-mmth group))
    (list :read (gnus-info-read info) :marks (gnus-info-marks info) :mmth times)))

(defun mail-load-write-table (file header group)
  "Save HEADER and GROUP's articles, lowest number first, as the table FILE."
  (make-directory (file-name-directory file) t)
  (let ((part (concat file ".part")))
    (with-temp-buffer
      ;; ASCII only, so the session can read it literally
      (let ((print-length nil)
            (print-level nil)
            (print-escape-newlines t)
            (print-escape-multibyte t)
            (print-escape-nonascii t))
        (prin1 header (current-buffer))
        (insert "\n")
        (dolist (entry (reverse (nnmaildir--grp-nlist group)))
          (let ((article (cdr entry)))
            (prin1 (list (car entry) (nnmaildir--art-prefix article)
                         (nnmaildir--art-suffix article) (nnmaildir--art-msgid article))
                   (current-buffer))
            (insert "\n"))))
      (let ((coding-system-for-write 'no-conversion))
        (write-region nil nil part nil 'silent)))
    (rename-file part file t)))

(defun mail-load-group (directory group store force)
  "Read GROUP from STORE and save its table under DIRECTORY; return the file.
nil when neither the maildir nor the table changed, unless FORCE."
  (pcase-let* ((`(,server . ,name) (mail-table-group-parts group))
               (dir (file-name-as-directory (expand-file-name name store)))
               (numdir (nnmaildir--num-dir (nnmaildir--nndir dir)))
               (file (mail-table-file directory group))
               (table (ignore-errors (mail-load-read-table file numdir))))
    (mail-load-open-server server store)
    (let ((groups (nnmaildir--srv-groups nnmaildir--cur-server)))
      ;; a group read from its table scans only what changed since
      (if table
          (puthash name (mail-load-table-group name table) groups)
        (remhash name groups))
      (nnmaildir-request-scan name server)
      (let* ((loaded (or (gethash name groups)
                         (error "%s" (nnmaildir--srv-error nnmaildir--cur-server))))
             (pruned (mail-load-prune loaded dir)))
        ;; mail moved out of new/ changes cur/ as well
        (when (or force pruned (not table)
                  (not (time-equal-p (nnmaildir--grp-cur loaded) (plist-get (car table) :cur))))
          (mail-load-write-table
           file
           (append (list :version mail-table-version :num-dir (mail-load-inode numdir)
                         :new (nnmaildir--grp-new loaded) :cur (nnmaildir--grp-cur loaded)
                         :count (nnmaildir--grp-count loaded)
                         :cache (+ 16 (nnmaildir--grp-count loaded)))
                   (mail-load-marks name server))
           loaded)
          file)))))

(defun mail-load-main (directory specs)
  "Read each (GROUP STORE FORCE) of SPECS, saving its table under DIRECTORY.
Each result goes to stderr as it lands: (mail-load loaded GROUP FILE),
\(mail-load unchanged GROUP) or (mail-load failed GROUP MESSAGE)."
  ;; nnmaildir looks its group parameters up through the newsrc
  (setq gnus-newsrc-hashtb (gnus-make-hashtable)
        gnus-verbose 0)
  (let ((status 0))
    (pcase-dolist (`(,group ,store ,force) specs)
      (condition-case err
          (if-let* ((file (mail-load-group directory group store force)))
              (mail-load-report 'loaded group file)
            (mail-load-report 'unchanged group))
        (error
         (setq status 1)
         (mail-load-report 'failed group (error-message-string err)))))
    (kill-emacs status)))

(provide 'mail-load)
;;; mail-load.el ends here
