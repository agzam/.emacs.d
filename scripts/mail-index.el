;;; scripts/mail-index.el --- build nnmaildir overviews outside a session -*- lexical-binding: t; -*-
;; Usage: emacs -Q --batch --init-directory DIR -l scripts/mail-index.el
;;        --eval '(mail-index-main "STORE" REBUILD "GROUP" ...)'
;;
;; nnmaildir keeps one overview file per message under GROUP/.nnmaildir/nov
;; and builds the missing ones during a scan, in the main thread, at tens
;; of milliseconds each - so a label that arrives with thousands of
;; messages stalls the first Gnus session that scans it.  This builds them
;; here, with nnmaildir alone: no Gnus startup, no newsrc, and nothing
;; written outside GROUP/.nnmaildir/ except the maildir's own new/ to cur/
;; move that every scan makes.  A rebuild discards nov/ and num/ first,
;; which renumbers the group; nnmaildir re-derives read state from the
;; maildir flags, and marks/ is keyed by file name, so both survive.
;; Numbers follow each file's mtime, the day Gmail received the message,
;; so a group's highest numbers - the ones a `display' limit shows - are
;; its newest mail even when a backfill downloaded old mail last.

(require 'cl-lib)
(require 'gnus)
;; the header parser reads a decoder variable gnus-sum defines
(require 'gnus-sum)
(require 'nnmaildir)
(require 'seq)

(defconst mail-index-server "gmail"
  "Name of the nnmaildir server the build opens.")

(defun mail-index-groups (store)
  "Group names under STORE, one per maildir directory."
  (seq-filter (lambda (name) (file-directory-p (expand-file-name name store)))
              (directory-files store nil "\\`[^.]")))

(defun mail-index-count (dir)
  "Number of entries in DIR, or 0 when it does not exist."
  (if (file-directory-p dir)
      (length (directory-files dir nil "\\`[^.]" 'nosort))
    0))

(defun mail-index-messages (store group)
  "Number of messages GROUP under STORE holds in cur/ and new/."
  (let ((dir (expand-file-name group store)))
    (+ (mail-index-count (expand-file-name "cur" dir))
       (mail-index-count (expand-file-name "new" dir)))))

(defun mail-index-overviews (store group)
  "Number of overview files GROUP under STORE holds."
  (mail-index-count (expand-file-name (concat group "/.nnmaildir/nov") store)))

(defun mail-index-discard (store group)
  "Drop GROUP's overviews and article numbers under STORE, keeping its marks."
  (dolist (sub '("nov" "num"))
    (let ((dir (expand-file-name (concat group "/.nnmaildir/" sub) store)))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(defun mail-index-arrival-order (dir)
  "Predicate ordering the message files of DIR by mtime, oldest first.
mbsync's CopyArrivalDate sets the mtime to Gmail's arrival date, while
nnmaildir's own order is the download time in the file name, which
breaks the ties."
  (let ((mtimes (make-hash-table :test #'equal))
        (by-name (symbol-function 'nnmaildir--sort-files)))
    (cl-flet ((mtime (file)
                ;; nnmaildir--parse-filename wraps a name it can read
                ;; in a vector and leaves (PREFIX . SUFFIX) otherwise
                (let ((file (if (vectorp file) (aref file 3) file)))
                  (with-memoization (gethash (car file) mtimes)
                    (float-time
                     (or (file-attribute-modification-time
                          (file-attributes
                           (expand-file-name (concat (car file) (cdr file)) dir)))
                         0))))))
      (lambda (a b)
        (let ((ta (mtime a))
              (tb (mtime b)))
          (if (= ta tb)
              (funcall by-name a b)
            (< ta tb)))))))

(defun mail-index-build (store groups rebuild)
  "Scan GROUPS of the nnmaildir store at STORE, building missing overviews.
With REBUILD, discard each group's overviews first.  Returns one
\(GROUP MESSAGES BEFORE AFTER SECONDS) row per group."
  ;; nnmaildir looks its group parameters up through the newsrc
  (let ((gnus-newsrc-hashtb (gnus-make-hashtable))
        (gnus-verbose 0)
        (defs `((directory ,store) (get-new-mail nil))))
    (unless (nnmaildir-open-server mail-index-server defs)
      (error "nnmaildir did not open %s: %s" store
             (nnmaildir--srv-error nnmaildir--cur-server)))
    ;; with a second group registered, every later call resolves the
    ;; server's method through Gnus's server tables, which no Gnus
    ;; session has filled here
    (setf (nnmaildir--srv-method nnmaildir--cur-server)
          `(nnmaildir ,mail-index-server ,@defs))
    (unwind-protect
        (mapcar (lambda (group)
                  (when rebuild
                    (mail-index-discard store group))
                  (let ((messages (mail-index-messages store group))
                        (before (mail-index-overviews store group))
                        (start (float-time)))
                    (message "Scanning %s: %d messages, %d overviews..."
                             group messages before)
                    ;; nnmaildir numbers the files it has no number for
                    ;; in the order this sorts them
                    (cl-letf (((symbol-function 'nnmaildir--sort-files)
                               (mail-index-arrival-order
                                (expand-file-name (concat group "/cur") store))))
                      (nnmaildir-request-scan group mail-index-server))
                    (list group messages before (mail-index-overviews store group)
                          (- (float-time) start))))
                groups)
      ;; an open server keeps its directory, and nnmaildir-close-server
      ;; does nothing while no group is selected; the next build must
      ;; not inherit this one's store
      (setf (alist-get mail-index-server nnmaildir--servers nil 'remove #'equal) nil)
      (setq nnmaildir--cur-server nil))))

(defun mail-index-main (store rebuild &rest groups)
  "Build overviews for GROUPS under STORE, every group when none is named.
REBUILD discards the existing overviews first."
  (dolist (row (mail-index-build store (or groups (mail-index-groups store)) rebuild))
    (pcase-let ((`(,group ,messages ,before ,after ,seconds) row))
      (message "%s: %d messages, overviews %d -> %d, %.1f s"
               group messages before after seconds)))
  (kill-emacs 0))
