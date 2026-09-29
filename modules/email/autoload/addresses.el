;;; modules/email/autoload/addresses.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Recipient completion from the notmuch index: whom the account wrote
;; to, then who wrote to it, each by frecency.  notmuch runs in the
;; background and the ranked list is kept in a file between sessions.
;;; Code:

(require 'seq)
(require 'subr-x)

(defvar doom-cache-dir)
(defvar mail-from-address)
(defvar mail-inbox-address)

(defvar mail-address-program "notmuch"
  "Program the recipient addresses are read from.")

(defvar mail-address-file (expand-file-name "mail-addresses.eld" doom-cache-dir)
  "File the ranked addresses are kept in between sessions.")

(defvar mail-address-refresh-interval 3600
  "Seconds a read of the addresses stays fresh for a new message.")

(defvar mail-address-ages '((nil . 0.1) ("3years" . 2) ("1year" . 4) ("30days" . 8))
  "Weight of a message by age, as (DATE . WEIGHT) from oldest to newest.
A message weighs as the last entry whose notmuch DATE it is newer than;
nil stands for every message.")

(defvar mail-address-ignored
  (rx (or (seq "no" (? (any "-._")) "reply")
          (seq "do" (? (any "-._")) "not" (? (any "-._")) "reply")
          (seq "notification" (? "s") "@")
          "mailer-daemon"
          (seq "bounce" (? "s") (any "@+-"))
          "@reply."
          "craigslist.org"
          ;; a generated token: letters and digits alternate four times
          (seq (or (seq (+ alpha) (+ digit) (+ alpha) (+ digit) alpha)
                   (seq (+ digit) (+ alpha) (+ digit) (+ alpha) digit))
               (* (not (any "@<> ")))
               "@")))
  "Addresses never offered: automated senders and one-off reply addresses.")

(defvar mail-addresses nil
  "Recipient addresses to complete, best first.")

(defvar mail-addresses-read-at nil
  "When notmuch last gave `mail-addresses', as a float time, or nil.")

(defvar mail-address-processes nil
  "The notmuch runs of the read in progress.")

;;; The ranking

(defun mail-address-own ()
  "The account's own addresses, never offered."
  (list mail-from-address mail-inbox-address))

(defun mail-address-runs ()
  "The notmuch runs a read makes, as (GROUP INCREMENT ARGS).
GROUP is `wrote' for the recipients of the account's mail and
`wrote-you' for the senders of mail to it; each message a run counts
adds INCREMENT, so a message weighs as `mail-address-ages' says."
  (let ((from (mapconcat (lambda (address) (concat "from:" address)) (mail-address-own) " or "))
        (to (mapconcat (lambda (address) (concat "to:" address)) (mail-address-own) " or "))
        (previous 0)
        runs)
    (pcase-dolist (`(,date . ,weight) mail-address-ages)
      (let ((window (if date (format " and date:%s.." date) "")))
        (dolist (run `((wrote "recipients" ,from) (wrote-you "sender" ,to)))
          (push (list (car run) (- weight previous)
                      (list "address" "--format=sexp" (concat "--output=" (nth 1 run))
                            "--output=count" "--deduplicate=address"
                            (format "(%s)%s" (nth 2 run) window)))
                runs))
        (setq previous weight)))
    (nreverse runs)))

(defun mail-address-named (name-addr)
  "NAME-ADDR when it carries a name besides the address."
  (and name-addr (string-search "<" name-addr) name-addr))

(defun add-mail-address-run (rows index group increment entries)
  "Add ENTRIES, notmuch's output of run INDEX for GROUP, to the hash table ROWS.
A row is [NAME-ADDR SCORE RUN] for `wrote' and the same again for
`wrote-you', or `ignored'.  Each entry adds INCREMENT per message, the
name of the latest run wins, and a name that is an address counts as none."
  (let ((own (mapcar #'downcase (mail-address-own)))
        (base (if (eq group 'wrote) 0 3)))
    (dolist (entry entries)
      (let* ((address (plist-get entry :address))
             (key (downcase address))
             (name-addr (if (string-search "@" (plist-get entry :name))
                            address
                          (plist-get entry :name-addr)))
             (row (gethash key rows)))
        (cond ((eq row 'ignored))
              ((and (null row)
                    (or (member key own)
                        (string-match-p mail-address-ignored (downcase name-addr))))
               (puthash key 'ignored rows))
              (t
               (unless row
                 (setq row (puthash key (vector nil 0 -1 nil 0 -1) rows)))
               (when (or (null (aref row base))
                         (and (mail-address-named name-addr)
                              (or (not (mail-address-named (aref row base)))
                                  (< (aref row (+ base 2)) index))))
                 (aset row base name-addr)
                 (aset row (+ base 2) index))
               (aset row (1+ base) (+ (aref row (1+ base))
                                      (* increment (plist-get entry :count))))))))))

(defun ranked-mail-addresses (rows)
  "The addresses of ROWS, best first.
Whom the account wrote to come first, then the rest, each by score."
  (let (keyed)
    (maphash (lambda (_ row)
               (unless (eq row 'ignored)
                 (let ((wrote (aref row 1))
                       (wrote-you (aref row 4))
                       (name-addr (or (mail-address-named (aref row 0))
                                      (mail-address-named (aref row 3))
                                      (aref row 0) (aref row 3))))
                   (push (cons (if (< 0 wrote)
                                   (list 0 (- wrote) (- wrote-you) name-addr)
                                 (list 1 (- wrote-you) 0 name-addr))
                               name-addr)
                         keyed))))
             rows)
    (mapcar #'cdr (sort keyed :key #'car))))

;;; The read

(defun mail-address-entries (buffer)
  "The entries notmuch printed into BUFFER, or `failed' when they do not read."
  (with-current-buffer buffer
    (goto-char (point-min))
    (if (not (re-search-forward "[^ \t\n]" nil t))
        nil
      (goto-char (point-min))
      (condition-case nil
          (let ((entries (read (current-buffer))))
            (if (listp entries) entries 'failed))
        (error 'failed)))))

(defun save-mail-addresses ()
  "Write `mail-addresses' into `mail-address-file'."
  (make-directory (file-name-directory mail-address-file) t)
  (let ((coding-system-for-write 'utf-8)
        (print-length nil)
        (print-level nil))
    (with-temp-file mail-address-file
      (prin1 mail-addresses (current-buffer)))))

(defun load-mail-addresses ()
  "Read `mail-addresses' from `mail-address-file' when the session has none."
  (when (and (null mail-addresses) (file-readable-p mail-address-file))
    (let ((saved (with-temp-buffer
                   (let ((coding-system-for-read 'utf-8))
                     (insert-file-contents mail-address-file))
                   (ignore-errors (read (current-buffer))))))
      (setq mail-addresses (and (proper-list-p saved) (seq-filter #'stringp saved))))))

(defun mail-address-output (process)
  "The entries PROCESS printed, or the reason it failed as a string."
  (let ((entries (if (and (eq (process-status process) 'exit)
                          (zerop (process-exit-status process)))
                     (mail-address-entries (process-buffer process))
                   'failed)))
    (if (listp entries)
        entries
      (with-current-buffer (process-buffer process)
        (or (car (split-string (buffer-string) "\n" t "[ \t]+"))
            (format "%s exited %s" mail-address-program (process-exit-status process)))))))

(defun finish-mail-address-read (rows failure)
  "Rank ROWS into `mail-addresses' and save them, or report FAILURE."
  (if failure
      (message "Addresses not read: %s" failure)
    (setq mail-addresses (ranked-mail-addresses rows)
          mail-addresses-read-at (float-time))
    (save-mail-addresses)))

;;;###autoload
(defun refresh-mail-addresses ()
  "Read the recipient addresses from notmuch in the background.
Nothing starts while a read runs, and the old list stays until a read
succeeds.  Each run's sentinel adds its own output, so none holds Emacs long."
  (unless (seq-some #'process-live-p mail-address-processes)
    (let* ((runs (mail-address-runs))
           (rows (make-hash-table :test #'equal))
           (left (length runs))
           failure)
      (setq mail-address-processes nil)
      (condition-case err
          (seq-do-indexed
           (lambda (run index)
             (push (make-process
                    :name "mail-addresses"
                    :buffer (generate-new-buffer " *mail-addresses*" t)
                    :command (cons mail-address-program (nth 2 run))
                    :coding 'utf-8
                    :connection-type 'pipe
                    :noquery t
                    :sentinel
                    (lambda (process _event)
                      (unless (process-live-p process)
                        (let ((output (mail-address-output process)))
                          (cond ((stringp output) (setq failure (or failure output)))
                                ((not failure)
                                 (add-mail-address-run rows index (car run) (cadr run) output))))
                        (kill-buffer (process-buffer process))
                        (when (zerop (setq left (1- left)))
                          (finish-mail-address-read rows failure)))))
                   mail-address-processes))
           runs)
        (error
         (dolist (process mail-address-processes)
           (set-process-sentinel process #'ignore)
           (delete-process process)
           (kill-buffer (process-buffer process)))
         (setq mail-address-processes nil)
         (message "Addresses not read: %s" (error-message-string err)))))))

;;;###autoload
(defun prepare-mail-addresses-h ()
  "Load the saved addresses, and read them again once they are stale."
  (load-mail-addresses)
  (when (or (null mail-addresses-read-at)
            (< mail-address-refresh-interval (- (float-time) mail-addresses-read-at)))
    (refresh-mail-addresses)))

;;; The completion

(defun mail-address-candidates (typed)
  "Completions of TYPED as (CANDIDATE . ADDRESS), best first.
Empty TYPED gives every address.  Otherwise TYPED has to start a word of
the name or the address itself, case ignored, and CANDIDATE is TYPED
followed by the rest of the address from there."
  (if (string-empty-p typed)
      (mapcar (lambda (address) (cons address address)) mail-addresses)
    (let ((regexp (concat "\\(?:\\`\\|[^[:alnum:]]\\)\\(" (regexp-quote typed) "\\)"))
          (case-fold-search t)
          candidates)
      (dolist (address mail-addresses)
        (when (string-match regexp address)
          (let* ((start (match-beginning 1))
                 (end (match-end 1))
                 (bracket (string-search "<" address))
                 (mailbox (if bracket (1+ bracket) 0)))
            (cond ((and bracket (< start bracket))
                   (push (cons (concat typed (substring address end)) address) candidates))
                  ((= start mailbox)
                   (push (cons (concat typed (string-remove-suffix
                                              ">" (substring address end)))
                               address)
                         candidates))))))
      (nreverse candidates))))

(defun mail-address-table (originals)
  "Completion table over `mail-addresses', in their order.
Each candidate it offers is noted in the hash table ORIGINALS with the
address it stands for."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (let ((candidates (mail-address-candidates string)))
        (unless (string-empty-p string)
          (pcase-dolist (`(,candidate . ,address) candidates)
            (puthash candidate address originals)))
        (complete-with-action action (mapcar #'car candidates) string predicate)))))

(defun mail-address-exit (originals)
  "Exit function that puts the address from ORIGINALS in place of its candidate."
  (lambda (candidate _status)
    (when-let* ((address (gethash candidate originals))
                ((not (equal address candidate)))
                ((looking-back (regexp-quote candidate) (- (point) (length candidate)))))
      (replace-match address t t))))

;;;###autoload
(defun complete-mail-address ()
  "Complete the recipient at point from `mail-addresses', best first."
  (let ((beg (save-excursion
               (skip-chars-backward "^\n:,")
               (skip-chars-forward " \t")
               (point)))
        (end (save-excursion
               (skip-chars-forward "^\n,")
               (skip-chars-backward " \t")
               (point)))
        (originals (make-hash-table :test #'equal)))
    (list (min beg (point)) (max end (point))
          (mail-address-table originals)
          :exit-function (mail-address-exit originals))))

;;; addresses.el ends here
