;;; tests/email/addresses-tests.el --- recipient completion specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defvar mail-from-address "agzam.ibragimov@gmail.com")
(defvar mail-inbox-address "to.plotnick@gmail.com")

(load-module-file "modules/email/autoload/addresses.el")

(defun addresses-tests-entry (name address count &optional name-addr)
  "An entry of notmuch's sexp output: NAME, ADDRESS and COUNT."
  (list :name name :address address
        :name-addr (or name-addr (if (string-empty-p name) address (format "%s <%s>" name address)))
        :count count))

(defun addresses-tests-rank (&rest runs)
  "The ranked addresses of RUNS, each (GROUP INCREMENT ENTRIES), in run order."
  (let ((rows (make-hash-table :test #'equal)))
    (seq-do-indexed (lambda (run index)
                      (apply #'add-mail-address-run rows index run))
                    runs)
    (ranked-mail-addresses rows)))

(defconst addresses-tests-runs
  `(("recipients" "all"
     ,(list (addresses-tests-entry "Sandy Guan" "sguan@splash.example" 19)
            (addresses-tests-entry "" "rhill@old.example" 139)
            (addresses-tests-entry "Eric" "eric@home.example" 12)
            (addresses-tests-entry "" "reply+AAB2CX3DQ4EZ5F@reply.github.com" 5)
            (addresses-tests-entry "Ag Ibragimov" "agzam.ibragimov@gmail.com" 3)))
    ("recipients" "3years"
     ,(list (addresses-tests-entry "Sandy Guan" "sguan@splash.example" 19)
            (addresses-tests-entry "Eric" "eric@home.example" 2)))
    ("recipients" "1year" ,(list (addresses-tests-entry "Sandy Guan" "sguan@splash.example" 10)))
    ("recipients" "30days" ,(list (addresses-tests-entry "Sandy Guan" "sguan@splash.example" 2)))
    ("sender" "all"
     ,(list (addresses-tests-entry "Panther Creek" "pchs@school.example" 443)
            (addresses-tests-entry "Eric" "eric@home.example" 30)
            (addresses-tests-entry "Chase" "no.reply.alerts@chase.example" 200)
            (addresses-tests-entry "karthink" "notifications@github.com" 263)
            (addresses-tests-entry "Teacher" "teacher@school.example" 10)))
    ("sender" "3years"
     ,(list (addresses-tests-entry "Panther Creek" "pchs@school.example" 300)
            (addresses-tests-entry "Teacher" "teacher@school.example" 10)))
    ("sender" "1year" ,(list (addresses-tests-entry "Panther Creek" "pchs@school.example" 100)))
    ("sender" "30days" ,(list (addresses-tests-entry "Panther Creek" "pchs@school.example" 5))))
  "What notmuch prints per output and date window, for the stand-in.")

(defconst addresses-tests-ranked
  '("Sandy Guan <sguan@splash.example>" "rhill@old.example" "Eric <eric@home.example>"
    "Panther Creek <pchs@school.example>" "Teacher <teacher@school.example>")
  "`addresses-tests-runs' ranked.")

(defun addresses-tests-notmuch (dir &optional hold exit)
  "Stand-in notmuch in DIR printing `addresses-tests-runs' for its arguments.
It logs each call's arguments to DIR/args, waits while HOLD names no
file, and with EXIT prints an error and exits with that status."
  (let ((script (expand-file-name "notmuch" dir)))
    (pcase-dolist (`(,output ,window ,entries) addresses-tests-runs)
      (with-temp-file (expand-file-name (format "%s-%s" output window) dir)
        (prin1 entries (current-buffer))))
    (with-temp-file script
      (insert "#!/bin/sh\n"
              "printf '%s\\n' \"$*\" >> " (shell-quote-argument (expand-file-name "args" dir)) "\n"
              (if hold
                  (format "while [ ! -f %s ]; do sleep 0.05; done\n" (shell-quote-argument hold))
                "")
              (if exit
                  (format "echo 'notmuch: database locked' >&2\nexit %d\n" exit)
                (concat "case \"$*\" in *--output=recipients*) output=recipients;; *) output=sender;; esac\n"
                        "case \"$*\" in *date:30days*) window=30days;; *date:1year*) window=1year;;"
                        " *date:3years*) window=3years;; *) window=all;; esac\n"
                        "cat " (shell-quote-argument (file-name-as-directory dir))
                        "\"$output-$window\"\n"))))
    (set-file-modes script #o755)
    script))

(defmacro addresses-tests-with-store (&rest body)
  "Run BODY with a fresh address state and `mail-address-file' in a temp dir.
DIR is bound to that dir."
  (declare (indent 0))
  `(let* ((dir (file-name-as-directory (make-temp-file "addresses" t)))
          (mail-address-file (expand-file-name "mail-addresses.eld" dir))
          (mail-addresses nil)
          (mail-addresses-read-at nil)
          (mail-address-processes nil))
     (unwind-protect (progn ,@body)
       (dolist (process mail-address-processes)
         (when (process-live-p process)
           (delete-process process)))
       (delete-directory dir t))))

(defun addresses-tests-wait ()
  "Wait until every run of the read in progress has been handled."
  (with-timeout (10)
    (while (seq-some (lambda (process) (buffer-live-p (process-buffer process)))
                     mail-address-processes)
      (accept-process-output nil 0.05))))

(describe "mail-address-runs"
  (it "asks for the recipients of the account's mail and the senders of mail to it"
    (let ((runs (mail-address-runs)))
      (expect (length runs) :to-equal 8)
      (expect (car runs)
              :to-equal
              '(wrote 0.1 ("address" "--format=sexp" "--output=recipients" "--output=count"
                           "--deduplicate=address"
                           "(from:agzam.ibragimov@gmail.com or from:to.plotnick@gmail.com)")))
      (expect (nth 1 runs)
              :to-equal
              '(wrote-you 0.1 ("address" "--format=sexp" "--output=sender" "--output=count"
                               "--deduplicate=address"
                               "(to:agzam.ibragimov@gmail.com or to:to.plotnick@gmail.com)")))))
  (it "counts a message once per date window it falls in, so it weighs as its age says"
    (let ((runs (mail-address-runs)))
      (expect (mapcar (lambda (run) (car (last (nth 2 run)))) (seq-filter (lambda (run) (eq (car run) 'wrote)) runs))
              :to-equal
              '("(from:agzam.ibragimov@gmail.com or from:to.plotnick@gmail.com)"
                "(from:agzam.ibragimov@gmail.com or from:to.plotnick@gmail.com) and date:3years.."
                "(from:agzam.ibragimov@gmail.com or from:to.plotnick@gmail.com) and date:1year.."
                "(from:agzam.ibragimov@gmail.com or from:to.plotnick@gmail.com) and date:30days.."))
      ;; a message of the last 30 days lands in all four runs
      (expect (apply #'+ (mapcar #'cadr (seq-filter (lambda (run) (eq (car run) 'wrote-you)) runs)))
              :to-be-close-to 8 6)
      (expect (cadr (nth 2 runs)) :to-be-close-to 1.9 6))))

(describe "ranked-mail-addresses"
  (it "puts whom the account wrote to first, then who only wrote to it"
    (expect (addresses-tests-rank
             `(wrote-you 1 ,(list (addresses-tests-entry "School" "school@example.com" 500)))
             `(wrote 1 ,(list (addresses-tests-entry "Friend" "friend@example.com" 1))))
            :to-equal '("Friend <friend@example.com>" "School <school@example.com>")))
  (it "weighs recent mail above old volume"
    (expect (seq-take (addresses-tests-rank
                       `(wrote 0.1 ,(list (addresses-tests-entry "" "rhill@old.example" 139)
                                          (addresses-tests-entry "Sandy Guan" "sguan@splash.example" 19)))
                       `(wrote 1.9 ,(list (addresses-tests-entry "Sandy Guan" "sguan@splash.example" 19))))
                      2)
            :to-equal '("Sandy Guan <sguan@splash.example>" "rhill@old.example")))
  (it "ranks the stand-in's whole output"
    (expect (apply #'addresses-tests-rank
                   (seq-mapn (lambda (run spec)
                               (list (car run) (cadr run) (nth 2 spec)))
                             (mail-address-runs)
                             (list (nth 0 addresses-tests-runs) (nth 4 addresses-tests-runs)
                                   (nth 1 addresses-tests-runs) (nth 5 addresses-tests-runs)
                                   (nth 2 addresses-tests-runs) (nth 6 addresses-tests-runs)
                                   (nth 3 addresses-tests-runs) (nth 7 addresses-tests-runs))))
            :to-equal addresses-tests-ranked))
  (it "drops the account's own addresses, automated senders and generated reply addresses"
    (expect (addresses-tests-rank
             `(wrote 1 ,(list (addresses-tests-entry "Ag" "agzam.ibragimov@gmail.com" 9)
                              (addresses-tests-entry "Me" "To.Plotnick@gmail.com" 9)
                              (addresses-tests-entry "" "reply+AAB2CX3DQ4EZ5F@reply.github.com" 9)
                              (addresses-tests-entry "" "job-1038829670@craigslist.org" 9)
                              (addresses-tests-entry "Ag" "buzz+z120jxwbqn3ryjtjq04cdp@gmail.com" 9)
                              (addresses-tests-entry "" "c1a2b3c4d5@mail.applytojob.com" 9)
                              (addresses-tests-entry "Kept" "kept@example.com" 1)))
             `(wrote-you 1 ,(list (addresses-tests-entry "Chase" "no.reply.alerts@chase.com" 9)
                                  (addresses-tests-entry "Shop" "noreply@shop.example" 9)
                                  (addresses-tests-entry "Bank" "do-not-reply@bank.example" 9)
                                  (addresses-tests-entry "karthink" "notifications@github.com" 9)
                                  (addresses-tests-entry "Workday" "carmax@myworkday.com" 9
                                                         "\"workday.donotreply carmax\" <carmax@myworkday.com>")
                                  (addresses-tests-entry "" "MAILER-DAEMON@example.com" 9)
                                  (addresses-tests-entry "" "bounces+42@list.example" 9)
                                  (addresses-tests-entry "Phone" "6468818131@messaging.example" 1)
                                  (addresses-tests-entry "Year" "brenda1961@gmail.com" 1))))
            :to-equal '("Kept <kept@example.com>" "Phone <6468818131@messaging.example>"
                        "Year <brenda1961@gmail.com>")))
  (it "adds an address's messages across both groups and keeps it with whom the account wrote to"
    (expect (addresses-tests-rank
             `(wrote 1 ,(list (addresses-tests-entry "Eric" "eric@home.example" 1)
                              (addresses-tests-entry "Ann" "ann@home.example" 1)))
             `(wrote-you 1 ,(list (addresses-tests-entry "Eric" "Eric@Home.example" 5)
                                  (addresses-tests-entry "Ann" "ann@home.example" 2))))
            :to-equal '("Eric <eric@home.example>" "Ann <ann@home.example>")))
  (it "names an address as the newest run does, never by an address or an empty name"
    (expect (addresses-tests-rank
             `(wrote 0.1 ,(list (addresses-tests-entry "Khilola" "to.hilola@gmail.com" 5)
                                (addresses-tests-entry "Rob" "rob@example.com" 5)
                                (addresses-tests-entry "" "anon@example.com" 5)))
             `(wrote-you 0.1 ,(list (addresses-tests-entry "Anon Person" "anon@example.com" 1)))
             `(wrote 1.9 ,(list (addresses-tests-entry "Hilola Ibragimova" "to.hilola@gmail.com" 1)
                                (addresses-tests-entry "" "rob@example.com" 1)
                                (addresses-tests-entry "x@y.com" "x@y.com" 1
                                                       "\"x@y.com\" <x@y.com>"))))
            :to-equal '("Hilola Ibragimova <to.hilola@gmail.com>" "Rob <rob@example.com>"
                        "x@y.com" "Anon Person <anon@example.com>")))
  (it "breaks ties by the other group's count, then by name"
    (expect (addresses-tests-rank
             `(wrote 1 ,(list (addresses-tests-entry "Bea" "bea@example.com" 1)
                              (addresses-tests-entry "Abe" "abe@example.com" 1)
                              (addresses-tests-entry "Cid" "cid@example.com" 1)))
             `(wrote-you 1 ,(list (addresses-tests-entry "Cid" "cid@example.com" 3))))
            :to-equal '("Cid <cid@example.com>" "Abe <abe@example.com>" "Bea <bea@example.com>"))))

(describe "mail-address-candidates"
  (before-each
    (setq mail-addresses '("Sandy Guan <sguan@splash.example>" "rhill@old.example"
                           "Eric <eric.natale@home.example>" "Eric Natale <eric@bunk1.example>"
                           "\"Douglas, Matthew\" <madougla@teksys.example>")))
  (after-each (setq mail-addresses nil))
  (it "gives every address as it is for an empty input"
    (expect (mapcar #'car (mail-address-candidates "")) :to-equal mail-addresses))
  (it "matches the start of a name, case ignored, and keeps the typed text"
    (expect (mail-address-candidates "eri")
            :to-equal '(("eric <eric.natale@home.example>" . "Eric <eric.natale@home.example>")
                        ("eric Natale <eric@bunk1.example>" . "Eric Natale <eric@bunk1.example>"))))
  (it "matches the start of any word of the name"
    (expect (mail-address-candidates "nat")
            :to-equal '(("natale <eric@bunk1.example>" . "Eric Natale <eric@bunk1.example>")))
    (expect (mapcar #'cdr (mail-address-candidates "mat"))
            :to-equal '("\"Douglas, Matthew\" <madougla@teksys.example>")))
  (it "matches the start of the address, without the closing bracket"
    (expect (mail-address-candidates "eric.n")
            :to-equal '(("eric.natale@home.example" . "Eric <eric.natale@home.example>")))
    (expect (mail-address-candidates "RHI")
            :to-equal '(("RHIll@old.example" . "rhill@old.example"))))
  (it "never matches inside an address"
    (expect (mail-address-candidates "splash") :to-be nil)
    (expect (mail-address-candidates "home") :to-be nil)
    (expect (mail-address-candidates "hill") :to-be nil)
    (expect (mapcar #'cdr (mail-address-candidates "natale"))
            :to-equal '("Eric Natale <eric@bunk1.example>"))))

(describe "mail-address-table"
  (before-each (setq mail-addresses '("Eric <eric@home.example>" "Erin Brock <erin@example.com>")))
  (after-each (setq mail-addresses nil))
  (it "keeps the ranked order for every front end"
    (let ((table (mail-address-table (make-hash-table :test #'equal))))
      (expect (completion-metadata-get (completion-metadata "" table nil) 'display-sort-function)
              :to-be 'identity)
      (expect (completion-metadata-get (completion-metadata "" table nil) 'cycle-sort-function)
              :to-be 'identity)
      (expect (all-completions "" table) :to-equal mail-addresses)
      (expect (all-completions "er" table)
              :to-equal '("eric <eric@home.example>" "erin Brock <erin@example.com>"))
      (expect (try-completion "er" table) :to-equal "eri")))
  (it "notes the address each candidate stands for"
    (let* ((originals (make-hash-table :test #'equal))
           (table (mail-address-table originals)))
      (all-completions "bro" table)
      (expect (gethash "brock <erin@example.com>" originals)
              :to-equal "Erin Brock <erin@example.com>"))))

(describe "complete-mail-address"
  (before-each (setq mail-addresses '("Eric Natale <eric@bunk1.example>" "Ann <ann@example.com>")))
  (after-each (setq mail-addresses nil))
  (it "completes the recipient typed after the colon or the last comma"
    (with-temp-buffer
      (insert "To: Ann <ann@example.com>, nat")
      (pcase-let ((`(,beg ,end ,_table . ,props) (complete-mail-address)))
        (expect (buffer-substring beg end) :to-equal "nat")
        (expect (plist-get props :exit-function) :to-be-truthy)))
    (with-temp-buffer
      (insert "Cc: ")
      (pcase-let ((`(,beg ,end . ,_) (complete-mail-address)))
        (expect (list beg end) :to-equal (list (point) (point))))))
  (it "takes the whole recipient around point, on a continuation line too"
    (with-temp-buffer
      (insert "To: Ann <ann@example.com>,\n  Eric Na")
      (save-excursion (insert "tale <eric@bunk1.example>"))
      (pcase-let ((`(,beg ,end . ,_) (complete-mail-address)))
        (expect (buffer-substring beg end) :to-equal "Eric Natale <eric@bunk1.example>"))))
  (it "puts the address in place of the candidate the inline preview inserted"
    (with-temp-buffer
      (insert "To: nat")
      (pcase-let* ((`(,_beg ,end ,table . ,props) (complete-mail-address))
                   (candidate (car (all-completions "nat" table))))
        ;; what completion-preview-insert does: the rest, then the exit function
        (goto-char end)
        (insert (substring candidate 3))
        (funcall (plist-get props :exit-function) candidate 'finished)
        (expect (buffer-string) :to-equal "To: Eric Natale <eric@bunk1.example>")
        (expect (point) :to-equal (point-max)))))
  (it "leaves an address the list inserted as it is"
    (with-temp-buffer
      (insert "To: Ann <ann@example.com>")
      (pcase-let ((`(,_beg ,_end ,table . ,props) (complete-mail-address)))
        (all-completions "" table)
        (funcall (plist-get props :exit-function) "Ann <ann@example.com>" 'finished)
        (expect (buffer-string) :to-equal "To: Ann <ann@example.com>")))))

(describe "refresh-mail-addresses"
  (it "reads notmuch in the background, ranks what it prints and saves it"
    (addresses-tests-with-store
      (let ((mail-address-program (addresses-tests-notmuch dir)))
        (refresh-mail-addresses)
        (expect (length mail-address-processes) :to-equal 8)
        (expect mail-addresses :to-be nil)
        (addresses-tests-wait)
        (expect mail-addresses :to-equal addresses-tests-ranked)
        (expect mail-addresses-read-at :to-be-truthy)
        (expect (with-temp-buffer
                  (insert-file-contents mail-address-file)
                  (read (current-buffer)))
                :to-equal addresses-tests-ranked))))
  (it "starts no second read while one runs"
    (addresses-tests-with-store
      (let* ((go (expand-file-name "go" dir))
             (mail-address-program (addresses-tests-notmuch dir go)))
        (refresh-mail-addresses)
        (let ((first mail-address-processes))
          (refresh-mail-addresses)
          (expect mail-address-processes :to-be first))
        (write-region "" nil go)
        (addresses-tests-wait)
        (expect (length (with-temp-buffer
                          (insert-file-contents (expand-file-name "args" dir))
                          (split-string (buffer-string) "\n" t)))
                :to-equal 8)
        (expect mail-addresses :to-equal addresses-tests-ranked))))
  (it "keeps the list it has and says why when notmuch fails"
    (addresses-tests-with-store
      (let ((mail-address-program (addresses-tests-notmuch dir nil 1))
            (mail-addresses '("Old <old@example.com>"))
            said)
        (cl-letf (((symbol-function 'message)
                   (lambda (format &rest args) (push (apply #'format format args) said))))
          (refresh-mail-addresses)
          (addresses-tests-wait))
        (expect mail-addresses :to-equal '("Old <old@example.com>"))
        (expect mail-addresses-read-at :to-be nil)
        (expect (file-exists-p mail-address-file) :to-be nil)
        (expect said :to-equal '("Addresses not read: notmuch: database locked")))))
  (it "says why when notmuch is missing, and leaves no run behind"
    (addresses-tests-with-store
      (let ((mail-address-program (expand-file-name "no-such-notmuch" dir))
            said)
        (cl-letf (((symbol-function 'message)
                   (lambda (format &rest args) (push (apply #'format format args) said))))
          (refresh-mail-addresses))
        (expect mail-address-processes :to-be nil)
        (expect (car said) :to-match "\\`Addresses not read: ")))))

(describe "prepare-mail-addresses-h"
  (it "loads the saved list when the session has none, and reads notmuch again"
    (addresses-tests-with-store
      (let (read)
        (let ((coding-system-for-write 'utf-8))
          (with-temp-file mail-address-file
            (prin1 '("Saved <saved@example.com>" "Łukasz <l@example.com>") (current-buffer))))
        (cl-letf (((symbol-function 'refresh-mail-addresses) (lambda () (setq read t))))
          (prepare-mail-addresses-h))
        (expect mail-addresses :to-equal '("Saved <saved@example.com>" "Łukasz <l@example.com>"))
        (expect read :to-be t))))
  (it "reads notmuch again only once the list is stale"
    (addresses-tests-with-store
      (let ((mail-addresses '("Kept <kept@example.com>"))
            (reads 0))
        (cl-letf (((symbol-function 'refresh-mail-addresses) (lambda () (cl-incf reads))))
          (setq mail-addresses-read-at (float-time))
          (prepare-mail-addresses-h)
          (expect reads :to-equal 0)
          (setq mail-addresses-read-at (- (float-time) mail-address-refresh-interval 1))
          (prepare-mail-addresses-h)
          (expect reads :to-equal 1))
        (expect mail-addresses :to-equal '("Kept <kept@example.com>")))))
  (it "shrugs off a damaged file"
    (addresses-tests-with-store
      (write-region "(\"Half <half@example.com>\"" nil mail-address-file)
      (cl-letf (((symbol-function 'refresh-mail-addresses) #'ignore))
        (prepare-mail-addresses-h))
      (expect mail-addresses :to-be nil))))

(describe "save-mail-addresses"
  (it "keeps names outside ASCII across a save and a load"
    (addresses-tests-with-store
      (let ((saved '("Lowe’s <lowes@example.com>" "Łukasz <l@example.com>")))
        (setq mail-addresses saved)
        (save-mail-addresses)
        (setq mail-addresses nil)
        (load-mail-addresses)
        (expect mail-addresses :to-equal saved)))))

;;; addresses-tests.el ends here
