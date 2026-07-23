;;; scripts/elpaca-update-report.el --- per-package update changelog -*- lexical-binding: t; -*-
;; Shared by `bb update's two drivers - the live emacsclient path
;; (elpaca-live-update.el) and the headless --batch path (elpaca-update.el) -
;; to turn a bulk Elpaca update into an audit trail: for every package that
;; moved, the commits that landed plus a forge "compare" link.
;;
;; Approach: snapshot each source repo's HEAD BEFORE the update, then once a
;; package has merged, diff OLD..NEW here with `git log'.  We compute the range
;; ourselves rather than scrape Elpaca's update-log buffer because the bounds
;; are exact (our snapshot vs the post-merge HEAD) and the format and links are
;; ours to shape.  Snapshotting up front is race-free: `git fetch' only moves
;; remote-tracking refs, so local HEAD sits still until the ff-only merge.

(require 'cl-lib)
(require 'subr-x)
;; The drivers load Elpaca for real; soft here so the pure helpers below stay
;; loadable (and unit-testable) without it.
(require 'elpaca nil t)

(defvar elpaca-update-report-color (and (getenv "ELPACA_UPDATE_COLOR") t)
  "When non-nil, wrap changelog shas and dates in ANSI SGR color codes.
`bb update' exports ELPACA_UPDATE_COLOR to the headless driver only when its
own stdout is a terminal, so the default is right there; the live driver - a
long-running server whose environment predates the run - overrides this per
run from its start argument, so a piped or redirected update stays plain.")

(defun elpaca-update-report--paint (code s)
  "Wrap S in ANSI SGR CODE (e.g. \"33\") when `elpaca-update-report-color' is on.
Uses a literal ESC via `string' rather than a \\e escape so the source stays
free of raw control characters."
  (if (and elpaca-update-report-color s (not (string-empty-p s)))
      (concat (string 27) "[" code "m" s (string 27) "[0m")
    s))

(defun elpaca-update-report-log-limit ()
  "Max commits listed per package; 0 means no limit.
Read from ELPACA_UPDATE_LOG_LIMIT, defaulting to 20."
  (max 0 (string-to-number (or (getenv "ELPACA_UPDATE_LOG_LIMIT") "20"))))

(defun elpaca-update-report--git (dir &rest args)
  "Run \"git ARGS\" in DIR; return trimmed stdout, or nil on failure/empty."
  (when (and dir (file-directory-p dir))
    (with-temp-buffer
      (let ((default-directory (file-name-as-directory dir)))
        (and (eq 0 (apply #'call-process "git" nil t nil args))
             (let ((s (string-trim (buffer-string))))
               (unless (string-empty-p s) s)))))))

(defun elpaca-update-report--head (dir)
  "Read DIR's checked-out HEAD sha straight from .git, spawning no process.
Snapshotting the whole package set with a `git' per repo costs a couple of
seconds and freezes the frame at kickoff; a file read is ~15x cheaper.
Returns nil - so the package is skipped - for anything that is not a plain
clone rooted at DIR: notably an in-tree local package whose .git lives higher
up, which has no upstream of its own to report."
  (when-let* ((git  (expand-file-name ".git/" dir))
              ((file-directory-p git))
              (head (expand-file-name "HEAD" git))
              ((file-readable-p head))
              (s (string-trim (with-temp-buffer (insert-file-contents head)
                                                (buffer-string)))))
    (if (not (string-prefix-p "ref: " s))
        s ; detached HEAD holds the sha directly
      (let* ((ref   (substring s 5))
             (loose (expand-file-name ref git)))
        (if (file-readable-p loose) ; loose ref wins over packed, as in git
            (string-trim (with-temp-buffer (insert-file-contents loose)
                                           (buffer-string)))
          (let ((packed (expand-file-name "packed-refs" git)))
            (when (file-readable-p packed)
              (with-temp-buffer
                (insert-file-contents packed)
                (goto-char (point-min))
                (when (re-search-forward
                       (concat "^\\([0-9a-f]+\\) " (regexp-quote ref) "$") nil t)
                  (match-string 1))))))))))

(defun elpaca-update-report-snapshot ()
  "Hash mapping every queued package's source dir to its current HEAD sha.
Take this before updating: it is the baseline each package is diffed against
once its update has merged."
  (let ((snapshot (make-hash-table :test 'equal)))
    (dolist (cell (elpaca--queued) snapshot)
      (when-let* ((dir (ignore-errors (elpaca<-source-dir (cdr cell))))
                  ((not (gethash dir snapshot)))
                  (sha (elpaca-update-report--head dir)))
        (puthash dir sha snapshot)))))

(defun elpaca-update-report--compare-url (url old new)
  "Forge compare link for repo URL between OLD and NEW, else URL, else nil.
GitLab nests compares under /-/; GitHub and the Gitea/Forgejo/Codeberg family
share the /compare form.  Anything else just gets the plain repo URL - a wrong
compare path is worse than none, and the shas are printed regardless."
  (when url
    (cond
     ((string-match-p "gitlab\\." url) (format "%s/-/compare/%s...%s" url old new))
     ((string-match-p "github\\.com\\|codeberg\\.org\\|gitea\\|forgejo" url)
      (format "%s/compare/%s...%s" url old new))
     (t url))))

(defun elpaca-update-report--url (e)
  "E's forge web URL, or nil.  Guarded: non-git recipes have no `elpaca--url'."
  (condition-case nil (elpaca--url e) (error nil)))

(defun elpaca-update-report--render (dir url old new &optional indent)
  "Changelog block for repo DIR moving OLD..NEW, or nil when unchanged.
URL is the repo's forge web address (for the compare link) or nil.  INDENT is
prefixed to every line (default four spaces).  A pinned rollback - NEW behind
OLD - is reported as such; the common case is a fast-forward."
  (when (and old new (not (equal old new)))
    (let* ((indent (or indent "    "))
           (limit  (elpaca-update-report-log-limit))
           (ahead  (string-to-number
                    (or (elpaca-update-report--git dir "rev-list" "--count"
                                                   (concat old ".." new))
                        "0")))
           (rollback (zerop ahead))
           (range  (if rollback (concat new ".." old) (concat old ".." new)))
           (count  (if rollback
                       (string-to-number
                        (or (elpaca-update-report--git dir "rev-list" "--count" range)
                            "0"))
                     ahead))
           (shown  (if (> limit 0) (min limit count) count))
           ;; Let git paint the commit lines: %C(...) placeholders emit SGR
           ;; codes under --color=always even though call-process pipes stdout.
           (pretty (if elpaca-update-report-color
                       "--pretty=format:%C(yellow)%h%C(reset) %s %C(dim)(%cr)%C(reset)"
                     "--pretty=format:%h %s (%cr)"))
           (log    (apply #'elpaca-update-report--git
                          dir "-c" "log.showSignature=false" "--no-pager" "log"
                          (format "--max-count=%d" shown)
                          (append (and elpaca-update-report-color '("--color=always"))
                                  (list pretty range))))
           (compare (elpaca-update-report--compare-url url old new))
           (phrase (format "%s%d commit%s"
                           (if rollback "rolled back " "")
                           count (if (= count 1) "" "s")))
           (head (concat indent
                         (elpaca-update-report--paint
                          "33" (concat (substring old 0 (min 8 (length old))) ".."
                                       (substring new 0 (min 8 (length new)))))
                         "  " phrase
                         (and compare (concat "  " compare))))
           (body (and log (not (string-empty-p log))
                      (mapconcat (lambda (l) (concat indent "  " l))
                                 (split-string log "\n" t) "\n")))
           (more (and (> count shown)
                      (concat indent "  … " (number-to-string (- count shown))
                              " more"))))
      (string-join (delq nil (list head body more)) "\n"))))

(defun elpaca-update-report-block (e snapshot &optional indent)
  "Changelog block for package E given the pre-update SNAPSHOT, or nil.
Nil when E's repo is absent from SNAPSHOT or its HEAD did not move."
  (when-let* ((dir (ignore-errors (elpaca<-source-dir e)))
              (old (gethash dir snapshot)))
    (elpaca-update-report--render
     dir (elpaca-update-report--url e) old
     (elpaca-update-report--head dir) indent)))

(defun elpaca-update-report-block-once (e snapshot seen &optional indent)
  "Changelog block for E, at most once per source dir across calls.
SEEN is a hash table threading already-reported dirs between calls - both
drivers dedup this way, because packages built from one repo (a monorepo, or
a package plus its extensions) would otherwise repeat the same block.  Nil
when SNAPSHOT is not a hash table (the update never reached its baseline),
E's dir is unknown or already in SEEN, or its HEAD did not move."
  (when-let* (((hash-table-p snapshot))
              (dir (ignore-errors (elpaca<-source-dir e)))
              ((not (gethash dir seen)))
              (changes (elpaca-update-report-block e snapshot indent)))
    (puthash dir t seen)
    changes))

;;; Progress streaming: batch unchanged-package names into `pulled:' lines

;; Both drivers report the (usually many) unchanged packages the same way - a
;; running `pulled: a, b, c' line rather than a whole line each - so the
;; batching lives here.  A batcher is a (PENDING-NAMES . WIDTH) cons the caller
;; threads through `-note'/`-flush'.  EMIT is any (EMIT FMT &rest ARGS) sink
;; that writes one line: stdout for the headless driver, the tailed logfile for
;; the live one.

(defun elpaca-update-report-batcher (&optional width)
  "Return a fresh `pulled:' batcher, flushing once a line passes WIDTH columns."
  (cons nil (or width 72)))

(defun elpaca-update-report-flush (batcher emit)
  "Emit BATCHER's pending names as one `pulled:' line via EMIT, then clear them.
EMIT is called as (EMIT FMT &rest ARGS) and writes exactly one line."
  (when (car batcher)
    (funcall emit "pulled: %s" (string-join (nreverse (car batcher)) ", "))
    (setcar batcher nil)))

(defun elpaca-update-report-note (batcher emit name)
  "Add NAME to BATCHER, first flushing via EMIT if the line would overflow.
The 8 covers the width of the `pulled: ' prefix the flush prepends."
  (when (and (car batcher)
             (< (cdr batcher)
                (+ 8 (length (string-join (cons name (car batcher)) ", ")))))
    (elpaca-update-report-flush batcher emit))
  (push name (car batcher)))

;;; Live streaming to stderr: keep the headless run followable

;; The headless driver prints progress as the update runs, but a --batch Emacs
;; block-buffers stdout whenever it is not a terminal (a pipe, a redirect, an
;; editor's compilation buffer), so anything `princ'd there stays invisible
;; until the process exits - the "goes silent with no server" complaint.
;; stderr is unbuffered, so routing progress here streams it live regardless of
;; how `bb update' was invoked.  The live driver has no such problem: it writes
;; to a logfile the caller tails and flushes.

(defun elpaca-update-report-progress (line)
  "Write LINE plus a newline to stderr, which `--batch' leaves unbuffered."
  (princ (concat line "\n") #'external-debugging-output))

;;; Pending-work summary: what the update is still waiting on

;; Both drivers wait for the async queue to settle.  When nothing has settled
;; for a while - a slow compile, or a genuinely wedged package - the driver
;; must still say what it is waiting on rather than sit mute.  These turn the
;; live queue into a short "id=status, …" line; the runtime collector needs a
;; live Elpaca, the formatter is pure so the shape stays unit-testable.

(defun elpaca-update-report-pending ()
  "Return (ID . STATUS) for every queued package not yet finished or failed."
  (when (fboundp 'elpaca--queued)
    (let (out)
      (dolist (cell (elpaca--queued))
        (let ((status (elpaca<-status (cdr cell))))
          (unless (memq status '(finished failed))
            (push (cons (car cell) status) out))))
      (nreverse out))))

(defun elpaca-update-report-format-pending (pending &optional limit)
  "Render PENDING - a list of (ID . STATUS) - as one compact `id=status' line.
At most LIMIT entries are shown (default 6); any remainder is counted."
  (let* ((limit (or limit 6))
         (n (length pending))
         (shown (if (> n limit) (cl-subseq pending 0 limit) pending)))
    (concat (mapconcat (lambda (c) (format "%s=%s" (car c) (cdr c))) shown ", ")
            (and (> n limit) (format ", … +%d more" (- n limit))))))

(defun elpaca-update-report-heartbeat-line (pending last-emit interval &optional now)
  "Return a `still working' line when the run has been silent for INTERVAL secs.
PENDING is the (ID . STATUS) list from `elpaca-update-report-pending', LAST-EMIT
the `float-time' of the most recent output, NOW defaults to the current time.
Returns nil when nothing is pending or the silence is still under INTERVAL - so
a phase that streams per-package lines (each refreshing LAST-EMIT) stays quiet,
while the first-run build, which settles steadily but emits nothing, does not."
  (let ((now (or now (float-time))))
    (when (and pending (>= (- now last-emit) interval))
      (format "still working (%ds, %d pending): %s"
              (round (- now last-emit)) (length pending)
              (elpaca-update-report-format-pending pending)))))

;;; Persistent append-log: every session teed to one growing, trimmed file

;; Beyond the per-run streaming, keep a durable record: every session appended
;; to one file so past updates can be diffed and blamed after the fact.  It is
;; deliberately belt-and-suspenders - every write is guarded, because a broken
;; log (unwritable /tmp, full disk) must never be what derails an update.  The
;; file grows across sessions and is trimmed a whole session at a time so it
;; never straddles a boundary, staying greppable plain text (ANSI stripped).

(defconst elpaca-update-report--log-marker "===== update session "
  "Line prefix that marks a session boundary in the persistent log.")

(defun elpaca-update-report-log-file ()
  "Persistent-log path from ELPACA_UPDATE_LOG_FILE (default /tmp/elpaca-update.log).
An explicitly empty value disables persistence (returns nil)."
  (let ((f (getenv "ELPACA_UPDATE_LOG_FILE")))
    (cond ((null f) "/tmp/elpaca-update.log")
          ((string-empty-p f) nil)
          (t f))))

(defun elpaca-update-report-log-max-bytes ()
  "Byte cap for the persistent log from ELPACA_UPDATE_LOG_MAX_BYTES.
Defaults to 2 MiB; 0 disables trimming so the log grows without bound."
  (max 0 (string-to-number
          (or (getenv "ELPACA_UPDATE_LOG_MAX_BYTES")
              (number-to-string (* 2 1024 1024))))))

(defun elpaca-update-report--trim-log (file max)
  "Trim FILE to under MAX bytes by dropping whole oldest sessions.
No-op when MAX is 0, the file is missing, or it already fits.  A lone session
bigger than MAX is left whole - a session is the atomic unit - and a note
records that older sessions were dropped."
  (when (and file (> max 0) (file-exists-p file)
             (> (file-attribute-size (file-attributes file)) max))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8)) (insert-file-contents file))
      (let ((drop (- (position-bytes (point-max)) max))
            (marker (concat "^" (regexp-quote elpaca-update-report--log-marker)))
            cut)
        (goto-char (point-min))
        ;; Past the first session's own header, find the earliest later header
        ;; that leaves the remainder within MAX, and cut the file there.
        (when (re-search-forward marker nil t)
          (while (and (not cut) (re-search-forward marker nil t))
            (when (>= (- (position-bytes (match-beginning 0)) 1) drop)
              (setq cut (match-beginning 0)))))
        (when cut
          (delete-region (point-min) cut)
          (goto-char (point-min))
          (insert (format "===== [older sessions trimmed to keep under %d bytes] =====\n"
                          max))
          (let ((coding-system-for-write 'utf-8))
            (write-region (point-min) (point-max) file nil 'silent)))))))

(defun elpaca-update-report-open-session (label)
  "Trim the persistent log, append a header for LABEL, and return the log path.
Returns nil when persistence is disabled or the header could not be written -
in which case the caller simply skips teeing.  Never signals."
  (when-let* ((file (elpaca-update-report-log-file)))
    (condition-case nil
        (progn
          (elpaca-update-report--trim-log file (elpaca-update-report-log-max-bytes))
          (write-region (format "%s%s [%s]\n" elpaca-update-report--log-marker
                                (format-time-string "%Y-%m-%d %H:%M:%S") label)
                        nil file 'append 'silent)
          file)
      (error nil))))

(defun elpaca-update-report-tee (file text)
  "Append TEXT as one ANSI-stripped line to persistent-log FILE.
No-op when FILE is nil; never signals, so a failed write cannot stall a run."
  (when file
    (condition-case nil
        (write-region
         (concat (replace-regexp-in-string
                  (concat (string 27) "\\[[0-9;]*m") "" text)
                 "\n")
         nil file 'append 'silent)
      (error nil))))

(provide 'elpaca-update-report)
;;; scripts/elpaca-update-report.el ends here
