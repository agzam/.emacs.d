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

(defvar elpaca-update-report-color nil
  "When non-nil, wrap changelog shas and dates in ANSI SGR color codes.
The drivers set this per run from a flag `bb update' raises only when its own
stdout is a terminal, so a piped or redirected update stays plain.")

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

(provide 'elpaca-update-report)
;;; scripts/elpaca-update-report.el ends here
