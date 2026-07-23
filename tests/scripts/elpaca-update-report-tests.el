;;; tests/scripts/elpaca-update-report-tests.el --- update audit-trail specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

(load-module-file "scripts/elpaca-update-report.el")

;; In the buttercup sandbox Elpaca itself is not installed, so the struct
;; accessors the block helper reaches for are void.  Give them inert stubs the
;; specs override via `cl-letf' (whose getter would otherwise choke on a void
;; function cell).
(unless (fboundp 'elpaca<-source-dir) (defun elpaca<-source-dir (_e) nil))
(unless (fboundp 'elpaca--url) (defun elpaca--url (_e) nil))

(defun elpaca-update-report-tests--git (dir &rest args)
  "Run git ARGS in DIR, discarding output."
  (let ((default-directory (file-name-as-directory dir)))
    (apply #'call-process "git" nil nil nil args)))

(defun elpaca-update-report-tests--repo (subjects)
  "Create a temp git repo with one commit per string in SUBJECTS.
Return a plist (:dir DIR :revs (SHA...)) with revs oldest-first."
  (let ((dir (make-temp-file "eur-spec" t))
        (n 0))
    (elpaca-update-report-tests--git dir "init" "-q")
    (elpaca-update-report-tests--git dir "config" "user.email" "t@example.com")
    (elpaca-update-report-tests--git dir "config" "user.name" "Tester")
    (elpaca-update-report-tests--git dir "config" "commit.gpgsign" "false")
    (dolist (s subjects)
      (write-region (format "%d\n" (cl-incf n)) nil (expand-file-name "f" dir))
      (elpaca-update-report-tests--git dir "add" "-A")
      (elpaca-update-report-tests--git dir "commit" "-q" "-m" s))
    (list :dir dir
          :revs (with-temp-buffer
                  (let ((default-directory (file-name-as-directory dir)))
                    (call-process "git" nil t nil "--no-pager" "log"
                                  "--pretty=%H" "--reverse"))
                  (split-string (buffer-string) "\n" t)))))

(describe "elpaca-update-report--compare-url"
  (it "uses the GitHub/Gitea compare form"
    (expect (elpaca-update-report--compare-url "https://github.com/a/b" "o" "n")
            :to-equal "https://github.com/a/b/compare/o...n")
    (expect (elpaca-update-report--compare-url "https://codeberg.org/a/b" "o" "n")
            :to-equal "https://codeberg.org/a/b/compare/o...n"))
  (it "nests GitLab compares under /-/"
    (expect (elpaca-update-report--compare-url "https://gitlab.com/a/b" "o" "n")
            :to-equal "https://gitlab.com/a/b/-/compare/o...n"))
  (it "falls back to the bare repo URL for an unknown forge"
    (expect (elpaca-update-report--compare-url "https://git.sr.ht/~a/b" "o" "n")
            :to-equal "https://git.sr.ht/~a/b"))
  (it "returns nil without a URL"
    (expect (elpaca-update-report--compare-url nil "o" "n") :to-be nil)))

(describe "elpaca-update-report-log-limit"
  (it "defaults to 20 when unset"
    (let ((process-environment (cons "ELPACA_UPDATE_LOG_LIMIT" process-environment)))
      (expect (elpaca-update-report-log-limit) :to-equal 20)))
  (it "honors the env override"
    (let ((process-environment (cons "ELPACA_UPDATE_LOG_LIMIT=5" process-environment)))
      (expect (elpaca-update-report-log-limit) :to-equal 5)))
  (it "clamps a negative limit to 0 (unlimited)"
    (let ((process-environment (cons "ELPACA_UPDATE_LOG_LIMIT=-3" process-environment)))
      (expect (elpaca-update-report-log-limit) :to-equal 0))))

(describe "elpaca-update-report--git"
  (it "returns nil for a non-existent directory"
    (expect (elpaca-update-report--git "/no/such/dir" "rev-parse" "HEAD") :to-be nil))
  (it "runs git in a real repo"
    (let* ((repo (elpaca-update-report-tests--repo '("only")))
           (dir (plist-get repo :dir)))
      (unwind-protect
          (expect (elpaca-update-report--git dir "rev-parse" "HEAD")
                  :to-equal (car (last (plist-get repo :revs))))
        (delete-directory dir t)))))

(describe "elpaca-update-report--paint"
  (it "wraps in an SGR pair when color is on"
    (let ((elpaca-update-report-color t))
      (expect (elpaca-update-report--paint "33" "abc")
              :to-equal (concat (string 27) "[33mabc" (string 27) "[0m"))))
  (it "returns the string unchanged when color is off"
    (let ((elpaca-update-report-color nil))
      (expect (elpaca-update-report--paint "33" "abc") :to-equal "abc")))
  (it "leaves an empty string alone even with color on"
    (let ((elpaca-update-report-color t))
      (expect (elpaca-update-report--paint "33" "") :to-equal ""))))

(describe "elpaca-update-report--head"
  (it "matches git rev-parse for a plain clone"
    (let* ((repo (elpaca-update-report-tests--repo '("a" "b")))
           (dir (plist-get repo :dir)))
      (unwind-protect
          (expect (elpaca-update-report--head dir)
                  :to-equal (elpaca-update-report--git dir "rev-parse" "HEAD"))
        (delete-directory dir t))))
  (it "reads a detached HEAD"
    (let* ((repo (elpaca-update-report-tests--repo '("a" "b")))
           (dir (plist-get repo :dir))
           (first (car (plist-get repo :revs))))
      (unwind-protect
          (progn
            (elpaca-update-report-tests--git dir "checkout" "-q" first)
            (expect (elpaca-update-report--head dir) :to-equal first))
        (delete-directory dir t))))
  (it "resolves a packed ref with the loose copy gone"
    (let* ((repo (elpaca-update-report-tests--repo '("a")))
           (dir (plist-get repo :dir))
           (sha (car (plist-get repo :revs))))
      (unwind-protect
          (progn
            (elpaca-update-report-tests--git dir "pack-refs" "--all")
            (let ((heads (expand-file-name ".git/refs/heads" dir)))
              (when (file-directory-p heads) (delete-directory heads t)))
            (expect (elpaca-update-report--head dir) :to-equal sha))
        (delete-directory dir t))))
  (it "returns nil outside a git repo root (e.g. an in-tree local package)"
    (let ((dir (make-temp-file "eur-bare" t)))
      (unwind-protect
          (expect (elpaca-update-report--head dir) :to-be nil)
        (delete-directory dir t)))))

(describe "elpaca-update-report--render"
  (let (dir revs)
    (before-each
      (let ((repo (elpaca-update-report-tests--repo
                   '("add one" "add two" "add three" "add four"))))
        (setq dir (plist-get repo :dir) revs (plist-get repo :revs))))
    (after-each (delete-directory dir t))

    (it "lists commits newest-first with a compare link"
      (let ((out (elpaca-update-report--render
                  dir "https://github.com/a/b" (nth 0 revs) (nth 3 revs))))
        (expect out :to-match "3 commits")
        (expect out :to-match "github.com/a/b/compare/")
        (expect out :to-match "add four")
        (expect out :to-match "add two")
        (expect (< (string-match-p "add four" out) (string-match-p "add two" out))
                :to-be-truthy)))

    (it "caps the list and notes the remainder"
      (let* ((process-environment (cons "ELPACA_UPDATE_LOG_LIMIT=2" process-environment))
             (out (elpaca-update-report--render
                   dir "https://github.com/a/b" (nth 0 revs) (nth 3 revs))))
        (expect out :to-match "add four")
        (expect out :to-match "1 more")
        (expect out :not :to-match "add two")))

    (it "labels a pinned rollback"
      (let ((out (elpaca-update-report--render
                  dir "https://github.com/a/b" (nth 3 revs) (nth 0 revs))))
        (expect out :to-match "rolled back 3 commits")))

    (it "returns nil when HEAD did not move"
      (expect (elpaca-update-report--render
               dir "https://github.com/a/b" (nth 0 revs) (nth 0 revs))
              :to-be nil))

    (it "omits the compare line without a URL"
      (let ((out (elpaca-update-report--render dir nil (nth 0 revs) (nth 3 revs))))
        (expect out :not :to-match "compare/")
        (expect out :to-match "add four")))

    (it "wraps shas in SGR escapes when color is on"
      (let* ((elpaca-update-report-color t)
             (out (elpaca-update-report--render
                   dir "https://github.com/a/b" (nth 0 revs) (nth 3 revs))))
        (expect out :to-match (concat (string 27) "\\[33m")) ; yellow sha
        (expect out :to-match "add four")))                  ; subject stays literal

    (it "emits no escapes when color is off"
      (let ((elpaca-update-report-color nil))
        (expect (elpaca-update-report--render
                 dir "https://github.com/a/b" (nth 0 revs) (nth 3 revs))
                :not :to-match (string 27))))))

(describe "elpaca-update-report-block"
  (let (dir revs)
    (before-each
      (let ((repo (elpaca-update-report-tests--repo '("first" "second"))))
        (setq dir (plist-get repo :dir) revs (plist-get repo :revs))))
    (after-each (delete-directory dir t))

    (it "renders the block for a package whose HEAD moved"
      (let ((snapshot (make-hash-table :test 'equal)))
        (puthash dir (nth 0 revs) snapshot)
        (cl-letf (((symbol-function 'elpaca<-source-dir) (lambda (_e) dir))
                  ((symbol-function 'elpaca--url) (lambda (_e) "https://github.com/a/b")))
          (let ((out (elpaca-update-report-block 'fake snapshot)))
            (expect out :to-match "1 commit")
            (expect out :to-match "github.com/a/b/compare/")
            (expect out :to-match "second")))))

    (it "returns nil when the repo is absent from the snapshot"
      (cl-letf (((symbol-function 'elpaca<-source-dir) (lambda (_e) dir)))
        (expect (elpaca-update-report-block 'fake (make-hash-table :test 'equal))
                :to-be nil)))))

(describe "elpaca-update-report pulled-line batcher"
  (it "collects names into a single pulled: line on flush"
    (let ((b (elpaca-update-report-batcher 72)) out)
      (let ((emit (lambda (fmt &rest args) (push (apply #'format fmt args) out))))
        (elpaca-update-report-note b emit "aa")
        (elpaca-update-report-note b emit "bb")
        (expect out :to-be nil)               ; buffered, nothing emitted yet
        (elpaca-update-report-flush b emit)
        (expect out :to-equal '("pulled: aa, bb")))))

  (it "auto-flushes once a line would overflow the width"
    (let ((b (elpaca-update-report-batcher 12)) out)
      (let ((emit (lambda (fmt &rest args) (push (apply #'format fmt args) out))))
        (elpaca-update-report-note b emit "aaaa")
        (elpaca-update-report-note b emit "bbbb") ; overflow: flush aaaa, buffer bbbb
        (elpaca-update-report-flush b emit)
        (expect (nreverse out) :to-equal '("pulled: aaaa" "pulled: bbbb")))))

  (it "flushing an empty batcher emits nothing"
    (let ((b (elpaca-update-report-batcher)) (n 0))
      (elpaca-update-report-flush b (lambda (&rest _) (cl-incf n)))
      (expect n :to-equal 0))))

(describe "elpaca-update-report-format-pending"
  (it "renders id=status pairs joined by commas"
    (expect (elpaca-update-report-format-pending '((a . queued) (b . building)))
            :to-equal "a=queued, b=building"))
  (it "caps the list and counts the remainder"
    (expect (elpaca-update-report-format-pending '((a . x) (b . x) (c . x)) 2)
            :to-equal "a=x, b=x, … +1 more"))
  (it "is empty for no pending work"
    (expect (elpaca-update-report-format-pending nil) :to-equal "")))

(describe "elpaca-update-report-log-file"
  (it "defaults to /tmp/elpaca-update.log when unset"
    (let ((process-environment (cons "ELPACA_UPDATE_LOG_FILE" process-environment)))
      (expect (elpaca-update-report-log-file) :to-equal "/tmp/elpaca-update.log")))
  (it "treats an empty value as disabled"
    (let ((process-environment (cons "ELPACA_UPDATE_LOG_FILE=" process-environment)))
      (expect (elpaca-update-report-log-file) :to-be nil)))
  (it "honors an explicit path"
    (let ((process-environment (cons "ELPACA_UPDATE_LOG_FILE=/x/y.log" process-environment)))
      (expect (elpaca-update-report-log-file) :to-equal "/x/y.log"))))

(describe "elpaca-update-report-log-max-bytes"
  (it "defaults to 2 MiB when unset"
    (let ((process-environment (cons "ELPACA_UPDATE_LOG_MAX_BYTES" process-environment)))
      (expect (elpaca-update-report-log-max-bytes) :to-equal (* 2 1024 1024))))
  (it "honors the env override"
    (let ((process-environment (cons "ELPACA_UPDATE_LOG_MAX_BYTES=4096" process-environment)))
      (expect (elpaca-update-report-log-max-bytes) :to-equal 4096)))
  (it "clamps a negative cap to 0 (grow forever)"
    (let ((process-environment (cons "ELPACA_UPDATE_LOG_MAX_BYTES=-1" process-environment)))
      (expect (elpaca-update-report-log-max-bytes) :to-equal 0))))

(describe "elpaca-update-report-progress"
  (it "streams to stderr, bypassing the block-buffered stdout"
    ;; --batch buffers stdout unless it's a TTY, so progress must go to the
    ;; unbuffered stderr stream instead - and never leak onto stdout.
    (let ((captured "")
          (stdout (generate-new-buffer " *stdout*")))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'external-debugging-output)
                       (lambda (ch) (setq captured (concat captured (char-to-string ch))))))
              (let ((standard-output stdout))
                (elpaca-update-report-progress "still working: foo")))
            (expect captured :to-equal "still working: foo\n")
            (expect (with-current-buffer stdout (buffer-string)) :to-equal ""))
        (kill-buffer stdout)))))

(describe "elpaca-update-report-tee"
  (it "appends a line, stripping ANSI color"
    (let ((f (make-temp-file "eur-tee")))
      (unwind-protect
          (progn
            (elpaca-update-report-tee f (concat (string 27) "[33mabc" (string 27) "[0m"))
            (elpaca-update-report-tee f "plain")
            (expect (with-temp-buffer (insert-file-contents f) (buffer-string))
                    :to-equal "abc\nplain\n"))
        (delete-file f))))
  (it "is a no-op for a nil file"
    (expect (elpaca-update-report-tee nil "x") :to-be nil))
  (it "never signals when the target is unwritable"
    (expect (elpaca-update-report-tee "/no/such/dir/x.log" "x") :to-be nil)))

(describe "elpaca-update-report-open-session"
  (it "appends a labelled session header and returns the file"
    (let* ((f (make-temp-file "eur-session"))
           (process-environment
            (append (list (concat "ELPACA_UPDATE_LOG_FILE=" f)
                          "ELPACA_UPDATE_LOG_MAX_BYTES=0")
                    process-environment)))
      (unwind-protect
          (progn
            (expect (elpaca-update-report-open-session "live") :to-equal f)
            (expect (with-temp-buffer (insert-file-contents f) (buffer-string))
                    :to-match "^===== update session .* \\[live\\]$"))
        (delete-file f))))
  (it "returns nil when persistence is disabled"
    (let ((process-environment (cons "ELPACA_UPDATE_LOG_FILE=" process-environment)))
      (expect (elpaca-update-report-open-session "live") :to-be nil))))

(describe "elpaca-update-report--trim-log"
  (it "drops whole oldest sessions to fit under max, keeping the newest"
    (let ((f (make-temp-file "eur-trim")))
      (unwind-protect
          (progn
            (with-temp-buffer
              (insert "===== update session S1 [x]\n" (make-string 200 ?a) "\n")
              (insert "===== update session S2 [x]\n" (make-string 200 ?b) "\n")
              (insert "===== update session S3 [x]\n" (make-string 200 ?c) "\n")
              (write-region (point-min) (point-max) f nil 'silent))
            (elpaca-update-report--trim-log f 300)
            (let ((out (with-temp-buffer (insert-file-contents f) (buffer-string))))
              (expect out :to-match "older sessions trimmed")
              (expect out :to-match "S3")
              (expect out :not :to-match "S1")
              (expect out :to-match "^===== update session S")))
        (delete-file f))))
  (it "leaves a single oversized session intact (a session is atomic)"
    (let ((f (make-temp-file "eur-trim1")))
      (unwind-protect
          (progn
            (with-temp-buffer
              (insert "===== update session S1 [x]\n" (make-string 500 ?a) "\n")
              (write-region (point-min) (point-max) f nil 'silent))
            (elpaca-update-report--trim-log f 100)
            (expect (with-temp-buffer (insert-file-contents f) (buffer-string))
                    :to-match "S1"))
        (delete-file f))))
  (it "is a no-op when max is 0 (grow forever)"
    (let ((f (make-temp-file "eur-trim0")))
      (unwind-protect
          (progn
            (write-region "big\n" nil f nil 'silent)
            (elpaca-update-report--trim-log f 0)
            (expect (with-temp-buffer (insert-file-contents f) (buffer-string))
                    :to-equal "big\n"))
        (delete-file f)))))

;;; tests/scripts/elpaca-update-report-tests.el ends here
