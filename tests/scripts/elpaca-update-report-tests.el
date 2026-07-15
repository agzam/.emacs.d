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

;;; tests/scripts/elpaca-update-report-tests.el ends here
