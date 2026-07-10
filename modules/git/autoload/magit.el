;;; modules/git/autoload/magit.el -*- lexical-binding: t; -*-

;;; Quit

;;;###autoload
(defun magit-quit (&optional kill-buffer)
  "Bury the current magit buffer.

If KILL-BUFFER, kill this buffer instead of burying it.
If the buried/killed magit buffer was the last magit buffer open for this
repo, kill all magit buffers for this repo."
  (interactive "P")
  (let ((topdir (magit-toplevel)))
    (funcall magit-bury-buffer-function kill-buffer)
    (or (cl-find-if (lambda (win)
                      (with-selected-window win
                        (and (derived-mode-p 'magit-mode)
                             (equal magit--default-directory topdir))))
                    (window-list))
        (magit-quit-all))))

;;;###autoload
(defun magit-quit-all ()
  "Kill all magit buffers for the current repository."
  (interactive)
  (mapc #'magit-quit--kill-buffer (magit-mode-get-buffers))
  ;; Visible file buffers may be stale after git operations; buried ones are
  ;; covered by `doom-auto-revert-mode' when switched to (doom-defaults.el).
  (when (fboundp 'doom-auto-revert-buffers-h)
    (doom-auto-revert-buffers-h)))

(defun magit-quit--kill-buffer (buf)
  "Kill BUF, waiting out its live process if it has one."
  (when (and (bufferp buf) (buffer-live-p buf))
    (let ((process (get-buffer-process buf)))
      (if (not (processp process))
          (kill-buffer buf)
        (with-current-buffer buf
          (if (process-live-p process)
              (run-with-timer 5 nil #'magit-quit--kill-buffer buf)
            (kill-process process)
            (kill-buffer buf)))))))

;;; Log/diff/rebase extensions (appended to magit's transients in config.el)

;;;###autoload
(defun magit-log-orig_head--head (args files)
  "Compare log since the last pull: only commits between ORIG_HEAD and HEAD."
  (interactive (magit-log-arguments))
  (magit-log-other
   (list "ORIG_HEAD..HEAD")
   (car (magit-log-arguments)) files))

;;;###autoload
(defun magit-log-other--current (revision)
  "Log commits that differ between REVISION and the current branch."
  (interactive (list (magit-read-other-branch-or-commit "Log compare")))
  (magit-log-other
   (list (concat revision ".." (magit-get-current-branch)))
   (car (magit-log-arguments)) nil))

;;;###autoload
(defun magit-log--origin-main ()
  "Log commits between origin's main branch and the current branch."
  (interactive)
  (magit-log-other
   (list (format
          "origin/%s..%s"
          (magit-main-branch)
          (magit-get-current-branch)))
   (car (magit-log-arguments)) nil))

;;;###autoload
(defun magit-diff-range-reversed (rev-or-range &optional args files)
  "Diff between two branches, in `base..current' order unlike `diff-range'."
  (interactive (list (magit-read-other-branch-or-commit "Diff range")))
  (magit-diff-range (concat rev-or-range ".." (magit-get-current-branch)) args files))

;;;###autoload
(defun magit-diff--origin-main ()
  "Diff between origin's main branch and the current branch."
  (interactive)
  (magit-diff-range
   (format
    "origin/%s..%s"
    (magit-main-branch)
    (magit-get-current-branch))))

;;;###autoload
(defun magit-rebase-origin-main ()
  "Fetch latest of main and rebase the current branch on it."
  (interactive)
  (let* ((main (magit-main-branch))
         (remote (magit-get-remote main)))
    (magit-fetch-branch remote main nil)
    (magit-rebase-branch (format "%s/%s" remote main) nil)))

;;; Worktrees

(defun magit-create-branch-friendly-string (sentence)
  "From SENTENCE, e.g. a GitHub issue title, derive a git-branch-safe name."
  (let ((words
         (seq-map
          (lambda (word)
            (let* ((w (replace-regexp-in-string "?\\|@\\|~\\|\\^\\|\\/\\|\\\\" "-" word))
                   (w (downcase w))
                   (w (replace-regexp-in-string "_+\\|\\-+\\|'" "_" w))
                   ;; strip the ticket number
                   (w (replace-regexp-in-string "#\\([0-9]+\\)" "" w)))
              (string-trim w "_\\|-" "_\\|-")))
          (split-string sentence " \\|\\[\\|\\]\\|\\:\\|{\\|}" :omit-nulls " "))))
    (string-join
     (seq-take
      (seq-remove (lambda (w) (string-match-p "\\`[-_ ]*\\'" w)) words)
      8)
     "_")))

(defun forge-select-issue ()
  "Select an issue of the current repository; return (TITLE NUMBER-STRING).
doom.d's version leaned on `forge--format-topic-choice'/`forge-ls-issues',
both gone from current forge - `forge-read-issue' replaces the whole dance."
  (require 'forge)
  (let ((issue (forge-get-issue (forge-read-issue "Choose an issue: "))))
    (list (oref issue title)
          (number-to-string (oref issue number)))))

;;;###autoload
(defun magit-worktree-branch-from-issue ()
  "Select a forge issue, and create a worktree and a branch for it."
  (interactive)
  (let* ((sel-issue (forge-select-issue))
         (w-tree (format
                  "%s__%s"
                  (nth 1 sel-issue)
                  (magit-create-branch-friendly-string (car sel-issue))))
         (worktree? (not (string-suffix-p ".bare" (magit-rev-parse "--git-dir"))))
         (def-dir (if worktree?
                      (format "/%s/"
                              (string-join
                               (butlast
                                (seq-remove #'string-blank-p
                                            (file-name-split default-directory)))
                               "/"))
                    default-directory))
         (path (read-directory-name "Create new worktree at:" def-dir nil nil w-tree))
         (branch (magit-read-string-ns "With branch: " (last (file-name-split path)))))
    (if (magit-local-branch-p (format "refs/heads/%s" branch))
        (magit-run-git "worktree" "add" (magit--expand-worktree path) branch)
      (magit-run-git "worktree" "add" "-b"
                     branch (magit--expand-worktree path)
                     (magit-main-branch)))
    (magit-diff-visit-directory path)))

;;;###autoload
(defun magit-worktree-move-file (file worktree)
  "Move FILE to another WORKTREE preserving its relative path."
  (interactive
   (let* ((file (magit-read-file "Move file"))
          (path (expand-file-name file (magit-toplevel))))
     (list path (magit-completing-read
                 "Select worktree to move the file"
                 (thread-last
                   (magit-list-worktrees)
                   (seq-remove
                    (lambda (x)
                      (or (null (cadr x))
                          (file-equal-p
                           (car x)
                           (expand-file-name (directory-file-name (magit-toplevel))))))))))))
  (let* ((relname (file-relative-name file (magit-toplevel)))
         (dest (expand-file-name relname worktree))
         (status-buf (get-buffer
                      (format
                       "magit: %s"
                       (file-name-nondirectory
                        (directory-file-name worktree))))))
    (if (file-exists-p dest)
        (user-error "Already exists: %s" dest)
      (progn
        (make-directory (file-name-directory dest) :parents)
        (rename-file file dest)
        (magit-refresh)
        (if (and status-buf (get-buffer-window status-buf))
            (switch-to-buffer-other-window status-buf)
          (progn
            (split-window-right)
            (other-window 1)
            (magit-worktree-status worktree)))
        (magit-refresh)))))

;;; Cloning

;;;###autoload
(defun magit-clone-regular+ (&optional repository directory)
  "Simplified version of magit's cloning fn, for calling directly."
  (interactive)
  (let* ((repository (magit-read-string-ns "Clone from url or name"
                                           repository))
         (repository (if (string-match "^https://.*$" repository)
                         (git-https-url->ssh repository)
                       repository))
         (parts (parse-git-url repository))
         (org (plist-get parts :org))
         (cwd (file-name-as-directory
               (or directory
                   (if (functionp magit-clone-default-directory)
                       (funcall magit-clone-default-directory repository)
                     (expand-file-name org magit-clone-default-directory)))))
         (directory (file-name-as-directory
                     (expand-file-name
                      (read-directory-name
                       "Clone to: " cwd nil nil
                       (magit-clone--url-to-name repository)))))
         (default-directory cwd))
    (if (file-exists-p directory)
        (progn
          (message "%s exists already" directory)
          (dired directory))
      (unwind-protect
          (progn
            (unless (file-directory-p cwd)
              (make-directory cwd))
            (run-with-timer
             0.1 nil
             #'magit-clone-internal repository directory nil))
        (when (and (file-directory-p cwd)
                   (directory-empty-p cwd))
          (delete-directory cwd))))))

;;;###autoload
(defun magit-convert-to-bare ()
  "Convert current repo into a bare repo with worktrees layout.
Turns a normal clone into:
  repo-dir/.git/       (bare repo)
  repo-dir/<branch>/   (worktree for current branch)

Refuses to run on dirty trees, existing bare repos, or repos
that already have worktrees."
  (interactive)
  (let* ((toplevel (magit-toplevel))
         (gitdir (and toplevel (expand-file-name ".git" toplevel)))
         (branch (and toplevel (magit-get-current-branch))))
    (cond
     ((null toplevel)
      (user-error "Not inside a Git repository"))
     ((magit-bare-repo-p)
      (user-error "Already a bare repository"))
     ((magit-anything-modified-p)
      (user-error "Working tree has uncommitted changes"))
     ((< 1 (length (magit-list-worktrees)))
      (user-error "Repository already has worktrees - convert manually"))
     ((not (file-directory-p gitdir))
      (user-error ".git is not a directory (already a worktree?)"))
     ((null branch)
      (user-error "HEAD is detached - check out a branch first"))
     ((not (yes-or-no-p
            (format "Convert %s to bare repo with worktree for '%s'? "
                    (abbreviate-file-name toplevel) branch)))
      (user-error "Aborted")))
    (let ((tmpdir (make-temp-file "git-bare-" t))
          (worktree-path (expand-file-name branch toplevel)))
      ;; move .git to temp location
      (rename-file gitdir (expand-file-name ".git" tmpdir) t)
      ;; remove all working tree files
      (dolist (f (directory-files toplevel t))
        (let ((name (file-name-nondirectory f)))
          (unless (member name '("." ".."))
            (if (file-directory-p f)
                (delete-directory f t)
              (delete-file f)))))
      ;; move .git back
      (rename-file (expand-file-name ".git" tmpdir) gitdir)
      (delete-directory tmpdir)
      ;; configure as bare
      (let ((default-directory toplevel))
        (magit-git "config" "--bool" "core.bare" "true")
        (magit-git "config" "remote.origin.fetch"
                   "+refs/heads/*:refs/remotes/origin/*")
        ;; create worktree for the branch we were on
        (magit-git "worktree" "add" worktree-path branch))
      (magit-diff-visit-directory worktree-path)
      (message "Converted. Worktree at: %s" worktree-path))))

;;;###autoload
(defun magit-python-which-function ()
  "Like `magit-which-function' but strip class prefix from Python names.
Git's -L flag doesn't support Class.method notation for Python."
  (when-let* ((name (magit-which-function)))
    (if (string-match "\\." name)
        (replace-regexp-in-string ".*\\." "" name)
      name)))
