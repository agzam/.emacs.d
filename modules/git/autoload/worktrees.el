;;; modules/git/autoload/worktrees.el -*- lexical-binding: t; -*-

(defun magit-worktree--merge-status (branch target)
  "Return how BRANCH relates to TARGET: `ancestor', `squashed', or nil.

`ancestor' means TARGET already contains BRANCH's commits (a normal or
fast-forward merge).  `squashed' means BRANCH's cumulative changes exist
in TARGET as a single patch-equivalent commit - a squash- or rebase-merged
PR whose commit SHAs were rewritten, which git's own --merged is blind to.
nil means BRANCH still carries work that is not in TARGET.

The squash test replays BRANCH's tree as a lone commit on the merge-base
and asks `git cherry' whether TARGET holds an equivalent patch, so a
branch with any commit not represented in TARGET never reports merged."
  (cond
   ((magit-rev-ancestor-p branch target) 'ancestor)
   ((when-let* ((base (magit-git-string "merge-base" target branch))
                (tree (magit-rev-parse (concat branch "^{tree}")))
                (synth (magit-git-string "commit-tree" tree "-p" base "-m" "_")))
      (seq-find (lambda (line) (string-prefix-p "-" line))
                (magit-git-lines "cherry" target synth)))
    'squashed)))

;;;###autoload
(defun magit-worktree-delete-merged ()
  "Delete worktrees whose branch is already merged, and their branches.

Merge detection (`magit-worktree--merge-status') recognizes both normal
merges and squash/rebase merges - the latter is what GitHub-style PR
workflows produce and what git's own --merged cannot see - judged against
the main branch's upstream (e.g. \"origin/main\"), or the local main branch
when there is no upstream.  A merged branch's worktree is removed with
\"git worktree remove\" and the branch with \"git branch -d\", or \"-D\" for a
squash-merged branch, which -d refuses even though the merge check has
already proven its work is in TARGET.

Left untouched: the primary and current worktrees, the main branch's own
worktree, and any that are bare, detached, locked, gone, or hold
uncommitted or untracked changes.  Fetch first to weigh branches against
freshly merged upstream commits.

Progress is logged to the echo area and *Messages* as each worktree is
examined, since the per-branch merge check spawns several git processes."
  (interactive)
  (unless (magit-toplevel)
    (user-error "Not inside a Git repository"))
  (let* ((main (or (magit-main-branch)
                   (user-error "No main branch found")))
         (target (or (magit-get-upstream-branch main) main))
         (worktrees (magit-list-worktrees))
         (primary (file-name-as-directory (caar worktrees)))
         (current (file-name-as-directory (magit-toplevel)))
         (total (length (cdr worktrees)))
         (idx 0)
         (note (lambda (name fmt &rest args)
                 (apply #'message
                        (concat "worktree-delete-merged: [%d/%d] %s " fmt)
                        idx total name args)))
         skipped candidates)
    (message "worktree-delete-merged: examining %d worktree(s) against %s"
             total target)
    (pcase-dolist (`(,path ,_commit ,branch ,bare ,detached ,locked ,_prunable)
                   (cdr worktrees))
      (setq idx (1+ idx))
      (let ((name (or branch (file-name-nondirectory (directory-file-name path))))
            status)
        (cond
         ((or bare detached locked (null branch)
              (equal branch main)
              (not (file-directory-p path))
              (file-equal-p path current))
          (funcall note name "- skipped (%s)"
                   (cond (bare "bare") (detached "detached") (locked "locked")
                         ((null branch) "no branch")
                         ((equal branch main) "main branch")
                         ((not (file-directory-p path)) "missing")
                         (t "current worktree"))))
         (t
          (funcall note name "- checking merge status...")
          ;; a synchronous loop never repaints on its own; force it so the
          ;; echo area updates before the slow per-branch git calls run
          (redisplay t)
          (setq status (magit-worktree--merge-status branch target))
          (cond
           ((null status) (funcall note name "- not merged, keeping"))
           ((let ((default-directory path))
              (or (magit-anything-modified-p) (magit-untracked-files)))
            (push (list path branch status) skipped)
            (funcall note name "- %s but dirty, skipping"
                     (if (eq status 'squashed) "squash-merged" "merged")))
           (t
            (push (list path branch status) candidates)
            (funcall note name "- %s, will delete"
                     (if (eq status 'squashed) "squash-merged" "merged"))))))))
    (setq candidates (nreverse candidates))
    (if (null candidates)
        (message "worktree-delete-merged: no merged worktrees to delete%s"
                 (if skipped
                     (format " (%d dirty merged worktree(s) skipped)"
                             (length skipped))
                   ""))
      (when (yes-or-no-p
             (format "Delete %d merged worktree(s) and their branches?\n%s\n"
                     (length candidates)
                     (mapconcat
                      (pcase-lambda (`(,path ,branch ,status))
                        (format "  %s  (%s%s)"
                                (abbreviate-file-name path) branch
                                (if (eq status 'squashed) ", squashed" "")))
                      candidates "\n")))
        (let ((default-directory primary)
              removed kept)
          (pcase-dolist (`(,path ,branch ,status) candidates)
            (message "worktree-delete-merged: removing worktree %s..." branch)
            (redisplay t)
            (if (zerop (magit-call-git "worktree" "remove" path))
                (progn
                  (push path removed)
                  (if (zerop (magit-call-git
                              "branch" (if (eq status 'ancestor) "-d" "-D")
                              branch))
                      (message "worktree-delete-merged: removed %s, deleted branch"
                               branch)
                    (push branch kept)
                    (message "worktree-delete-merged: removed %s, branch kept" branch)))
              (push (list path branch status) skipped)
              (message "worktree-delete-merged: could not remove %s" branch)))
          (magit-refresh)
          (message "worktree-delete-merged: removed %d worktree(s)%s%s"
                   (length removed)
                   (if kept
                       (format "; branch kept: %s"
                               (string-join (nreverse kept) ", "))
                     "")
                   (if skipped
                       (format "; skipped %d" (length skipped))
                     "")))))))
