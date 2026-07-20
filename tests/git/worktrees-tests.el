;;; tests/git/worktrees-tests.el --- git/autoload/worktrees.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/git/autoload/worktrees.el")

;; --- merge detection ---------------------------------------------------------
;; magit isn't installed in the batch sandbox, so the git plumbing is stubbed.
;; These specs pin the classification the squash detection produces; they mirror
;; what was verified live against a real squash-merged bare+worktrees repo.
(describe "magit-worktree--merge-status"
  (it "reports ancestor when the branch is reachable from target"
    (cl-letf (((symbol-function 'magit-rev-ancestor-p) (lambda (&rest _) t)))
      (expect (magit-worktree--merge-status "b" "origin/main") :to-be 'ancestor)))

  (it "reports squashed when git cherry finds an equivalent patch"
    (cl-letf (((symbol-function 'magit-rev-ancestor-p) (lambda (&rest _) nil))
              ((symbol-function 'magit-git-string)
               (lambda (cmd &rest _)
                 (pcase cmd ("merge-base" "base") ("commit-tree" "synth"))))
              ((symbol-function 'magit-rev-parse) (lambda (&rest _) "tree"))
              ((symbol-function 'magit-git-lines) (lambda (&rest _) '("- abc123"))))
      (expect (magit-worktree--merge-status "b" "origin/main") :to-be 'squashed)))

  (it "returns nil when the branch has a commit not in target"
    ;; a `+' line is a branch commit with no equivalent in target
    (cl-letf (((symbol-function 'magit-rev-ancestor-p) (lambda (&rest _) nil))
              ((symbol-function 'magit-git-string)
               (lambda (cmd &rest _)
                 (pcase cmd ("merge-base" "base") ("commit-tree" "synth"))))
              ((symbol-function 'magit-rev-parse) (lambda (&rest _) "tree"))
              ((symbol-function 'magit-git-lines) (lambda (&rest _) '("+ abc123"))))
      (expect (magit-worktree--merge-status "b" "origin/main") :to-be nil)))

  (it "returns nil when histories share no merge-base"
    (cl-letf (((symbol-function 'magit-rev-ancestor-p) (lambda (&rest _) nil))
              ((symbol-function 'magit-git-string) (lambda (&rest _) nil))
              ((symbol-function 'magit-rev-parse) (lambda (&rest _) "tree"))
              ((symbol-function 'magit-git-lines) (lambda (&rest _) nil)))
      (expect (magit-worktree--merge-status "b" "origin/main") :to-be nil))))

;; --- orchestration -----------------------------------------------------------
;; The command's selection/deletion loop, with `magit-worktree--merge-status'
;; stubbed per branch so each spec drives a specific mix of merge states.
(defvar worktrees-tests--git-calls nil)
(defvar worktrees-tests--worktrees nil)
(defvar worktrees-tests--status nil
  "Alist of branch name -> merge status returned by the stubbed detector.")
(defvar worktrees-tests--dirty nil
  "List of worktree paths that should report uncommitted changes.")

(defmacro worktrees-tests--run (&rest body)
  "Run BODY with all magit worktree dependencies stubbed."
  `(let ((worktrees-tests--git-calls nil)
         (real-file-directory-p (symbol-function 'file-directory-p)))
     (cl-letf (((symbol-function 'magit-toplevel)
                (lambda (&rest _) "/repo/main/"))
               ((symbol-function 'magit-main-branch) (lambda () "main"))
               ((symbol-function 'magit-get-upstream-branch)
                (lambda (&rest _) "origin/main"))
               ((symbol-function 'magit-list-worktrees)
                (lambda () worktrees-tests--worktrees))
               ((symbol-function 'magit-worktree--merge-status)
                (lambda (branch &optional _target)
                  (cdr (assoc branch worktrees-tests--status))))
               ((symbol-function 'magit-anything-modified-p)
                (lambda (&rest _)
                  (and (member (directory-file-name default-directory)
                               (mapcar #'directory-file-name
                                       worktrees-tests--dirty))
                       t)))
               ((symbol-function 'magit-untracked-files) (lambda (&rest _) nil))
               ((symbol-function 'magit-refresh) (lambda (&rest _) nil))
               ;; fake worktree paths don't exist on disk
               ((symbol-function 'file-directory-p)
                (lambda (p) (or (string-prefix-p "/repo/" p)
                                (funcall real-file-directory-p p))))
               ((symbol-function 'magit-call-git)
                (lambda (&rest args)
                  (push args worktrees-tests--git-calls)
                  0)))
       ,@body
       (nreverse worktrees-tests--git-calls))))

(describe "magit-worktree-delete-merged"
  (before-each
    ;; PATH COMMIT BRANCH BARE DETACHED LOCKED PRUNABLE
    (setq worktrees-tests--worktrees
          '(("/repo/.git/"     "c0" nil           t   nil nil nil)
            ("/repo/main/"     "c1" "main"        nil nil nil nil)
            ("/repo/anc/"      "c2" "anc"         nil nil nil nil)
            ("/repo/squash/"   "c3" "squash"      nil nil nil nil)
            ("/repo/open/"     "c4" "open"        nil nil nil nil)
            ("/repo/dirty/"    "c5" "dirty"       nil nil nil nil)
            ("/repo/bare/"     "c6" nil           t   nil nil nil)
            ("/repo/detached/" "c7" nil           nil t   nil nil)
            ("/repo/locked/"   "c8" "locked-br"   nil nil t   nil))
          worktrees-tests--status '(("anc" . ancestor)
                                    ("squash" . squashed)
                                    ("dirty" . squashed)
                                    ("locked-br" . squashed))
          worktrees-tests--dirty '("/repo/dirty/")))

  (it "removes merged clean worktrees, deleting squashed branches with -D"
    (let ((calls (worktrees-tests--run
                  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                    (magit-worktree-delete-merged)))))
      (expect calls :to-equal
              '(("worktree" "remove" "/repo/anc/")
                ("branch" "-d" "anc")
                ("worktree" "remove" "/repo/squash/")
                ("branch" "-D" "squash")))))

  (it "marks squashed branches in the confirmation prompt"
    (let (prompt)
      (worktrees-tests--run
       (cl-letf (((symbol-function 'yes-or-no-p)
                  (lambda (p) (setq prompt p) nil)))
         (magit-worktree-delete-merged)))
      (expect prompt :to-match "/repo/squash/  (squash, squashed)")
      (expect prompt :to-match "/repo/anc/  (anc)")
      (expect prompt :not :to-match "/repo/open/")
      (expect prompt :not :to-match "/repo/dirty/")))

  (it "does nothing when the confirmation is declined"
    (let ((calls (worktrees-tests--run
                  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
                    (magit-worktree-delete-merged)))))
      (expect calls :to-equal nil)))

  (it "issues no git calls when nothing is merged"
    (setq worktrees-tests--status nil)
    (let (asked
          (calls (worktrees-tests--run
                  (cl-letf (((symbol-function 'yes-or-no-p)
                             (lambda (&rest _) (setq asked t) t)))
                    (magit-worktree-delete-merged)))))
      (expect calls :to-equal nil)
      (expect asked :to-be nil)))

  (it "keeps the branch when git refuses to delete it"
    (let ((calls (worktrees-tests--run
                  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                            ((symbol-function 'magit-call-git)
                             (lambda (&rest args)
                               (push args worktrees-tests--git-calls)
                               ;; worktree removes ok, branch deletes fail
                               (if (equal (car args) "branch") 1 0))))
                    (magit-worktree-delete-merged)))))
      (expect calls :to-equal
              '(("worktree" "remove" "/repo/anc/")
                ("branch" "-d" "anc")
                ("worktree" "remove" "/repo/squash/")
                ("branch" "-D" "squash")))))

  (it "errors when run outside a repository"
    (cl-letf (((symbol-function 'magit-toplevel) (lambda (&rest _) nil)))
      (expect (magit-worktree-delete-merged) :to-throw 'user-error))))
