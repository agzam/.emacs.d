;;; tests/e2e/pr-browse.el --- pull request urls reach the browser -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; code-review registers a browse-url handler for every GitHub PR url each
;; time its autoloads run: at boot, and again on every live package update.
;; Only a real boot shows that no browse path reaches that handler, however
;; late it was registered.

(require 'cl-lib)

(defvar pr-browse-url "https://github.com/agzam/foo/pull/12"
  "A PR of a repo forge does not track, so only code-review could claim it.")

(defvar pr-browse-pr
  `((url . ,pr-browse-url) (number . 12) (title . "Fix the thing")
    (state . "open") (isDraft . :false) (createdAt . "2026-01-01T00:00:00Z")
    (body . "") (author (login . "agzam") (url . "https://github.com/agzam"))
    (repository (nameWithOwner . "agzam/foo")))
  "`pr-browse-url' the way a github-topics search returns it.")

(defun pr-browse--run (label thunk)
  "Call THUNK under LABEL and expect its `browse-url' to reach the browser.
Both destinations are faked: the browser, and the code-review handler,
which would otherwise fetch the PR."
  (let (seen err)
    (cl-letf (((symbol-function 'code-review-browse-url)
               (lambda (url &rest _) (push (cons 'code-review url) seen))))
      (let ((browse-url-browser-function
             (lambda (url &rest _) (push (cons 'browser url) seen))))
        (condition-case e (funcall thunk) ((error quit) (setq err e)))))
    (let ((want (list (cons 'browser pr-browse-url))))
      (list :label (format "pr browse: %s" label)
            :ok (and (null err) (equal seen want))
            :got (format "%S" seen) :want (format "%S" want) :err err))))

(defun pr-browse--forge-browse ()
  "Run `forge-browse' the way C-c C-o runs it in a forge PR buffer."
  (cl-letf (((symbol-function 'forge--browse-target) (lambda () pr-browse-url)))
    (forge-browse)))

(defun pr-browse--code-review-key ()
  "Press C-c C-o in a code-review buffer reviewing `pr-browse-url'."
  (let ((buf (generate-new-buffer "*pr-browse code-review*")))
    (unwind-protect
        (with-current-buffer buf
          (switch-to-buffer buf)
          (code-review-mode)
          (cl-letf (((symbol-function 'code-review-db-get-pullreq)
                     (lambda ()
                       (make-instance 'code-review-github-repo
                                      :owner "agzam" :repo "foo" :number "12"))))
            (execute-kbd-macro (kbd "C-c C-o"))))
      (kill-buffer buf))))

(defun pr-browse--github-topics-act ()
  "Press C-; b o on `pr-browse-pr' in a github-topics read."
  (let ((github-topics-convert-body-with-pandoc nil)
        (map (make-sparse-keymap))
        (guard (run-with-timer 10 nil (lambda ()
                                         (when (active-minibuffer-window)
                                           (abort-recursive-edit))))))
    (define-key map [f12] (lambda ()
                            (interactive)
                            (github-topics--consult-prs (list pr-browse-pr))))
    (unwind-protect
        (progn
          (set-transient-map map)
          ;; embark runs the action once the command that opened the read
          ;; ends, and from a timer when no command loop gets to it
          (execute-kbd-macro (vconcat [f12] (kbd "C-; b o")))
          (accept-process-output nil 0.05))
      (cancel-timer guard))))

(defun pr-browse--rerun-code-review-autoloads ()
  "Evaluate code-review's autoloads again, as a live package update does."
  (load (locate-library "code-review-autoloads") nil 'nomessage))

(defun pr-browse-e2e ()
  "Every way to browse a PR lands in the browser."
  (require 'forge)
  (require 'github-topics)
  (let ((loaded (featurep 'code-review)))
    (list
     (pr-browse--run (format "forge-browse after boot (code-review loaded: %s)" loaded)
                     #'pr-browse--forge-browse)
     (pr-browse--run "C-; b o on a github-topics PR"
                     #'pr-browse--github-topics-act)
     (progn
       (require 'code-review)
       (pr-browse--run "forge-browse once code-review has loaded"
                       #'pr-browse--forge-browse))
     (pr-browse--run "C-c C-o in a code-review buffer"
                     #'pr-browse--code-review-key)
     (progn
       (pr-browse--rerun-code-review-autoloads)
       (pr-browse--run "forge-browse after a live code-review update"
                       #'pr-browse--forge-browse))
     (pr-browse--run "C-; b o on a github-topics PR after a live code-review update"
                     #'pr-browse--github-topics-act))))

(add-to-list 'e2e-scenarios #'pr-browse-e2e)
