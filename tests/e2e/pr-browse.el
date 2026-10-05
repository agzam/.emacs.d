;;; tests/e2e/pr-browse.el --- pull request urls reach the browser -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; code-review registers a browse-url handler for every GitHub PR url: from
;; its autoloads at boot, and again as code-review-browse loads.  The config
;; drops it after both.  Only a real boot runs them in their real order; the
;; batch suite evaluates the config's forms by hand.

(require 'cl-lib)

(defvar pr-browse-url "https://github.com/agzam/foo/pull/12"
  "A PR of a repo forge does not track, so only code-review could claim it.")

(defun pr-browse--run (label thunk)
  "Call THUNK under LABEL and expect its `browse-url' to reach the browser.
Both destinations are faked: the browser, and the code-review handler,
which would otherwise fetch the PR."
  (let (seen err)
    (cl-letf (((symbol-function 'code-review-browse-url)
               (lambda (url &rest _) (push (cons 'code-review url) seen))))
      (let ((browse-url-browser-function
             (lambda (url &rest _) (push (cons 'browser url) seen))))
        (condition-case e (funcall thunk) (error (setq err e)))))
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

(defun pr-browse-e2e ()
  "Every way to browse a PR lands in the browser."
  (require 'forge)
  (let ((loaded (featurep 'code-review)))
    (list
     (pr-browse--run (format "forge-browse after boot (code-review loaded: %s)" loaded)
                     #'pr-browse--forge-browse)
     (progn
       (require 'code-review)
       (pr-browse--run "forge-browse once code-review has loaded"
                       #'pr-browse--forge-browse))
     (pr-browse--run "C-c C-o in a code-review buffer"
                     #'pr-browse--code-review-key))))

(add-to-list 'e2e-scenarios #'pr-browse-e2e)
