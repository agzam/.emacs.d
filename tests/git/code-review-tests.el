;;; tests/git/code-review-tests.el --- git/autoload/code-review.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'eieio)

(load-module-file "modules/git/autoload/code-review.el")

;; forge's default; valued, so the command's let-bind is dynamic here too
(defvar forge-browse-topics-using-forge t)

(defclass code-review-tests-pr ()
  ((owner :initarg :owner)
   (repo :initarg :repo)
   (number :initarg :number))
  "Stand-in for the PR object `code-review-db-get-pullreq' returns.")

(describe "code-review-browse-pr"
  :var (seen)

  (before-each
    (setq seen nil)
    (spy-on 'code-review-db-get-pullreq :and-return-value
            (make-instance 'code-review-tests-pr
                           :owner "agzam" :repo "foo" :number "12"))
    (spy-on 'browse-url :and-call-fake
            (lambda (url &rest _)
              (push (list url forge-browse-topics-using-forge) seen))))

  (it "opens the reviewed PR on GitHub"
    (code-review-browse-pr)
    (expect (caar seen) :to-equal "https://github.com/agzam/foo/pull/12"))

  ;; forge's browse-url handler takes a PR of a repo it tracks to a forge
  ;; buffer instead of the browser
  (it "keeps forge's handler away from the url"
    (code-review-browse-pr)
    (expect (cadar seen) :to-be nil)))

;;; tests/git/code-review-tests.el ends here
