;;; tests/jira/jira-tests.el --- jira/autoload/jira.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; The module autoloads lean on go-jira internals plus the web-browsing/git
;; module fns, none loadable in the batch tier; each spec stubs only what its
;; branch touches.  `with-fake-feature' neutralises the functions' own
;; (require 'go-jira).
(load-module-file "modules/jira/autoload/jira.el")

(describe "go-jira-browse-ticket-url"
  (it "activates an already-open browser tab instead of shelling out"
    (with-fake-feature 'go-jira
      (let ((system-type 'darwin)
            activated shelled)
        (cl-letf (((symbol-function 'go-jira--find-exe) (lambda (&rest _) "jira"))
                  ((symbol-function 'go-jira-ticket->url)
                   (lambda (_) "https://jira/browse/SAC-1"))
                  ((symbol-function 'browser-get-tabs)
                   (lambda ()
                     '((:url "https://other"             :windowIndex 1 :tabIndex 1)
                       (:url "https://jira/browse/SAC-1" :windowIndex 2 :tabIndex 3))))
                  ((symbol-function 'browser-activate-tab)
                   (lambda (win tab) (setq activated (list win tab))))
                  ((symbol-function 'shell-command-to-string)
                   (lambda (&rest _) (setq shelled t) "")))
          (go-jira-browse-ticket-url "SAC-1")
          (expect activated :to-equal '(2 3))
          (expect shelled :to-be nil)))))

  (it "shells out to `jira browse' when no tab matches"
    (with-fake-feature 'go-jira
      (let ((system-type 'darwin)
            activated cmd)
        (cl-letf (((symbol-function 'go-jira--find-exe) (lambda (&rest _) "jira"))
                  ((symbol-function 'go-jira-ticket->url)
                   (lambda (_) "https://jira/browse/SAC-9"))
                  ((symbol-function 'browser-get-tabs) (lambda () nil))
                  ((symbol-function 'browser-activate-tab)
                   (lambda (&rest _) (setq activated t)))
                  ((symbol-function 'shell-command-to-string)
                   (lambda (c) (setq cmd c) "")))
          (go-jira-browse-ticket-url "SAC-9")
          (expect activated :to-be nil)
          (expect cmd :to-equal "jira browse SAC-9")))))

  (it "shells out on non-darwin without scraping browser tabs"
    (with-fake-feature 'go-jira
      (let ((system-type 'gnu/linux)
            cmd)
        (cl-letf (((symbol-function 'go-jira--find-exe) (lambda (&rest _) "jira"))
                  ((symbol-function 'go-jira-ticket->url)
                   (lambda (_) "https://jira/browse/SAC-2"))
                  ((symbol-function 'browser-get-tabs)
                   (lambda () (error "must not scrape tabs off-darwin")))
                  ((symbol-function 'shell-command-to-string)
                   (lambda (c) (setq cmd c) "")))
          (go-jira-browse-ticket-url "SAC-2")
          (expect cmd :to-equal "jira browse SAC-2")))))

  (it "requires go-jira before touching its private helpers"
    ;; the private go-jira--find-exe carries no autoload cookie; a cold M-x
    ;; must pull the package in first (the doom.d version void-errored here).
    (let ((required nil))
      (cl-letf* ((real-require (symbol-function 'require))
                 ((symbol-function 'require)
                  (lambda (f &optional fn noerr)
                    (when (eq f 'go-jira) (setq required t))
                    (unless (eq f 'go-jira) (funcall real-require f fn noerr))))
                 ((symbol-function 'go-jira--find-exe) (lambda (&rest _) "jira"))
                 ((symbol-function 'go-jira-ticket->url) (lambda (_) "u"))
                 ((symbol-function 'browser-get-tabs) (lambda () nil))
                 ((symbol-function 'shell-command-to-string) (lambda (&rest _) "")))
        (let ((system-type 'darwin))
          (go-jira-browse-ticket-url "SAC-3"))
        (expect required :to-be t)))))

(describe "go-jira-find-pull-requests-on-github"
  (it "searches GitHub for the ticket at point"
    (with-fake-feature 'go-jira
      (let (searched)
        (cl-letf (((symbol-function 'go-jira--ticket-arg-or-ticket-at-point)
                   (lambda (arg) (or arg "SAC-42")))
                  ((symbol-function 'github-topics-find-prs)
                   (lambda (tkt) (setq searched tkt))))
          (go-jira-find-pull-requests-on-github)
          (expect searched :to-equal "SAC-42")))))

  (it "prompts when no ticket is resolvable"
    (with-fake-feature 'go-jira
      (let (searched)
        (cl-letf (((symbol-function 'go-jira--ticket-arg-or-ticket-at-point)
                   (lambda (_) nil))
                  ((symbol-function 'read-string) (lambda (&rest _) "SAC-7"))
                  ((symbol-function 'github-topics-find-prs)
                   (lambda (tkt) (setq searched tkt))))
          (go-jira-find-pull-requests-on-github)
          (expect searched :to-equal "SAC-7"))))))

(describe "go-jira-search-slack-threads"
  (it "searches Slack for the ticket at point"
    (with-fake-feature 'go-jira
      (let (searched)
        (cl-letf (((symbol-function 'go-jira--ticket-arg-or-ticket-at-point)
                   (lambda (arg) (or arg "SAC-42")))
                  ((symbol-function 'slacko-search)
                   (lambda (tkt &optional _host) (setq searched tkt))))
          (go-jira-search-slack-threads)
          (expect searched :to-equal "SAC-42")))))

  (it "prompts when no ticket is resolvable"
    (with-fake-feature 'go-jira
      (let (searched)
        (cl-letf (((symbol-function 'go-jira--ticket-arg-or-ticket-at-point)
                   (lambda (_) nil))
                  ((symbol-function 'read-string) (lambda (&rest _) "SAC-7"))
                  ((symbol-function 'slacko-search)
                   (lambda (tkt &optional _host) (setq searched tkt))))
          (go-jira-search-slack-threads)
          (expect searched :to-equal "SAC-7"))))))

(provide 'jira-tests)
;;; tests/jira/jira-tests.el ends here
