;;; tests/search/consult-omni-tests.el --- search/autoload/consult-omni.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; the file's top-level (require 'consult-omni) must no-op in the batch
;; tier - the transient and the advice fn are package-independent
(provide 'consult-omni)

(load-module-file "modules/search/autoload/consult-omni.el")

(describe "consult-omni-transient layout"
  (it "binds exactly the ported suffix commands"
    (expect (sort (transient-layout-commands
                   (get 'consult-omni-transient 'transient--layout))
                  #'string<)
            :to-equal
            '(consult-hn-transient
              consult-omni-apps
              consult-omni-browser-history
              consult-omni-github
              consult-omni-google
              consult-omni-gptel
              consult-omni-multi
              consult-omni-wikipedia
              consult-omni-youtube
              gptel-log-find
              search-github-with-lang
              slack-visible-capture
              slack-visible-yank
              slacko-search)))

  (it "carries none of the dropped rows"
    ;; elfeed left with its package, notmuch parks until its module,
    ;; search-in-slack was superseded by slacko-search
    (let ((cmds (transient-layout-commands
                 (get 'consult-omni-transient 'transient--layout))))
      (expect (memq 'consult-omni-elfeed cmds) :to-be nil)
      (expect (memq 'consult-omni-notmuch cmds) :to-be nil)
      (expect (memq 'search-in-slack cmds) :to-be nil))))

(describe "consult-omni-no-require-match-a"
  (it "appends :require-match nil when options carry none"
    (expect (consult-omni-no-require-match-a
             '(("Google") 3 (:count 30) :prompt "S: " :sort t))
            :to-equal
            '(("Google") 3 (:count 30) :prompt "S: " :sort t :require-match nil)))

  (it "overrides a source-declared :require-match t"
    (expect (consult-omni-no-require-match-a
             '(("gptel") nil nil :prompt "S: " :require-match t :sort t))
            :to-equal
            '(("gptel") nil nil :prompt "S: " :require-match nil :sort t)))

  (it "keeps empty options well-formed"
    (expect (consult-omni-no-require-match-a '(("Brave") nil nil))
            :to-equal '(("Brave") nil nil :require-match nil)))

  (it "leaves the original arg list unmutated"
    (let ((args (list '("gptel") nil nil :require-match t)))
      (consult-omni-no-require-match-a args)
      (expect args :to-equal '(("gptel") nil nil :require-match t)))))
