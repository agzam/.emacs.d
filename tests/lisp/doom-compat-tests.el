;;; tests/lisp/doom-compat-tests.el --- doom-compat layer specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(describe "the XDG sandbox (helper self-check)"
  (it "keeps every doom dir inside the test sandbox"
    (dolist (dir (list doom-local-dir doom-data-dir doom-state-dir doom-cache-dir))
      (expect (file-in-directory-p dir test-sandbox-dir) :to-be-truthy))))

(describe "file quarantine"
  (it "redirects state files outside user-emacs-directory"
    (dolist (var '(savehist-file recentf-save-file bookmark-default-file
                   project-list-file transient-history-file
                   auto-save-list-file-prefix url-cache-directory
                   eshell-directory-name))
      (expect (file-in-directory-p (symbol-value var) user-emacs-directory)
              :to-be nil)))
  (it "points state files at the quarantine dirs"
    (expect (file-in-directory-p savehist-file doom-state-dir) :to-be-truthy)
    (expect (file-in-directory-p tramp-persistency-file-name doom-cache-dir)
            :to-be-truthy)
    (expect (file-in-directory-p url-configuration-directory doom-data-dir)
            :to-be-truthy)))

(describe "modulep!"
  (it "matches an enabled module"
    (let ((doom-modules-enabled '((:custom general))))
      (expect (modulep! :custom general) :to-be-truthy)))
  (it "is nil for absent modules"
    (let ((doom-modules-enabled '((:custom general))))
      (expect (modulep! :custom completion) :to-be nil)))
  (it "checks flags against the registry entry"
    (let ((doom-modules-enabled '((:editor evil +everywhere))))
      (expect (modulep! :editor evil +everywhere) :to-be-truthy)
      (expect (modulep! :editor evil +nonexistent) :to-be nil))))

(describe "after!"
  (it "defers the body until the feature is provided"
    (put 'after-defer-probe 'ran nil)
    (after! after-defer-probe-feature
      (put 'after-defer-probe 'ran t))
    (expect (get 'after-defer-probe 'ran) :to-be nil)
    (provide 'after-defer-probe-feature)
    (expect (get 'after-defer-probe 'ran) :to-be-truthy))
  (it "runs the body immediately for already-loaded features"
    (put 'after-now-probe 'ran nil)
    (after! cl-lib
      (put 'after-now-probe 'ran t))
    (expect (get 'after-now-probe 'ran) :to-be-truthy)))
