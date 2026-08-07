;;; tests/lisp/doom-compat-tests.el --- doom-compat layer specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(describe "the sandbox (helper self-check)"
  (it "keeps every doom dir inside the test sandbox"
    (dolist (dir (list doom-local-dir doom-data-dir doom-state-dir doom-cache-dir))
      (expect (file-in-directory-p dir test-sandbox-dir) :to-be-truthy))))

(describe "file quarantine"
  (it "keeps state files under doom-local-dir (the git-ignored .local/)"
    (dolist (var '(savehist-file save-place-file recentf-save-file
                   bookmark-default-file project-list-file
                   transient-history-file auto-save-list-file-prefix
                   url-cache-directory eshell-directory-name
                   request-storage-directory))
      (expect (file-in-directory-p (symbol-value var) doom-local-dir)
              :to-be-truthy)))
  (it "points state files at the quarantine dirs"
    (expect (file-in-directory-p savehist-file doom-state-dir) :to-be-truthy)
    (expect (file-in-directory-p save-place-file doom-state-dir) :to-be-truthy)
    (expect (file-in-directory-p tramp-persistency-file-name doom-cache-dir)
            :to-be-truthy)
    (expect (file-in-directory-p url-configuration-directory doom-data-dir)
            :to-be-truthy))
  (it "redirects package build artifacts (request jar, tree-sitter grammars)"
    ;; request derives its curl cookie jar from request-storage-directory;
    ;; treesit installs .dylib grammars into treesit-extra-load-path's first
    ;; writable entry.  Both defaulted inside user-emacs-directory.
    (expect (file-in-directory-p request-storage-directory doom-cache-dir)
            :to-be-truthy)
    (expect (car treesit-extra-load-path) :to-be-truthy)
    (expect (file-in-directory-p (car treesit-extra-load-path) doom-cache-dir)
            :to-be-truthy)
    ;; raw treesit-install-language-grammar ignores treesit-extra-load-path;
    ;; its non-interactive default out-dir is the car of this history list.
    (expect (car treesit--install-language-grammar-out-dir-history)
            :to-equal (car treesit-extra-load-path))))

(describe "switch-frame hook machinery"
  (it "defines the debounced trigger and its hook"
    (expect (boundp 'doom-switch-frame-hook) :to-be-truthy)
    (expect (numberp doom-switch-frame-hook-debounce-delay) :to-be-truthy)
    (expect (fboundp 'doom-run-switch-frame-hooks-fn) :to-be-truthy)
    (expect (fboundp 'doom--run-switch-frame-hooks-fn) :to-be-truthy)))

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

(defvar probe-hook-log nil)
(defvar probe-hook-a nil)
(defvar probe-hook-b nil)
(defvar probe-transient-hook nil)
(defvar probe-transient-count 0)
(defvar probe-setq-hook nil)
(defvar probe-local-var 'global)

(describe "add-hook! / remove-hook!"
  (before-each
    (setq probe-hook-log nil
          probe-hook-a nil
          probe-hook-b nil))
  (it "adds a named function to multiple hooks"
    (add-hook! '(probe-hook-a probe-hook-b)
      (defun probe-hook-fn ()
        (push 'ran probe-hook-log)))
    (expect (memq 'probe-hook-fn probe-hook-a) :to-be-truthy)
    (expect (memq 'probe-hook-fn probe-hook-b) :to-be-truthy)
    (run-hooks 'probe-hook-a 'probe-hook-b)
    (expect probe-hook-log :to-equal '(ran ran)))
  (it "appends with :append"
    (add-hook 'probe-hook-a #'ignore)
    (add-hook! 'probe-hook-a :append
      (defun probe-hook-last-fn () nil))
    (expect (car (last probe-hook-a)) :to-be 'probe-hook-last-fn))
  (it "remove-hook! removes what add-hook! added"
    (add-hook! 'probe-hook-a #'probe-hook-fn)
    (remove-hook! 'probe-hook-a #'probe-hook-fn)
    (expect (memq 'probe-hook-fn probe-hook-a) :to-be nil)))

(describe "add-transient-hook!"
  (it "runs once, then removes itself"
    (setq probe-transient-count 0
          probe-transient-hook nil)
    (add-transient-hook! 'probe-transient-hook
      (cl-incf probe-transient-count))
    (run-hooks 'probe-transient-hook)
    (expect probe-transient-count :to-equal 1)
    (expect probe-transient-hook :to-equal nil)
    (run-hooks 'probe-transient-hook)
    (expect probe-transient-count :to-equal 1)))

(describe "setq-hook!"
  (it "sets buffer-local values when the hook fires, leaving globals alone"
    (setq-hook! 'probe-setq-hook probe-local-var 'hooked)
    (with-temp-buffer
      (run-hooks 'probe-setq-hook)
      (expect probe-local-var :to-be 'hooked)
      (expect (local-variable-p 'probe-local-var) :to-be-truthy))
    (expect (default-value 'probe-local-var) :to-be 'global))
  (it "unsetq-hook! detaches the setter"
    (setq-hook! 'probe-setq-hook probe-local-var 'hooked)
    (unsetq-hook! 'probe-setq-hook probe-local-var)
    (with-temp-buffer
      (run-hooks 'probe-setq-hook)
      (expect (local-variable-p 'probe-local-var) :to-be nil))))

(describe "defadvice! / undefadvice!"
  (it "defines the advice function and installs it"
    (defun probe-advice-target () 'original)
    (defadvice! probe-advice-a (&rest _)
      :override #'probe-advice-target
      'advised)
    (expect (probe-advice-target) :to-be 'advised)
    (undefadvice! probe-advice-a (&rest _)
      :override #'probe-advice-target)
    (expect (probe-advice-target) :to-be 'original)))

(describe "doom-glob"
  (it "expands globs and returns nil for no matches"
    (let ((dir (expand-file-name "glob-probe/" test-sandbox-dir)))
      (make-directory dir t)
      (dolist (f '("a.el" "b.el" "c.txt"))
        (with-temp-file (expand-file-name f dir)))
      (expect (length (doom-glob dir "*.el")) :to-equal 2)
      (expect (doom-glob dir "*.nothing") :to-be nil)))
  (it "returns only directories for trailing-slash globs"
    (let ((dir (expand-file-name "glob-dirs-probe/" test-sandbox-dir)))
      (make-directory (expand-file-name "sub" dir) t)
      (with-temp-file (expand-file-name "file.el" dir))
      (expect (mapcar #'file-name-nondirectory (doom-glob dir "*/"))
              :to-equal '("sub")))))

(describe "letf!"
  (it "temporarily shadows a function, restoring it afterwards"
    (defun letf-probe () 'original)
    (expect (letf! ((defun letf-probe () 'shadowed))
              (letf-probe))
            :to-be 'shadowed)
    (expect (letf-probe) :to-be 'original)))

(describe "quiet!"
  (it "neuters message but passes the body value through"
    (expect (quiet! (message "must not escape") 'passed-through)
            :to-be 'passed-through)
    (expect (quiet! (message "returns nil under quiet")) :to-be nil)))
