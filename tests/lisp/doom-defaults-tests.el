;;; tests/lisp/doom-defaults-tests.el --- doom-defaults specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'doom-defaults)

(describe "backup & autosave quarantine"
  (it "keeps backups inside the cache dir"
    (expect (file-in-directory-p (cdr (assoc "." backup-directory-alist))
                                 doom-cache-dir)
            :to-be-truthy)
    (expect tramp-backup-directory-alist :to-be backup-directory-alist))
  (it "hashes auto-save names into the quarantined prefix"
    (dolist (transform auto-save-file-name-transforms)
      (expect (string-prefix-p auto-save-list-file-prefix (nth 1 transform))
              :to-be-truthy)
      (expect (nth 2 transform) :to-be 'sha1))
    (expect auto-save-default :to-be-truthy)
    (expect make-backup-files :to-be nil)
    (expect create-lockfiles :to-be nil)))

(describe "quiet-UX defaults"
  (it "silences the bell and shortens prompts"
    (expect ring-bell-function :to-be #'ignore)
    (expect visible-bell :to-be nil)
    (expect use-short-answers :to-be-truthy)
    (expect confirm-nonexistent-file-or-buffer :to-be nil)
    (expect kill-do-not-save-duplicates :to-be-truthy)
    (expect uniquify-buffer-name-style :to-be 'forward))
  (it "unbinds SPC as an accidental yes in y-or-n-p"
    (expect (lookup-key y-or-n-p-map " ") :to-be nil)))

(describe "formatting defaults"
  (it "prefers spaces, final newlines and single sentence spacing"
    (expect (default-value 'indent-tabs-mode) :to-be nil)
    (expect (default-value 'tab-always-indent) :to-be nil)
    (expect (default-value 'require-final-newline) :to-be-truthy)
    (expect sentence-end-double-space :to-be nil))
  (it "wraps at word boundaries but truncates by default"
    (expect (default-value 'word-wrap) :to-be-truthy)
    (expect (default-value 'truncate-lines) :to-be-truthy)
    (expect (memq #'visual-line-mode text-mode-hook) :to-be-truthy)))

(describe "doom/escape"
  (it "remaps keyboard-quit globally"
    (expect (lookup-key global-map [remap keyboard-quit]) :to-be #'doom/escape))
  (it "stops at the first escape hook that returns non-nil"
    (let* ((ran 0)
           (doom-escape-hook (list (lambda () (cl-incf ran) t)
                                   (lambda () (cl-incf ran) t))))
      (doom/escape)
      (expect ran :to-equal 1)))
  (it "falls back to keyboard-quit when no hook handles it"
    ;; not :to-throw - buttercup only catches `error'-derived conditions,
    ;; a raw quit would abort the whole batch run
    (let ((doom-escape-hook nil))
      (expect (condition-case nil
                  (progn (doom/escape) nil)
                (quit 'quit))
              :to-be 'quit)))
  (it "does not abort macro recording"
    (let ((doom-escape-hook nil)
          (defining-kbd-macro t))
      (expect (doom/escape) :to-be nil))))

(describe "file-handling hooks"
  (it "registers the missing-directory creator"
    (expect (memq 'doom-create-missing-directories-h
                  find-file-not-found-functions)
            :to-be-truthy))
  (it "resolves symlinks and truenames"
    (expect find-file-visit-truename :to-be-truthy)
    (expect vc-follow-symlinks :to-be-truthy)))

(defvar text-mode-local-vars-hook)

(describe "MODE-local-vars-hook"
  (it "fires the mode's local-vars hook once per buffer"
    (let ((buf (get-buffer-create "doom-defaults-local-vars-probe"))
          (ran 0))
      (unwind-protect
          (with-current-buffer buf
            (let ((text-mode-local-vars-hook (list (lambda () (cl-incf ran)))))
              (text-mode)
              (doom-run-local-var-hooks-h)
              (expect ran :to-equal 1)
              ;; second run inhibited by the buffer-local flag
              (doom-run-local-var-hooks-h)
              (expect ran :to-equal 1)))
        (kill-buffer buf))))
  (it "skips temporary (space-prefixed) buffers"
    (with-temp-buffer
      (let ((ran 0))
        (let ((text-mode-local-vars-hook (list (lambda () (cl-incf ran)))))
          (text-mode)
          (doom-run-local-var-hooks-h)
          (expect ran :to-equal 0))))))

(describe "C-i / C-m translation hack"
  (it "installs key-translation dispatchers for the GUI-only events"
    (expect (functionp (lookup-key key-translation-map [?\C-i])) :to-be-truthy)
    (expect (functionp (lookup-key key-translation-map [?\C-m])) :to-be-truthy)))

(describe "built-in package staging"
  (it "hooks the built-ins onto the doom lifecycle"
    (expect (memq 'recentf-mode doom-first-file-hook) :to-be-truthy)
    (expect (memq 'doom-auto-revert-mode doom-first-file-hook) :to-be-truthy)
    (expect (memq 'global-so-long-mode doom-first-file-hook) :to-be-truthy)
    (expect (memq 'save-place-mode doom-first-input-hook) :to-be-truthy)
    (expect (memq 'show-paren-mode doom-first-buffer-hook) :to-be-truthy))
  (it "keeps winner-mode and global-hl-line-mode out (deliberate deviations)"
    (dolist (hook (list doom-first-input-hook doom-first-buffer-hook
                        doom-first-file-hook))
      (expect (memq 'winner-mode hook) :to-be nil)
      (expect (memq 'global-hl-line-mode hook) :to-be nil))))

(describe "doom-auto-revert-mode"
  (it "wires and unwires the lazy revert hooks"
    (require 'autorevert)
    (unwind-protect
        (progn
          (doom-auto-revert-mode +1)
          (expect (memq 'doom-auto-revert-buffer-h doom-switch-buffer-hook)
                  :to-be-truthy)
          (expect (memq 'doom-auto-revert-buffer-h doom-switch-window-hook)
                  :to-be-truthy)
          (expect (memq 'doom-auto-revert-buffers-h doom-switch-frame-hook)
                  :to-be-truthy)
          (expect (memq 'doom-auto-revert-buffers-h after-save-hook)
                  :to-be-truthy))
      (doom-auto-revert-mode -1))
    (expect (memq 'doom-auto-revert-buffer-h doom-switch-buffer-hook) :to-be nil)
    (expect (memq 'doom-auto-revert-buffers-h after-save-hook) :to-be nil)))

(describe "doom-so-long-p"
  (before-all (require 'so-long))
  (it "is installed as the so-long predicate"
    (expect so-long-predicate :to-be #'doom-so-long-p))
  (it "flags buffers with overlong lines, leaves short ones alone"
    (let ((buf (get-buffer-create "doom-defaults-so-long-probe.js")))
      (unwind-protect
          (with-current-buffer buf
            (insert (make-string (* so-long-threshold 2) ?x))
            (expect (doom-so-long-p) :to-be-truthy)
            (erase-buffer)
            (insert "short line\n")
            (expect (doom-so-long-p) :to-be nil))
        (kill-buffer buf))))
  (it "exempts special buffers"
    (let ((buf (get-buffer-create "*doom-defaults-so-long-probe*")))
      (unwind-protect
          (with-current-buffer buf
            (insert (make-string (* so-long-threshold 2) ?x))
            (expect (doom-so-long-p) :to-be nil))
        (kill-buffer buf)))))

(describe "savehist settings"
  (it "persists kill-ring and registers once savehist loads"
    (require 'savehist)
    (expect (memq 'kill-ring savehist-additional-variables) :to-be-truthy)
    (expect (memq 'register-alist savehist-additional-variables) :to-be-truthy)
    (expect (memq 'doom-savehist-unpropertize-variables-h savehist-save-hook)
            :to-be-truthy)))
