;;; tests/e2e/ticket-tldr.el --- ticket spec and summary keys -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; The batch suite calls the summary commands in a stand-in parent mode.
;; Only a real boot has gfm-view-mode under the summary, evil's normal state
;; handing m, gr, o and q to its map with gg left working, and embark taking
;; a jira ticket in an ordinary buffer to both actions.

(require 'cl-lib)

(defvar llm-specs-dir)
(defvar ticket-tldr-backend)
(defvar ticket-tldr-model)
(defvar gptel--known-backends)

(defvar ticket-tldr-e2e--requests nil
  "Model of every summary request the fake gptel received, newest first.")

(defconst ticket-tldr-e2e--spec
  "---
ticket: ABC-123
title: Nightly export stops halfway
status: implementing
created: 2026-10-01
---

# ABC-123: Nightly export stops halfway

## Synopsis

The export stops after two hours.

## Log

- 2026-10-02: found the cause.
"
  "The main spec of the fixture ticket.")

(defun ticket-tldr-e2e--result (label got want)
  "A harness result plist for LABEL comparing GOT with WANT."
  (list :label (format "ticket summary: %s" label)
        :ok (equal got want) :got (format "%S" got) :want (format "%S" want)))

(defun ticket-tldr-e2e--text (buffer)
  "The text of BUFFER, without its fontification."
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ticket-tldr-e2e--corpus ()
  "Write the fixture corpus under `e2e-work-dir' and return its root."
  (let ((root (file-name-as-directory (expand-file-name "llm-specs" e2e-work-dir))))
    (dolist (file `(("org/repo/ABC-123.md" . ,ticket-tldr-e2e--spec)
                    ("org/repo/ABC-123-review.md" . "---\ntitle: Review\n---\n")))
      (let ((path (expand-file-name (car file) root)))
        (make-directory (file-name-directory path) t)
        (with-temp-file path (insert (cdr file)))))
    root))

(defun ticket-tldr-e2e--fake-request (_prompt &rest args)
  "Answer a summary request at once, streamed, naming the model it went to.
Takes the place of `gptel-request', whose ARGS it reads the callback from."
  (let ((info (list :buffer (current-buffer) :context (plist-get args :context)
                    :stream t))
        (callback (plist-get args :callback)))
    (push gptel-model ticket-tldr-e2e--requests)
    (funcall callback '(reasoning . "thinking") info)
    (funcall callback "## Brief\n\n" info)
    (funcall callback (format "Written by %s." gptel-model) info)
    (funcall callback t info)))

(defun ticket-tldr-e2e--keys (keys)
  "Press KEYS, a key vector, with a guard against a read that never ends."
  (let ((guard (run-with-timer 10 nil (lambda ()
                                        (when (active-minibuffer-window)
                                          (abort-recursive-edit))))))
    (unwind-protect
        (condition-case err
            (progn (execute-kbd-macro keys) nil)
          ((error quit) err))
      (cancel-timer guard))))

(defun ticket-tldr-e2e--summary-keys (root)
  "Open the fixture's summary and drive its keys, with the corpus at ROOT."
  (let ((spec (expand-file-name "org/repo/ABC-123.md" root))
        results buffer)
    (ticket-tldr "ABC-123")
    (setq buffer (get-buffer "*tldr: ABC-123*"))
    (push (ticket-tldr-e2e--result
           "opens read-only in its mode and normal state, in the selected window"
           (with-current-buffer buffer
             (list major-mode (derived-mode-p 'gfm-view-mode) evil-state buffer-read-only
                   (eq (window-buffer (selected-window)) buffer)))
           '(ticket-tldr-mode gfm-view-mode normal t t))
          results)
    (push (ticket-tldr-e2e--result
           "streams the first summary in and caches it beside the spec"
           (list (ticket-tldr-e2e--text buffer)
                 (file-exists-p (expand-file-name "org/repo/ABC-123.tldr.md" root))
                 ticket-tldr-e2e--requests)
           '("## Brief\n\nWritten by e2e-a." t (e2e-a)))
          results)
    (push (ticket-tldr-e2e--result
           "the header line spells out the keys"
           (with-current-buffer buffer
             (and (string-match-p "m model  gr regenerate  o spec  q quit"
                                  (format-mode-line header-line-format))
                  t))
           t)
          results)
    (let ((err (ticket-tldr-e2e--keys (kbd "g r"))))
      (push (ticket-tldr-e2e--result "gr writes the summary again"
                                     (list err ticket-tldr-e2e--requests)
                                     '(nil (e2e-a e2e-a)))
            results))
    (let ((err (with-current-buffer buffer
                 (goto-char (point-max))
                 (ticket-tldr-e2e--keys (kbd "g g")))))
      (push (ticket-tldr-e2e--result "gg still goes to the first line"
                                     (list err (with-current-buffer buffer
                                                 (line-number-at-pos)))
                                     '(nil 1))
            results))
    (let ((err (ticket-tldr-e2e--keys (vconcat "m" "e2e-b" (kbd "RET")))))
      (push (ticket-tldr-e2e--result
             "m picks another model and writes the summary with it"
             (list err (buffer-local-value 'gptel-model buffer)
                   (ticket-tldr-e2e--text buffer)
                   (plist-get (ticket-tldr--read-cache spec) :model))
             '(nil e2e-b "## Brief\n\nWritten by e2e-b." e2e-b))
            results))
    (kill-buffer buffer)
    (ticket-tldr "ABC-123")
    (setq buffer (get-buffer "*tldr: ABC-123*"))
    (push (ticket-tldr-e2e--result
           "reopened with the spec unchanged, it reads the cache and asks nothing"
           (list (length ticket-tldr-e2e--requests)
                 (ticket-tldr-e2e--text buffer))
           '(3 "## Brief\n\nWritten by e2e-b."))
          results)
    (let ((err (ticket-tldr-e2e--keys (kbd "o RET"))))
      (push (ticket-tldr-e2e--result
             "o RET visits the main spec of the two"
             (list err (and (buffer-file-name (window-buffer (selected-window)))
                            (file-equal-p (buffer-file-name (window-buffer (selected-window)))
                                          spec)))
             '(nil t))
            results))
    (pop-to-buffer buffer)
    (let ((err (ticket-tldr-e2e--keys (kbd "q"))))
      (push (ticket-tldr-e2e--result
             "q buries the summary and keeps it"
             (list err (eq (window-buffer (selected-window)) buffer)
                   (buffer-live-p buffer))
             '(nil nil t))
            results))
    (kill-buffer buffer)
    (when-let* ((visited (find-buffer-visiting spec)))
      (kill-buffer visited))
    (nreverse results)))

(defun ticket-tldr-e2e--embark (root)
  "Act on a jira ticket with embark's j t and j o, the corpus at ROOT.
The ticket has two specs, so j o reads twice: the ticket, which embark
answers, then the file, which RET picks."
  (let* ((spec (expand-file-name "org/repo/ABC-123.md" root))
         (act (lambda (keys)
                (e2e-act-case
                 (list :label (format "ticket summary: embark %s on a jira ticket" keys)
                       :ext "txt"
                       :text "see ABC-123 soon"
                       :search "ABC-12"
                       :type 'jira-ticket
                       :keys keys
                       :want "see ABC-123 soon"))))
         results)
    (push (funcall act "j t") results)
    (let ((buffer (get-buffer "*tldr: ABC-123*")))
      (push (ticket-tldr-e2e--result
             "embark j t opened the ticket's summary"
             (and buffer (buffer-local-value 'major-mode buffer))
             'ticket-tldr-mode)
            results)
      (when buffer (kill-buffer buffer)))
    (push (funcall act "j o RET") results)
    (let ((visited (find-buffer-visiting spec)))
      (push (ticket-tldr-e2e--result "embark j o RET visited the main spec"
                                     (and visited t) t)
            results)
      (when visited (kill-buffer visited)))
    (nreverse results)))

(defun ticket-tldr-e2e ()
  "Drive the ticket summary buffer and the jira ticket actions."
  (require 'gptel)
  (let ((fn (symbol-function 'ticket-tldr)))
    (when (autoloadp fn)
      (autoload-do-load fn 'ticket-tldr)))
  (let* ((root (ticket-tldr-e2e--corpus))
         (llm-specs-dir root)
         (ticket-tldr-backend "E2E")
         (ticket-tldr-model 'e2e-a)
         (gptel--known-backends
          (list (cons "E2E" (gptel--make-backend :name "E2E" :models '(e2e-a e2e-b)))))
         (ticket-tldr-e2e--requests nil))
    (unwind-protect
        (cl-letf (((symbol-function 'gptel-request) #'ticket-tldr-e2e--fake-request))
          (append
           (ticket-tldr-e2e--summary-keys root)
           (if (memq 'jira active-modules)
               (progn
                 (require 'go-jira)
                 (require 'embark)
                 (ticket-tldr-e2e--embark root))
             (list (list :label "ticket summary: embark actions SKIPPED, no jira module here"
                         :ok t :got "skip" :want "skip")))))
      (discard-input)
      (when-let* ((buffer (get-buffer "*tldr: ABC-123*")))
        (kill-buffer buffer)))))

(add-to-list 'e2e-scenarios #'ticket-tldr-e2e)
