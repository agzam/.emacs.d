;;; tests/ai/ticket-tldr-tests.el --- ai/autoload/ticket-tldr.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

;; markdown-mode and gptel are absent from the batch tier.  The stand-in
;; parent turns read-only on like the real one, so the streaming specs prove
;; the inserts get past it; every spec that reaches a request stubs gptel.
(unless (fboundp 'gfm-view-mode)
  (define-derived-mode gfm-view-mode text-mode "GFM-View"
    (read-only-mode 1)))

;; bodiless: bound at all, gptel-backend changes what the gptel specs see
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel--known-backends)
(defvar gptel-use-tools)
(defvar gptel-use-context)

;; The same stub as the other gptel suites, whichever loads first.  Its
;; accessors inline into the module as it loads, so no spy reaches them.
(unless (fboundp 'gptel--make-backend)
  (cl-defstruct (gptel-backend (:constructor gptel--make-backend))
    name host header protocol stream endpoint key models url request-params curl-args))

(load-module-file "modules/ai/autoload/ticket-tldr.el")

(defconst ticket-tldr-tests--spec
  "---
ticket: ABC-123
title: Nightly export stops halfway
status: implementing
created: 2026-10-01
updated: 2026-10-03
---

# ABC-123: Nightly export stops halfway

## Synopsis

The export stops after two hours.

## Root cause

The server gives up on big requests.

## Dead ends

- Raising the timeout did nothing.

## Log

- 2026-10-01: created.
- 2026-10-02: found the cause.
  - one nested detail
- 2026-10-05: shipped the change.

## Full context

### Raw data

lots of noise
"
  "A main spec laid out like the corpus template.")

(defun ticket-tldr-tests--write (root file text)
  "Write TEXT to FILE under ROOT, creating its directories."
  (let ((path (expand-file-name file root)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path (insert text))
    path))

(defun ticket-tldr-tests--corpus ()
  "A fixture corpus and prompt in a fresh temp dir, as a directory name."
  (let ((root (file-name-as-directory (make-temp-file "ticket-tldr-corpus" t))))
    (ticket-tldr-tests--write root "org/repo/ABC-123.md" ticket-tldr-tests--spec)
    (ticket-tldr-tests--write root "org/repo/ABC-123-review.md" "---\ntitle: Review\n---\n")
    (ticket-tldr-tests--write root "org/data/ABC-123-notes.md" "notes\n")
    (ticket-tldr-tests--write root "org/.emacs.d/ABC-123-proof.md" "proof\n")
    (ticket-tldr-tests--write root "org/repo/ABC-1234.md" "another ticket\n")
    (ticket-tldr-tests--write root ".sync/Archive/org/repo/ABC-123.md" "archived\n")
    (ticket-tldr-tests--write root ".git/ABC-123.md" "git internals\n")
    (ticket-tldr-tests--write root "prompt.md" "Summarize.\n")
    root))

(defun ticket-tldr-tests--edit (file from to)
  "Replace FROM with TO in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (search-forward from)
    (replace-match to t t)
    (write-region nil nil file nil 'silent)))

(defun ticket-tldr-tests--answer (call &rest responses)
  "Feed RESPONSES, as gptel would stream them, to the callback of CALL."
  (let ((args (nth 1 call)))
    (dolist (response responses)
      (funcall (plist-get args :callback) response
               (list :buffer (nth 2 call) :context (plist-get args :context)
                     :stream t :status "HTTP/2 500")))))

(defmacro ticket-tldr-tests--with-corpus (&rest body)
  "Run BODY with gptel faked and the summary settings on the corpus at `root'."
  (declare (indent 0))
  `(with-fake-feature 'gptel
     (let ((llm-specs-dir root)
           (ticket-tldr-prompt-file (expand-file-name "prompt.md" root))
           (ticket-tldr-backend "Backend A")
           (ticket-tldr-model 'model-a))
       ,@body)))

(describe "ticket-spec-files"
  :var (root)
  (before-each (setq root (ticket-tldr-tests--corpus)))
  (after-each (delete-directory root t))

  (it "lists the main spec first, then every companion under the root"
    (make-symbolic-link (expand-file-name "org/repo/ABC-123-review.md" root)
                        (expand-file-name "org/repo/ABC-123-link.md" root))
    (ticket-tldr-tests--write root "org/repo/ABC-123.tldr.md" "cache\n")
    ;; skipped: a longer id, the summary cache, a symlink, .git, the sync archive
    (let ((llm-specs-dir root))
      (expect (mapcar (lambda (f) (file-relative-name f root))
                      (ticket-spec-files "ABC-123"))
              :to-equal '("org/repo/ABC-123.md"
                          "org/.emacs.d/ABC-123-proof.md"
                          "org/data/ABC-123-notes.md"
                          "org/repo/ABC-123-review.md"))))

  (it "is empty for a ticket without a spec"
    (let ((llm-specs-dir root))
      (expect (ticket-spec-files "XYZ-1") :to-be nil))))

(describe "ticket-spec--read-ticket"
  ;; always a prompt: embark answers the first one its action opens
  (it "offers the ticket under point as the default"
    (with-temp-buffer
      (insert "see ABC-123 soon")
      (goto-char 8)
      (spy-on 'read-string :and-call-fake (lambda (_prompt _initial _history default) default))
      (expect (ticket-spec--read-ticket) :to-equal "ABC-123")
      (expect 'read-string :to-have-been-called-with
              (format-prompt "Ticket" "ABC-123") nil nil "ABC-123")))

  (it "offers no default when point is on no ticket"
    (with-temp-buffer
      (insert "see nothing")
      (goto-char 2)
      (spy-on 'read-string :and-return-value "XYZ-9")
      (expect (ticket-spec--read-ticket) :to-equal "XYZ-9")
      (expect 'read-string :to-have-been-called-with "Ticket: " nil nil nil))))

(describe "ticket-spec-open"
  :var (root)
  (before-each
    (setq root (ticket-tldr-tests--corpus))
    (spy-on 'find-file))
  (after-each (delete-directory root t))

  (it "visits the only spec of a ticket without asking"
    (spy-on 'completing-read)
    (let ((llm-specs-dir root))
      (ticket-spec-open "ABC-1234"))
    (expect 'completing-read :not :to-have-been-called)
    (expect 'find-file :to-have-been-called-with
            (expand-file-name "org/repo/ABC-1234.md" root)))

  (it "offers several specs main first, in their own order, and visits the pick"
    (let (names metadata)
      (spy-on 'completing-read :and-call-fake
              (lambda (_prompt table &rest _)
                (setq names (all-completions "" table)
                      metadata (funcall table "" nil 'metadata))
                "org/data/ABC-123-notes.md"))
      (let ((llm-specs-dir root))
        (ticket-spec-open "ABC-123"))
      (expect (car names) :to-equal "org/repo/ABC-123.md")
      (expect (length names) :to-be 4)
      (expect metadata :to-equal '(metadata (display-sort-function . identity)))
      (expect 'find-file :to-have-been-called-with
              (expand-file-name "org/data/ABC-123-notes.md" root))))

  (it "says so when the ticket has no spec"
    (let ((llm-specs-dir root))
      (expect (ticket-spec-open "XYZ-1") :to-throw 'user-error))))

(describe "ticket-tldr--read-spec"
  :var (root spec)
  (before-each
    (setq root (ticket-tldr-tests--corpus)
          spec (expand-file-name "org/repo/ABC-123.md" root)))
  (after-each (delete-directory root t))

  (it "reads the status and the newest frontmatter or log date"
    (let ((facts (ticket-tldr--read-spec spec)))
      (expect (plist-get facts :status) :to-equal "implementing")
      (expect (plist-get facts :touched) :to-equal "2026-10-05")))

  (it "sends the frontmatter and sections, never dead ends or full context"
    (let ((input (plist-get (ticket-tldr--read-spec spec) :input)))
      (expect input :to-match "\\`---\nticket: ABC-123\n")
      (expect input :to-match "## Synopsis\n\nThe export stops")
      (expect input :to-match "## Root cause")
      (expect input :not :to-match "Raising the timeout")
      (expect input :not :to-match "lots of noise")))

  (it "sends only the newest log entries, each whole"
    (let* ((ticket-tldr-log-entries 2)
           (input (plist-get (ticket-tldr--read-spec spec) :input)))
      (expect input :to-match
              "- 2026-10-02: found the cause.\n  - one nested detail\n- 2026-10-05: shipped")
      (expect input :not :to-match "2026-10-01: created")))

  (it "reads a changelog as the log"
    (ticket-tldr-tests--edit spec "## Log" "## Changelog")
    (let* ((ticket-tldr-log-entries 1)
           (input (plist-get (ticket-tldr--read-spec spec) :input)))
      (expect input :to-match "2026-10-05: shipped")
      (expect input :not :to-match "2026-10-02")))

  (it "cuts the sections at the limit and keeps the log entries"
    (let* ((ticket-tldr-input-limit 20)
           (input (plist-get (ticket-tldr--read-spec spec) :input)))
      (expect input :to-match "\\[\\.\\.\\.\\]")
      (expect input :not :to-match "Root cause")
      (expect input :to-match "2026-10-05: shipped")))

  (it "reads a spec without frontmatter"
    (ticket-tldr-tests--write root "org/repo/XYZ-1.md" "# XYZ-1\n\n## Synopsis\n\nshort\n")
    (let ((facts (ticket-tldr--read-spec (expand-file-name "org/repo/XYZ-1.md" root))))
      (expect (plist-get facts :status) :to-be nil)
      (expect (plist-get facts :input) :to-match "\\`# XYZ-1\n\n## Synopsis"))))

(describe "ticket-tldr cache"
  :var (root spec)
  (before-each
    (setq root (ticket-tldr-tests--corpus)
          spec (expand-file-name "org/repo/ABC-123.md" root)))
  (after-each (delete-directory root t))

  (it "lives beside the spec and reads back what was written"
    (let ((written (ticket-tldr--write-cache spec "h1" 'model-a "## Title\n\nBody ✓")))
      (expect (file-exists-p (expand-file-name "org/repo/ABC-123.tldr.md" root)) :to-be t)
      (expect (ticket-tldr--read-cache spec) :to-equal written)
      (expect (plist-get written :date) :to-equal (format-time-string "%F"))))

  (it "is nil when missing or malformed"
    (expect (ticket-tldr--read-cache spec) :to-be nil)
    (ticket-tldr-tests--write root "org/repo/ABC-123.tldr.md" "no header\n")
    (expect (ticket-tldr--read-cache spec) :to-be nil)))

(describe "ticket-tldr"
  :var (root spec calls aborted)
  (before-each
    (setq root (ticket-tldr-tests--corpus)
          spec (expand-file-name "org/repo/ABC-123.md" root)
          calls nil
          aborted nil)
    (spy-on 'gptel-request :and-call-fake
            (lambda (prompt &rest args)
              (setq calls (append calls (list (list prompt args (current-buffer)))))))
    (spy-on 'gptel-abort :and-call-fake (lambda (buffer) (push buffer aborted)))
    (spy-on 'gptel-get-backend :and-return-value 'backend-a)
    (spy-on 'pop-to-buffer))
  (after-each
    (when-let* ((buffer (get-buffer "*tldr: ABC-123*")))
      ;; the gptel spies may be gone by now; nothing must reach the real abort
      (with-current-buffer buffer (setq ticket-tldr--request nil))
      (kill-buffer buffer))
    (delete-directory root t))

  (it "asks the summary model about the spec when nothing is cached"
    (ticket-tldr-tests--with-corpus (ticket-tldr "ABC-123"))
    (expect (length calls) :to-be 1)
    (pcase-let ((`(,prompt ,args ,buffer) (car calls)))
      (expect prompt :to-match "## Synopsis")
      (expect (plist-get args :system) :to-equal "Summarize.\n")
      (expect (plist-get args :stream) :to-be t)
      (expect buffer :to-be (get-buffer "*tldr: ABC-123*"))
      (with-current-buffer buffer
        (expect major-mode :to-be 'ticket-tldr-mode)
        (expect gptel-backend :to-be 'backend-a)
        (expect gptel-model :to-be 'model-a)
        (expect gptel-use-tools :to-be nil)
        (expect gptel-use-context :to-be nil)))
    (expect 'gptel-get-backend :to-have-been-called-with "Backend A")
    (expect 'pop-to-buffer :to-have-been-called-with (get-buffer "*tldr: ABC-123*")))

  (it "streams the answer into the read-only buffer and caches it when done"
    (ticket-tldr-tests--with-corpus (ticket-tldr "ABC-123"))
    (ticket-tldr-tests--answer (car calls) '(reasoning . "hmm") "## Title\n\n" "Body." t)
    (with-current-buffer "*tldr: ABC-123*"
      (expect buffer-read-only :to-be t)
      (expect (buffer-string) :to-equal "## Title\n\nBody.")
      (expect (substring-no-properties (ticket-tldr--header-line))
              :to-match (format "summary %s, model-a" (format-time-string "%F"))))
    (expect (plist-get (ticket-tldr--read-cache spec) :text)
            :to-equal "## Title\n\nBody."))

  (it "shows the cache without asking while the spec is unchanged"
    (ticket-tldr-tests--with-corpus (ticket-tldr "ABC-123"))
    (ticket-tldr-tests--answer (car calls) "Cached text." t)
    (kill-buffer "*tldr: ABC-123*")
    (ticket-tldr-tests--with-corpus (ticket-tldr "ABC-123"))
    (expect (length calls) :to-be 1)
    (with-current-buffer "*tldr: ABC-123*"
      (expect (buffer-string) :to-equal "Cached text.")))

  (it "keeps the cache when only the full context changed"
    (ticket-tldr-tests--with-corpus (ticket-tldr "ABC-123"))
    (ticket-tldr-tests--answer (car calls) "Cached text." t)
    (ticket-tldr-tests--edit spec "lots of noise" "other noise")
    (ticket-tldr-tests--with-corpus (ticket-tldr "ABC-123"))
    (expect (length calls) :to-be 1))

  (it "asks again when the prompt changed"
    (ticket-tldr-tests--with-corpus (ticket-tldr "ABC-123"))
    (ticket-tldr-tests--answer (car calls) "Cached text." t)
    (ticket-tldr-tests--write root "prompt.md" "Summarize briefly.\n")
    (ticket-tldr-tests--with-corpus (ticket-tldr "ABC-123"))
    (expect (length calls) :to-be 2))

  (it "shows the old summary until the new one streams in after a spec change"
    (ticket-tldr-tests--with-corpus (ticket-tldr "ABC-123"))
    (ticket-tldr-tests--answer (car calls) "Old text." t)
    (ticket-tldr-tests--edit spec "stops after two hours" "stops after one hour")
    (ticket-tldr-tests--with-corpus (ticket-tldr "ABC-123"))
    (expect (length calls) :to-be 2)
    (with-current-buffer "*tldr: ABC-123*"
      (expect (buffer-string) :to-equal "Old text.")
      (expect (substring-no-properties (ticket-tldr--header-line))
              :to-match "spec changed  model-a waiting")
      (ticket-tldr-tests--answer (cadr calls) "New")
      (expect (buffer-string) :to-equal "New")
      (expect (substring-no-properties (ticket-tldr--header-line))
              :not :to-match "spec changed")))

  (it "does not ask twice while a request is in flight"
    (ticket-tldr-tests--with-corpus
      (ticket-tldr "ABC-123")
      (ticket-tldr "ABC-123"))
    (expect (length calls) :to-be 1))

  (it "regenerates on demand, dropping the late chunks of the request it stops"
    (ticket-tldr-tests--with-corpus (ticket-tldr "ABC-123"))
    (ticket-tldr-tests--answer (car calls) "First." t)
    (with-current-buffer "*tldr: ABC-123*"
      (ticket-tldr-tests--with-corpus (ticket-tldr-regenerate))
      (expect (length calls) :to-be 2)
      (ticket-tldr-tests--with-corpus (ticket-tldr-regenerate))
      (expect aborted :to-equal (list (current-buffer)))
      (ticket-tldr-tests--answer (nth 1 calls) "Late chunk")
      (expect (buffer-string) :to-equal "First.")
      (ticket-tldr-tests--answer (nth 2 calls) "Second." t)
      (expect (buffer-string) :to-equal "Second.")))

  (it "keeps the old summary and says why when a request fails"
    (ticket-tldr-tests--with-corpus (ticket-tldr "ABC-123"))
    (ticket-tldr-tests--answer (car calls) "Old text." t)
    (with-current-buffer "*tldr: ABC-123*"
      (ticket-tldr-tests--with-corpus (ticket-tldr-regenerate))
      (ticket-tldr-tests--answer (nth 1 calls) nil)
      (expect (buffer-string) :to-equal "Old text.")
      (expect (substring-no-properties (ticket-tldr--header-line))
              :to-match "failed: HTTP/2 500"))
    (expect (plist-get (ticket-tldr--read-cache spec) :text) :to-equal "Old text."))

  (it "caches nothing from an empty answer"
    (ticket-tldr-tests--with-corpus (ticket-tldr "ABC-123"))
    (ticket-tldr-tests--answer (car calls) t)
    (expect (ticket-tldr--read-cache spec) :to-be nil)
    (with-current-buffer "*tldr: ABC-123*"
      (expect (substring-no-properties (ticket-tldr--header-line))
              :to-match "empty answer")))

  (it "caches an answer that came in one piece"
    (ticket-tldr-tests--with-corpus (ticket-tldr "ABC-123"))
    (pcase-let ((`(,_ ,args ,buffer) (car calls)))
      (funcall (plist-get args :callback) "Whole."
               (list :buffer buffer :context (plist-get args :context))))
    (expect (plist-get (ticket-tldr--read-cache spec) :text) :to-equal "Whole."))

  (it "shows the model thinking until text arrives"
    (ticket-tldr-tests--with-corpus (ticket-tldr "ABC-123"))
    (ticket-tldr-tests--answer (car calls) '(reasoning . "hmm"))
    (with-current-buffer "*tldr: ABC-123*"
      (expect (substring-no-properties (ticket-tldr--header-line))
              :to-match "model-a thinking")))

  (it "stops its request when the buffer is killed"
    (ticket-tldr-tests--with-corpus (ticket-tldr "ABC-123"))
    (let ((buffer (get-buffer "*tldr: ABC-123*")))
      (kill-buffer buffer)
      (expect aborted :to-equal (list buffer))))

  (it "says so when the ticket has no spec"
    (ticket-tldr-tests--with-corpus
      (expect (ticket-tldr "XYZ-1") :to-throw 'user-error))
    (expect calls :to-be nil))

  (it "names the ticket, its status, spec date, summary and keys in the header"
    (ticket-tldr-tests--with-corpus (ticket-tldr "ABC-123"))
    (ticket-tldr-tests--answer (car calls) "Text." t)
    (with-current-buffer "*tldr: ABC-123*"
      (expect (substring-no-properties (ticket-tldr--header-line))
              :to-equal (format (concat " ABC-123  implementing  spec 2026-10-05  "
                                        "summary %s, model-a  m model  gr regenerate  "
                                        "o spec  q quit")
                                (format-time-string "%F")))))

  (it "picks another model and writes the summary again with it"
    (ticket-tldr-tests--with-corpus (ticket-tldr "ABC-123"))
    (ticket-tldr-tests--answer (car calls) "Text." t)
    (let ((a (gptel--make-backend :name "Backend A" :models '(model-a model-b)))
          (b (gptel--make-backend :name "Backend B" :models '(model-c)))
          names)
      (spy-on 'completing-read :and-call-fake
              (lambda (_prompt choices &rest _)
                (setq names (mapcar #'car choices))
                "Backend B:model-c"))
      (with-current-buffer "*tldr: ABC-123*"
        (let ((gptel--known-backends (list (cons "Backend A" a) (cons "Backend B" b))))
          (ticket-tldr-tests--with-corpus (ticket-tldr-pick-model)))
        (expect names :to-equal '("Backend A:model-a" "Backend A:model-b" "Backend B:model-c"))
        (expect gptel-backend :to-be b)
        (expect gptel-model :to-be 'model-c)
        (expect (length calls) :to-be 2)
        (ticket-tldr-tests--answer (nth 1 calls) "By c." t)))
    (expect (plist-get (ticket-tldr--read-cache spec) :model) :to-be 'model-c))

  (it "visits the spec of the ticket it summarizes"
    (spy-on 'ticket-spec-open)
    (ticket-tldr-tests--with-corpus (ticket-tldr "ABC-123"))
    (with-current-buffer "*tldr: ABC-123*"
      (ticket-tldr-open-spec))
    (expect 'ticket-spec-open :to-have-been-called-with "ABC-123")))

(provide 'ticket-tldr-tests)
;;; tests/ai/ticket-tldr-tests.el ends here
