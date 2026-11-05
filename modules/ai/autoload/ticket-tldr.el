;;; modules/ai/autoload/ticket-tldr.el -*- lexical-binding: t; -*-

;;; Ticket specs and their plain-language summaries

;; An llm-spec is written for agents.  `ticket-tldr' streams a model's short
;; brief of it into a buffer and caches it next to the spec, keyed on a hash
;; of the prompt and of the spec text the request sent.

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-use-tools)
(defvar gptel-use-context)
(defvar gptel--known-backends)
(declare-function gptel-request "gptel-request")
(declare-function gptel-abort "gptel-request")
(declare-function gptel-get-backend "gptel-request")
(declare-function gfm-view-mode "markdown-mode")

(defvar llm-specs-dir "~/GitHub/agzam/llm-specs/"
  "Root of the investigation specs corpus.")

(defvar ticket-tldr-backend "Claude-OAuth"
  "Name of the gptel backend that writes ticket summaries.")

(defvar ticket-tldr-model 'claude-sonnet-5-5
  "Model that writes ticket summaries.")

(defvar ticket-tldr-prompt-file
  (expand-file-name "../ticket-tldr-prompt.md"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "File holding the system prompt of ticket summaries.")

(defvar ticket-tldr-log-entries 8
  "How many of the newest log entries a summary request sends.")

(defvar ticket-tldr-input-limit 40000
  "Most characters of spec sections a summary request sends.")

;;; Specs

(defun ticket-spec--ticket-at-point ()
  "The ticket id at point, or nil."
  (when-let* ((symbol (thing-at-point 'symbol t))
              ((string-match-p "\\`[A-Z][A-Z0-9_]+-[0-9]+\\'" symbol)))
    symbol))

(defun ticket-spec--read-ticket ()
  "Read a ticket id, the one at point by default.
Embark answers this prompt with its target; a prompt skipped for the
ticket at point would hand that target to the next one instead."
  (let ((at-point (ticket-spec--ticket-at-point)))
    (read-string (format-prompt "Ticket" at-point) nil nil at-point)))

(defun ticket-spec-files (ticket)
  "Spec files of TICKET under `llm-specs-dir', its main spec first."
  (let ((main (concat ticket ".md")))
    (sort (seq-remove
           #'file-symlink-p
           (directory-files-recursively
            (expand-file-name llm-specs-dir)
            (format "\\`%s\\(?:-.+\\)?\\.md\\'" (regexp-quote ticket))
            nil
            ;; .sync holds the sync tool's archived copies of the specs
            (lambda (dir)
              (not (member (file-name-nondirectory dir) '(".git" ".sync"))))))
          (lambda (a b)
            (let ((a-main (equal (file-name-nondirectory a) main))
                  (b-main (equal (file-name-nondirectory b) main)))
              (if (eq a-main b-main) (string< a b) a-main))))))

(defun ticket-spec--choose (ticket files)
  "Read one of TICKET's spec FILES, offered in their own order."
  (let* ((root (expand-file-name llm-specs-dir))
         (names (mapcar (lambda (file) (file-relative-name file root)) files)))
    (expand-file-name
     (completing-read (format "%s spec: " ticket)
                      (lambda (string predicate action)
                        (if (eq action 'metadata)
                            '(metadata (display-sort-function . identity))
                          (complete-with-action action names string predicate)))
                      nil t)
     root)))

;;;###autoload
(defun ticket-spec-open (ticket)
  "Visit the spec of TICKET, asking which file when it has several."
  (interactive (list (ticket-spec--read-ticket)))
  (let ((files (or (ticket-spec-files ticket)
                   (user-error "No spec for %s" ticket))))
    (find-file (if (cdr files)
                   (ticket-spec--choose ticket files)
                 (car files)))))

;;; What a summary is made from

(defun ticket-tldr--body-start ()
  "Position past the YAML frontmatter of the current buffer, or its start."
  (save-excursion
    (goto-char (point-min))
    (or (and (looking-at "---\n")
             (re-search-forward "^---\n" nil t 2))
        (point-min))))

(defun ticket-tldr--chunks ()
  "Split the current buffer at level-two headings into (HEADING . TEXT).
The text ahead of the first heading comes first, under a nil heading."
  (goto-char (point-min))
  (let ((starts (list (point-min))))
    (while (re-search-forward "^## " nil t)
      (push (match-beginning 0) starts))
    (cl-loop for (beg end) on (nreverse (cons (point-max) starts))
             when (and end (< beg end))
             collect (cons (save-excursion
                             (goto-char beg)
                             (and (looking-at "## +\\(.*?\\) *$")
                                  (match-string-no-properties 1)))
                           (buffer-substring-no-properties beg end)))))

(defun ticket-tldr--log-entries (section)
  "The entries of log SECTION, oldest first."
  (with-temp-buffer
    (insert section)
    (goto-char (point-min))
    (let (entries)
      (while (re-search-forward "^[-*] " nil t)
        (let ((start (match-beginning 0))
              (end (if (re-search-forward "^[-*] \\|^#" nil t)
                       (goto-char (match-beginning 0))
                     (point-max))))
          (push (string-trim-right (buffer-substring-no-properties start end))
                entries)))
      (nreverse entries))))

(defun ticket-tldr--input (body)
  "The text of the current spec buffer that a summary is made from.
BODY is where the sections start, past the frontmatter.  Dead ends, full
context and older log entries stay out."
  (let (kept entries past-context)
    (save-restriction
      (narrow-to-region body (point-max))
      (pcase-dolist (`(,heading . ,text) (ticket-tldr--chunks))
        (let ((name (downcase (or heading ""))))
          (cond ((member name '("log" "changelog"))
                 (setq entries (append entries (ticket-tldr--log-entries text))))
                ((equal name "full context") (setq past-context t))
                ((or past-context (equal name "dead ends")))
                (t (push text kept))))))
    (let ((sections (string-join (nreverse kept))))
      (concat (buffer-substring-no-properties (point-min) body)
              (if (< ticket-tldr-input-limit (length sections))
                  (concat (substring sections 0 ticket-tldr-input-limit) "\n[...]\n")
                sections)
              (when-let* ((recent (last entries ticket-tldr-log-entries)))
                (concat "\n## Log, newest entries\n\n"
                        (string-join recent "\n") "\n"))))))

(defun ticket-tldr--read-spec (spec)
  "Plist of SPEC's :status, :touched date and summary :input.
The touched date is the newest of its frontmatter and log dates."
  (with-temp-buffer
    (insert-file-contents spec)
    (let ((body (ticket-tldr--body-start))
          dates)
      (goto-char (point-min))
      (while (re-search-forward
              (concat "^\\(?:\\(?:created\\|updated\\): *['\"]?\\([0-9-]\\{10\\}\\)"
                      "\\|[-*] +\\([0-9-]\\{10\\}\\):\\)")
              nil t)
        (push (or (match-string-no-properties 1) (match-string-no-properties 2))
              dates))
      (goto-char (point-min))
      (list :status (and (re-search-forward "^status: *['\"]?\\([a-z-]+\\)" body t)
                         (match-string-no-properties 1))
            :touched (car (last (sort dates #'string<)))
            :input (ticket-tldr--input body)))))

;;; Cache

(defun ticket-tldr--prompt ()
  "The system prompt of ticket summaries."
  (with-temp-buffer
    (insert-file-contents ticket-tldr-prompt-file)
    (buffer-string)))

(defun ticket-tldr--cache-file (spec)
  "File caching the summary of SPEC."
  (concat (file-name-sans-extension spec) ".tldr.md"))

(defun ticket-tldr--read-cache (spec)
  "Plist of :hash, :model, :date and :text cached for SPEC, or nil."
  (let ((file (ticket-tldr--cache-file spec)))
    (when (file-readable-p file)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents file))
        (goto-char (point-min))
        (when (looking-at "---\nhash: \\(.+\\)\nmodel: \\(.+\\)\ndate: \\(.+\\)\n---\n")
          (list :hash (match-string-no-properties 1)
                :model (intern (match-string-no-properties 2))
                :date (match-string-no-properties 3)
                :text (string-trim (buffer-substring-no-properties
                                    (match-end 0) (point-max)))))))))

(defun ticket-tldr--write-cache (spec hash model text)
  "Cache TEXT, MODEL's summary of SPEC from input HASH, and return its plist."
  (let ((cache (list :hash hash :model model
                     :date (format-time-string "%F") :text text))
        (coding-system-for-write 'utf-8-unix))
    (with-temp-file (ticket-tldr--cache-file spec)
      (insert (format "---\nhash: %s\nmodel: %s\ndate: %s\n---\n\n%s\n"
                      hash model (plist-get cache :date) text)))
    cache))

;;; Summary buffer

(defvar-local ticket-tldr--ticket nil
  "Ticket the summary in this buffer is about.")

(defvar-local ticket-tldr--spec nil
  "Spec file the summary in this buffer is made from.")

(defvar-local ticket-tldr--facts nil
  "Plist of the spec's :status and :touched date.")

(defvar-local ticket-tldr--input-hash nil
  "Hash that a summary of the spec as it is now would carry.")

(defvar-local ticket-tldr--shown nil
  "Plist of :hash, :model and :date of the summary on display.")

(defvar-local ticket-tldr--request nil
  "Plist of the request answering into this buffer, or nil.")

(defvar-local ticket-tldr--note nil
  "What became of the last request when it failed or stopped.")

(defun ticket-tldr--key (key label)
  "KEY in the key binding face, then LABEL."
  (concat (propertize key 'face 'help-key-binding) " " label))

(defun ticket-tldr--header-line ()
  "Header line of a `ticket-tldr-mode' buffer."
  (let ((request ticket-tldr--request)
        (shown ticket-tldr--shown))
    (concat
     " "
     (string-join
      (delq nil
            (list (propertize (or ticket-tldr--ticket "") 'face 'bold)
                  (plist-get ticket-tldr--facts :status)
                  (when-let* ((date (plist-get ticket-tldr--facts :touched)))
                    (concat "spec " date))
                  (when (and shown (not (equal (plist-get shown :hash)
                                               ticket-tldr--input-hash)))
                    (propertize "spec changed" 'face 'warning))
                  (cond (request (format "%s %s" (plist-get request :model)
                                         (plist-get request :phase)))
                        (ticket-tldr--note)
                        ((plist-get shown :date)
                         (format "summary %s, %s" (plist-get shown :date)
                                 (plist-get shown :model))))
                  (ticket-tldr--key "m" "model")
                  (ticket-tldr--key "gr" "regenerate")
                  (ticket-tldr--key "o" "spec")
                  (ticket-tldr--key "q" "quit")))
      "  "))))

(define-derived-mode ticket-tldr-mode gfm-view-mode "TL;DR"
  "Read a model's plain-language summary of a ticket spec."
  (setq-local header-line-format '(:eval (ticket-tldr--header-line))
              gptel-use-tools nil
              gptel-use-context nil)
  (add-hook 'kill-buffer-hook #'ticket-tldr--abort nil t))

(defun ticket-tldr--show (cache)
  "Display the summary in CACHE, a plist from `ticket-tldr--read-cache'."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (plist-get cache :text))
    (goto-char (point-min)))
  (setq ticket-tldr--shown cache))

(defun ticket-tldr--abort ()
  "Stop the request answering into this buffer."
  (when ticket-tldr--request
    (setq ticket-tldr--request nil)
    (gptel-abort (current-buffer))))

(defun ticket-tldr--insert (request text)
  "Append TEXT of REQUEST, replacing the older summary on its first chunk."
  (let ((inhibit-read-only t))
    (unless (plist-get request :started)
      (erase-buffer)
      (plist-put request :started t)
      (setq ticket-tldr--shown (list :hash (plist-get request :hash)
                                     :model (plist-get request :model))))
    (save-excursion
      (goto-char (point-max))
      (insert text)))
  (plist-put request :phase 'writing))

(defun ticket-tldr--finish (request)
  "Close REQUEST and cache the summary it wrote."
  (setq ticket-tldr--request nil)
  (if (plist-get request :started)
      (setq ticket-tldr--shown
            (ticket-tldr--write-cache
             ticket-tldr--spec (plist-get request :hash) (plist-get request :model)
             (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (setq ticket-tldr--note "empty answer")))

(defun ticket-tldr--callback (response info)
  "Stream RESPONSE into the summary buffer of the request in INFO."
  (let ((request (plist-get info :context))
        (buffer (plist-get info :buffer)))
    (when (and (buffer-live-p buffer)
               (eq request (buffer-local-value 'ticket-tldr--request buffer)))
      (with-current-buffer buffer
        (pcase response
          ((pred stringp)
           (ticket-tldr--insert request response)
           (unless (plist-get info :stream)
             (ticket-tldr--finish request)))
          (`(reasoning . ,_)
           (when (eq (plist-get request :phase) 'waiting)
             (plist-put request :phase 'thinking)))
          ('t (ticket-tldr--finish request))
          ('abort (setq ticket-tldr--request nil
                        ticket-tldr--note "stopped"))
          ('nil (setq ticket-tldr--request nil
                      ticket-tldr--note (format "failed: %s" (plist-get info :status)))))
        (force-mode-line-update)))))

(defun ticket-tldr--request (prompt input hash)
  "Ask the buffer's model for a summary of spec INPUT under PROMPT.
HASH names the input, so the cache can tell when the spec moved on."
  (require 'gptel)
  (ticket-tldr--abort)
  (let ((request (list :hash hash :model gptel-model :phase 'waiting :started nil)))
    (setq ticket-tldr--request request
          ticket-tldr--note nil)
    (gptel-request input
      :system prompt
      :stream t
      :context request
      :callback #'ticket-tldr--callback)
    ;; gptel falls back to the backend's first model when this one is gone
    (plist-put request :model gptel-model)))

(defun ticket-tldr--refresh (&optional force)
  "Show the cached summary of the spec, and request one unless it is current.
With FORCE, request a new summary even when the cache is current."
  (let* ((spec (ticket-tldr--read-spec ticket-tldr--spec))
         (prompt (ticket-tldr--prompt))
         (input (plist-get spec :input))
         (hash (secure-hash 'sha1 (concat prompt "\n" input)))
         (cache (ticket-tldr--read-cache ticket-tldr--spec)))
    (setq ticket-tldr--facts spec
          ticket-tldr--input-hash hash)
    (when (and cache (not (equal (plist-get cache :hash)
                                 (plist-get ticket-tldr--shown :hash))))
      (ticket-tldr--show cache))
    (when (or force (not (equal (plist-get cache :hash) hash)))
      (ticket-tldr--request prompt input hash))))

;;;###autoload
(defun ticket-tldr (ticket)
  "Show a plain-language summary of TICKET's spec.
The summary comes from its cache until the spec changes."
  (interactive (list (ticket-spec--read-ticket)))
  (let ((spec (or (car (ticket-spec-files ticket))
                  (user-error "No spec for %s" ticket)))
        (buffer (get-buffer-create (format "*tldr: %s*" ticket))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ticket-tldr-mode)
        (require 'gptel)
        (ticket-tldr-mode)
        (setq-local gptel-backend (gptel-get-backend ticket-tldr-backend)
                    gptel-model ticket-tldr-model))
      (setq ticket-tldr--ticket ticket
            ticket-tldr--spec spec)
      (unless ticket-tldr--request
        (ticket-tldr--refresh)))
    (pop-to-buffer buffer)))

(defun ticket-tldr-regenerate ()
  "Write the summary in this buffer anew, cache or not."
  (interactive)
  (ticket-tldr--refresh 'force))

;; Autoloaded: `model-picker-commands' names it before this file loads.
;;;###autoload
(defun ticket-tldr-pick-model ()
  "Pick the model of the summary in this buffer, and write it again."
  (interactive)
  (require 'gptel)
  (let* ((choices (mapcan (pcase-lambda (`(,name . ,backend))
                            (mapcar (lambda (model)
                                      (cons (format "%s:%s" name model)
                                            (cons backend model)))
                                    (gptel-backend-models backend)))
                          gptel--known-backends))
         (pick (cdr (assoc (completing-read "Model: " choices nil t) choices))))
    (setq-local gptel-backend (car pick)
                gptel-model (cdr pick))
    (ticket-tldr-regenerate)))

(defun ticket-tldr-open-spec ()
  "Visit the spec of the summary in this buffer."
  (interactive)
  (ticket-spec-open ticket-tldr--ticket))
