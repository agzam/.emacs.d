;;; modules/ai/autoload/gptel.el -*- lexical-binding: t; -*-

;;; Lazy MCP tool registration for gptel

(defvar ensure-mcp-server--pending-callbacks (make-hash-table :test 'equal)
  "Callbacks waiting for lazy MCP servers to start.")

(defun ensure-mcp-server (server-name callback)
  "Ensure MCP server SERVER-NAME is connected, then call CALLBACK.
If already connected, CALLBACK fires immediately.  Queues concurrent requests.
A cached connection whose process died (server crash) is purged and
restarted instead of being treated as connected."
  (require 'mcp-hub)
  (cond
   ;; connected and the process is actually alive
   ((when-let* ((conn (gethash server-name mcp-server-connections)))
      (jsonrpc-running-p conn))
    (funcall callback))
   ;; currently starting - queue
   ((gethash server-name ensure-mcp-server--pending-callbacks)
    (push callback (gethash server-name ensure-mcp-server--pending-callbacks)))
   ;; start it, purging any dead cached connection first
   (t
    (when (gethash server-name mcp-server-connections)
      (ignore-errors (mcp-stop-server server-name))
      (remhash server-name mcp-server-connections))
    (puthash server-name (list callback) ensure-mcp-server--pending-callbacks)
    (message "Starting MCP server %s..." server-name)
    (mcp-hub-start-all-server
     (lambda ()
       (let ((cbs (gethash server-name ensure-mcp-server--pending-callbacks)))
         (remhash server-name ensure-mcp-server--pending-callbacks)
         (if (gethash server-name mcp-server-connections)
             (progn
               (message "MCP server %s ready" server-name)
               (dolist (cb cbs) (funcall cb)))
           (message "MCP server %s failed to start" server-name))))
     (list server-name)))))

(defun lazy-mcp-tool-fn (server-name tool-name arg-names)
  "Create an async tool function that lazily starts SERVER-NAME for TOOL-NAME.
ARG-NAMES is a list of argument name strings for reconstructing the MCP plist."
  (lambda (callback &rest args)
    (ensure-mcp-server
     server-name
     (lambda ()
       ;; Omitted optional args arrive as nil; elisp nil serializes to {}
       ;; in JSON-RPC, which servers reject. Drop them like other MCP
       ;; clients do. Explicit false is :json-false, so it survives.
       (let ((mcp-args (cl-mapcan
                        (lambda (name val)
                          (when val
                            (list (intern (concat ":" name)) val)))
                        arg-names args)))
         (mcp-async-call-tool
          (gethash server-name mcp-server-connections)
          tool-name
          mcp-args
          (lambda (res)
            (funcall callback (mcp--parse-tool-call-result res)))
          (lambda (code message)
            (funcall callback
                     (format "MCP tool %s error: [%s] %s"
                             tool-name code message)))))))))

(defconst mcp-tool-def-re
  (let ((meta "\\(?:\\^[^][ \t\n(){}]+[ \t]+\\)*"))
    (cons (concat "(def[ \t]+" meta "\\(?:tool-defs?\\|tools\\)\\_>")
          (concat "(def[ \t]+" meta "[a-zA-Z0-9_-]+-tools?\\_>")))
  "Regexps matching a collected and an individual tool definition.
Both tolerate reader metadata, as in (def ^:private tools [...]).")

(defun mcp-tool-source-file (command)
  "Return the file holding COMMAND's MCP tool definitions, or nil.
A launcher shell script hides the definitions, so follow its `-m
NAMESPACE' to the Clojure file that namespace maps to."
  (when (and (stringp command) (file-readable-p command))
    (if (string-match-p "\\.\\(bb\\|clj[sc]?\\)\\'" command)
        command
      (with-temp-buffer
        (insert-file-contents command)
        (goto-char (point-min))
        (let ((case-fold-search nil))
          (when (re-search-forward
                 "\\(?:^\\|[ \t]\\)-m[ \t]+\\([^ \t\n\"']+\\)" nil t)
            (let ((file (expand-file-name
                         (concat "src/"
                                 (subst-char-in-string
                                  ?. ?/ (subst-char-in-string ?- ?_ (match-string 1)))
                                 ".clj")
                         (file-name-directory (expand-file-name command)))))
              (and (file-readable-p file) file))))))))

(defun mcp-tool-defs-from-source (command)
  "Extract tool definitions from the MCP server started by COMMAND.
Returns a list of parsed tool definition hash-tables, nil when the
source isn't readable (missing harness checkout, CI).
Handles both collected defs like (def tools [...]) and individual
defs like (def my-tool {...})."
  (when-let* ((file (mcp-tool-source-file command)))
    (require 'parseedn)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (if (re-search-forward (car mcp-tool-def-re) nil t)
          ;; Collected: (def tools [...]) or (def tool-def {...})
          (when (re-search-forward "[{[]" nil t)
            (backward-char 1)
            (let* ((start (point))
                   (_ (forward-sexp 1))
                   (edn-str (buffer-substring-no-properties start (point)))
                   (result (parseedn-read-str edn-str)))
              (cond
               ((hash-table-p result) (list result))
               ((vectorp result) (append result nil))
               (t nil))))
        ;; Fallback: collect individual (def ...-tool {...}) forms
        (goto-char (point-min))
        (let (tools)
          (while (re-search-forward (cdr mcp-tool-def-re) nil t)
            (when (re-search-forward "{" nil t)
              (backward-char 1)
              (let* ((start (point))
                     (_ (forward-sexp 1))
                     (edn-str (buffer-substring-no-properties start (point)))
                     (result (parseedn-read-str edn-str)))
                (when (hash-table-p result)
                  (push result tools)))))
          (nreverse tools))))))

(defun mcp-schema-node->plist (node)
  "Convert a parsed JSON-schema NODE into the plist shape gptel serializes."
  (cond
   ((hash-table-p node)
    (let (plist)
      (maphash (lambda (k v)
                 (setq plist (nconc plist (list k (mcp-schema-node->plist v)))))
               node)
      plist))
   ((vectorp node) (vconcat (mapcar #'mcp-schema-node->plist node)))
   (t node)))

(defun mcp-schema->gptel-args (schema)
  "Convert MCP inputSchema hash-table to gptel args format."
  (let* ((properties (gethash :properties schema))
         (required (when-let* ((r (gethash :required schema)))
                     (append r nil)))
         (args '()))
    (when properties
      (maphash
       (lambda (key val)
         (let* ((name (substring (symbol-name key) 1))
                (arg (list :name name
                           :type (gethash :type val "string")
                           :description (or (gethash :description val) ""))))
           ;; gptel rejects an array argument that does not describe its
           ;; items, so these constraints have to survive the conversion.
           (dolist (extra '(:enum :items))
             (when-let* ((v (gethash extra val)))
               (setq arg (append arg (list extra (mcp-schema-node->plist v))))))
           (unless (member name required)
             (setq arg (append arg '(:optional t))))
           (push arg args)))
       properties))
    (nreverse args)))

;;;###autoload
(defun register-mcp-tools-lazy ()
  "Register MCP tools with gptel from server script schemas.
Reads tool definitions directly from the server sources using parseedn.
Tools appear in gptel immediately; servers start lazily on first use."
  (dolist (server mcp-hub-servers)
    (let* ((server-name (car server))
           (command (plist-get (cdr server) :command))
           (category server-name)
           (tool-defs (mcp-tool-defs-from-source command)))
      (dolist (td tool-defs)
        (let* ((tool-name (gethash :name td))
               (description (or (gethash :description td) ""))
               (schema (gethash :inputSchema td))
               (args (when schema (mcp-schema->gptel-args schema))))
          (gptel-make-tool
           :function (lazy-mcp-tool-fn
                      server-name tool-name
                      (mapcar (lambda (a) (plist-get a :name)) args))
           :name tool-name
           :async t
           :description description
           :args args
           :category category))))))

;;;###autoload
(defun mcp-servers-from-eca-config (&optional config-file)
  "Read MCP servers from ECA config.json, return `mcp-hub-servers' alist.
Parses CONFIG-FILE, by default ~/.config/eca/config.json (shared with ECA
and Claude Code CLI), and converts mcpServers entries to the format
expected by `mcp-hub-servers'."
  (require 'json)
  (when-let* ((config-file (expand-file-name
                            (or config-file "~/.config/eca/config.json")))
              (_ (file-exists-p config-file))
              (json (json-read-file config-file))
              (servers (alist-get 'mcpServers json)))
    (cl-loop
     for entry in servers
     for props = (cdr entry)
     unless (alist-get 'disabled props)
     collect
     (let* ((name (symbol-name (car entry)))
            (command (alist-get 'command props))
            (env-alist (alist-get 'env props))
            (result (list name :command command)))
       (when env-alist
         (nconc result
                (list :env
                      (cl-loop for (k . v) in env-alist
                               nconc (list (intern (concat ":" (symbol-name k))) v)))))
       result))))

;;;###autoload
(defun eca-agents-md-content (&optional file)
  "Read FILE, by default ~/.config/eca/AGENTS.md, and return its content.
Returns nil if the file does not exist."
  (let ((file (expand-file-name (or file "~/.config/eca/AGENTS.md"))))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

;;; Improve-text transient

(defvar gptel-improve-text-prompt nil)

(defvar gptel-improve-text-prompts-history
  (list
   (concat "You are a spelling corrector and text improver. "
           "Correct mistakes, but do not alter the text structure unless stylistic, "
           "orthographic, morphologic and other linguistic errors found. "
           "Do not replace hyphens with em-dash, keep the hyphens. "
           "Exclude any explanations - response must contain ONLY the altered text "
           "or nothing, if there were no changes.")

   (concat "You are a fact-checker and text enhancer. "
           "Fix mistakes and flag factual inaccuracies, do not alter the text structure "
           "unless it is absolutely necessary. "
           "Do not replace hyphens with em-dash, keep the hyphens. "
           "Exclude any explanations - response must contain ONLY the altered text "
           "or nothing, if there were no changes.")

   (concat "You are spelling corrector and text enhancer. "
           "Provide 3 different improved variations of the given text, "
           "separating each variant with: "
           "\n\n---\n\n"
           "Do not use em-dash, instead use hyphens"
           "Do not include any explanations, titles, headers or bullet points "
           "- ONLY plain text of variants, nothing else!")

   (concat "You are an experienced software developer. Explain the given code snippet, "
           "diving into technical details for better understanding. "
           "Suggest a better approach if necessary. "
           "Strive for concise code that is easy-to-reason about. "
           "Optionally, recommend libraries, tools and literature for better "
           "understanding the problem and improving upon it.")

   (concat "You are a great software developer. "
           "You strive for simplicity in your code "
           "that is both concise and easy-to-reason about. "
           "Add comments to the provide code snippet, without changing the code itself."
           "Do not include any headers, titles or explanations outside of the snippet, "
           "keep the three ticks with the language designator (markdown code markup).")))

;; Unlike Doom, the lab loads transient lazily - make sure the definers
;; below expand no matter which autoload fires first.
(require 'transient)

(transient-define-infix gptel-improve-text--infix-prompt ()
  "Prompt selection for improving text."
  :description "Set prompt"
  :prompt "Prompt: "
  :variable 'gptel-improve-text-prompt
  :class 'transient-lisp-variable
  :key "- RET"
  :format "%k %d"
  :reader (lambda (prompt &rest _)
            ;; usual bs to keep the preserve the list order
            (let* ((comp-table (lambda (cs)
                                 (lambda (str pred action)
                                   (if (eq action 'metadata)
                                       `(metadata (display-sort-function . ,#'identity))
                                     (complete-with-action action cs str pred)))))
                   (sel (completing-read
                         prompt
                         (funcall
                          comp-table
                          gptel-improve-text-prompts-history))))
              (add-to-list 'gptel-improve-text-prompts-history
                           sel)
              sel)))

(transient-define-infix gptel-improve-text--write-own-prompt ()
  "Custom prompt for improving text."
  :description "Write your own prompt"
  :prompt "Prompt: "
  :variable 'gptel-improve-text-prompt
  :class 'transient-lisp-variable
  :key "i"
  :format "%k %d"
  :reader (lambda (&rest _) (read-string "Prompt: ")))

;; Soft so the file loads in the test env, where gptel isn't installed;
;; at runtime the package is always there.
(require 'gptel-transient nil t)

;; The in-place flow rides on gptel-rewrite, loaded on demand.  The
;; defvar keeps the let below dynamic even before that load.
(defvar gptel--rewrite-directive)
(defvar gptel--rewrite-message)
(defvar gptel-rewrite-actions-map)
(defvar smerge-mode-map)
(declare-function gptel--suffix-rewrite "gptel-rewrite")
(declare-function gptel--rewrite-update-status "gptel-rewrite")
(declare-function gptel--rewrite-overlay-at "gptel-rewrite")
(declare-function gptel--rewrite-reject "gptel-rewrite")

(defun keymap-hint-segment (map cmd label face)
  "Render \"KEY LABEL\" for CMD in MAP, spacehammer-edit style.
Skips mouse bindings and non-normal evil state bindings; evil keymaps
embed states as pseudo-keys, so a normal-state binding like
\"<normal-state> ] ]\" is shown as \"]]\".  Prefers the shortest key."
  (let* ((descs (thread-last
                  (where-is-internal cmd map)
                  (mapcar #'key-description)
                  (seq-remove (lambda (d) (string-match-p "mouse" d)))
                  (mapcar (lambda (d)
                            (string-remove-prefix "<normal-state> " d)))
                  (seq-remove (lambda (d) (string-match-p "-state>" d)))))
         ;; shortest key wins; among equals (the digit run) the lowest,
         ;; so a 1-9 binding advertises as "1"
         (desc (or (car (seq-sort (lambda (a b)
                                    (or (< (length a) (length b))
                                        (and (= (length a) (length b))
                                             (string< a b))))
                                  descs))
                   "?"))
         ;; "] ]" reads as two keystrokes only to Emacs; show "]]".
         ;; Multi-char tokens (C-c, RET) keep their separating spaces.
         (desc (if (string-match-p "\\`.\\( .\\)+\\'" desc)
                   (string-replace " " "" desc)
                 desc)))
    (concat (propertize desc 'face `(:weight bold :inherit ,face))
            (propertize (concat " " label) 'face 'shadow))))

(defun keymap-hint-line (map segments)
  "Join SEGMENTS - (CMD LABEL FACE) lists - into a │-separated hint for MAP."
  (string-join
   (mapcar (lambda (seg) (apply #'keymap-hint-segment map seg)) segments)
   (propertize " │ " 'face 'shadow)))

;;;###autoload
(defun gptel-rewrite-ready-banner (ov)
  "Replace OV's \" Ready\" status with a legend of the pending actions.
For `gptel-rewrite-default-action': the rewrite overlay's keys are
otherwise invisible - RET's dispatch menu is the only discoverable
entry.  Rendered spacehammer-edit style: key, label, │ separators."
  (gptel--rewrite-update-status
   ov (concat
       " "
       (keymap-hint-line
        gptel-rewrite-actions-map
        '((gptel--rewrite-accept "accept" success)
          (gptel-rewrite-merge-sentences "merge" font-lock-constant-face)
          (gptel--rewrite-diff "diff" font-lock-keyword-face)
          (gptel--rewrite-ediff "ediff" font-lock-type-face)
          (gptel--rewrite-iterate "iterate" warning)
          (gptel--rewrite-reject "reject" error))))))

;;; Sentence-granular merge for rewrite overlays

(defvar-local gptel-improve-text-last-region nil
  "Cons of markers around the last completed sentence review.
The merge drops the rewrite overlay, so iterating after a review needs
a new request; this lets `gptel-improve-text' re-run over the same spot
without re-selecting.")

(defconst gptel-rewrite-glue-newline
  (propertize "\n" 'gptel-rewrite-glue t)
  "Structural newline injected to keep conflict markers on their own lines.
The text property lets the review cleanup find and remove these after
resolution, restoring the original line joins - unwrapped one-line
paragraphs come back out as one line.")

(defun gptel-rewrite-sentence-split (text)
  "Split TEXT into sentence chunks whose concatenation is exactly TEXT.
Inter-sentence whitespace rides on the preceding chunk, so unchanged
chunks reproduce the original spacing verbatim."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((sentence-end-double-space nil)
          (pos (point-min))
          chunks)
      (while (not (eobp))
        (forward-sentence)
        (skip-chars-forward " \t\n")
        (when (= (point) pos)           ;safety: never stall
          (goto-char (point-max)))
        (push (buffer-substring-no-properties pos (point)) chunks)
        (setq pos (point)))
      (nreverse chunks))))

(defun gptel-rewrite-sentence-hunks (old-lines new-lines)
  "Diff two string lists into hunks ((OBEG OEND NBEG NEND) ...), 1-based.
An empty side is encoded as BEG greater than END.  Returns nil when the
lists match; degrades to one all-covering hunk if diff is unusable."
  (let ((old-file (make-temp-file "sentence-hunks-a"))
        (new-file (make-temp-file "sentence-hunks-b")))
    (unwind-protect
        (progn
          (with-temp-file old-file
            (dolist (l old-lines) (insert l "\n")))
          (with-temp-file new-file
            (dolist (l new-lines) (insert l "\n")))
          (with-temp-buffer
            (let ((status (call-process "diff" nil t nil old-file new-file)))
              (if (not (memq status '(0 1)))
                  (list (list 1 (length old-lines) 1 (length new-lines)))
                (goto-char (point-min))
                (let (hunks)
                  (while (re-search-forward
                          "^\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?\\([acd]\\)\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?$"
                          nil t)
                    (let ((ob (string-to-number (match-string 1)))
                          (oe (string-to-number (or (match-string 2) (match-string 1))))
                          (op (aref (match-string 3) 0))
                          (nb (string-to-number (match-string 4)))
                          (ne (string-to-number (or (match-string 5) (match-string 4)))))
                      (push (pcase op
                              (?c (list ob oe nb ne))
                              (?d (list ob oe (1+ nb) nb))
                              (?a (list (1+ ob) ob nb ne)))
                            hunks)))
                  (nreverse hunks))))))
      (delete-file old-file)
      (delete-file new-file))))

(defun gptel-rewrite-sentence-conflicts (original improved label)
  "Merge IMPROVED into ORIGINAL as per-sentence conflict blocks.
Returns nil when the two differ only in whitespace.  Unchanged
sentences are emitted verbatim from ORIGINAL - the model gets no say
over pure whitespace - and each differing run becomes its own conflict
block (upper original, lower IMPROVED, labeled LABEL) so smerge can
accept or reject change by change.  Marker-separating newlines carry
`gptel-rewrite-glue' for post-resolution removal."
  (let* ((osents (gptel-rewrite-sentence-split original))
         (isents (gptel-rewrite-sentence-split improved))
         (norm (lambda (s) (string-join (split-string s) " ")))
         (hunks (gptel-rewrite-sentence-hunks
                 (mapcar norm osents) (mapcar norm isents))))
    (when hunks
      (let ((ovec (vconcat osents))
            (ivec (vconcat isents))
            (oi 1)
            (out '()))
        (cl-flet ((emit (s) (push s out))
                  (range (vec from to)
                    (when (<= from to)
                      (push (mapconcat #'identity
                                       (cl-subseq vec (1- from) to) "")
                            out)))
                  (ensure-bol ()
                    (when (and out (not (string-suffix-p "\n" (car out))))
                      (push gptel-rewrite-glue-newline out))))
          (pcase-dolist (`(,ob ,oe ,nb ,ne) hunks)
            (range ovec oi (1- ob))     ;equal run before the hunk
            (ensure-bol)
            (emit "<<<<<<< original")
            (emit gptel-rewrite-glue-newline)
            (range ovec ob oe)
            (ensure-bol)
            (emit "=======")
            (emit gptel-rewrite-glue-newline)
            (range ivec nb ne)
            (ensure-bol)
            (emit (concat ">>>>>>> " label))
            (emit gptel-rewrite-glue-newline)
            (setq oi (1+ (max oe (1- ob)))))
          (range ovec oi (length osents)) ;trailing equal run
          (apply #'concat (nreverse out)))))))

(defun gptel-rewrite-review-header ()
  "Header line advertising the smerge keys during a sentence review."
  (list (propertize " REVIEW "
                    'face '(:weight bold :inherit font-lock-function-name-face))
        (propertize "│ " 'face 'shadow)
        (keymap-hint-line
         smerge-mode-map
         '((smerge-next "next" font-lock-function-name-face)
           (smerge-prev "prev" font-lock-function-name-face)
           (smerge-keep-upper "keep original" success)
           (smerge-keep-lower "take rewrite" warning)
           (smerge-resolve "resolve" font-lock-constant-face)))))

(defun gptel-rewrite-arm-review-cleanup (beg end)
  "Finish the sentence review once BEG..END holds no more conflicts.
Watches from a buffer-local `post-command-hook', so any smerge command
can resolve the last conflict.  Finishing removes the injected glue
newlines (rejoining unwrapped paragraphs), restores the header line,
and turns off `smerge-mode' unless conflicts remain elsewhere."
  (let* ((beg-m (copy-marker beg))
         (end-m (copy-marker end t))
         (saved-header header-line-format)
         (watch nil))
    (setq header-line-format (gptel-rewrite-review-header))
    (setq watch
          (lambda ()
            (unless (save-excursion
                      (goto-char beg-m)
                      (re-search-forward "^<<<<<<< " end-m t))
              (save-excursion
                (let (pos)
                  (while (setq pos (text-property-any
                                    beg-m end-m 'gptel-rewrite-glue t))
                    (delete-region
                     pos (min (or (next-single-property-change
                                   pos 'gptel-rewrite-glue)
                                  end-m)
                              end-m)))))
              (setq header-line-format saved-header)
              (when (and (bound-and-true-p smerge-mode)
                         (not (save-excursion
                                (goto-char (point-min))
                                (re-search-forward "^<<<<<<< " nil t))))
                (smerge-mode -1))
              (remove-hook 'post-command-hook watch t)
              (setq gptel-improve-text-last-region
                    (cons (copy-marker beg-m) (copy-marker end-m t)))
              (set-marker beg-m nil)
              (set-marker end-m nil)
              (message "improve-text: review complete"))))
    (add-hook 'post-command-hook watch nil t)))

(defun gptel-rewrite-merge-review (beg end improved label)
  "Replace BEG..END with a sentence-conflict merge against IMPROVED.
Enables `smerge-mode', arms the review cleanup, and returns the merge's
start position.  Returns nil - leaving the region untouched - when
IMPROVED differs from the region only in whitespace.  LABEL names the
lower conflict side."
  (let* ((original (buffer-substring-no-properties beg end))
         (merged (gptel-rewrite-sentence-conflicts original improved label)))
    (when merged
      (save-excursion
        (goto-char beg)
        (delete-region beg end)
        (let ((mbeg (point)))
          ;; a marker-leading merge needs its own line; insert after MBEG
          ;; so the cleanup region covers this glue
          (when (and (not (bolp)) (string-prefix-p "<<<<<<<" merged))
            (insert gptel-rewrite-glue-newline))
          (insert merged)
          (require 'smerge-mode)
          (smerge-mode 1)
          (gptel-rewrite-arm-review-cleanup mbeg (point))
          mbeg)))))

(defun gptel-rewrite-review-go (pos)
  "Drop point inside the first conflict at or after POS; open the panel.
`smerge-next' skips a conflict already at point, and the keep commands
need point within one to act."
  (goto-char pos)
  (unless (looking-at-p "<<<<<<<")
    (re-search-forward "^<<<<<<< " nil t))
  (forward-line 1)
  (when (fboundp 'smerge-transient)
    (smerge-transient)))

;;;###autoload
(defun gptel-rewrite-merge-sentences (&optional ovs)
  "Merge pending rewrites in OVS as sentence-granular smerge conflicts.
Unchanged sentences stay untouched; each changed run becomes its own
conflict block, so the rewrite is accepted or rejected change by
change rather than as one region-sized lump.  A whitespace-only
rewrite is dropped, keeping the original.  Resolution is watched by
`gptel-rewrite-arm-review-cleanup', which rejoins the text when the
review is done."
  (interactive (list (gptel--rewrite-overlay-at)))
  (when-let* ((ov-buf (overlay-buffer (or (car-safe ovs) ovs)))
              ((buffer-live-p ov-buf)))
    (with-current-buffer ov-buf
      (let ((label (if (and (boundp 'gptel-backend) (fboundp 'gptel-backend-name))
                       (gptel-backend-name gptel-backend)
                     "rewrite"))
            (conflicted nil))
        ;; iterating re-pushes the same overlay onto the pending list, and
        ;; replacing its region evaporates it - visit each live one once
        (dolist (ov (seq-uniq (ensure-list ovs)))
          (when-let* ((improved (overlay-get ov 'gptel-rewrite))
                      ((overlay-buffer ov)))
            (if-let* ((mbeg (gptel-rewrite-merge-review
                             (overlay-start ov) (overlay-end ov)
                             improved label)))
                (setq conflicted (or conflicted mbeg))
              (message "improve-text: only whitespace differences; keeping original"))))
        (gptel--rewrite-reject ovs)
        (when conflicted
          (gptel-rewrite-review-go conflicted))))))

;;; Variant picking

;; The variants prompt asks for N rewrites separated by "---".  The
;; response lands in a picker buffer; choosing a variant replays it
;; through the same sentence-granular smerge review the in-place path
;; uses, against the origin region captured as markers at request time.

(defvar-local gptel-improve-text-variants nil
  "Variant strings offered by this picker buffer, in response order.")

(defvar-local gptel-improve-text-origin nil
  "Cons of markers around the text the picked variant will replace.
Captured when the request was sent, so the review targets the origin
region even after edits or a buffer switch.")

(defvar gptel-improve-text-variants-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "RET" #'gptel-improve-text-pick-variant)
    (keymap-set map "q" #'gptel-improve-text-dismiss-variants)
    (dotimes (i 9)
      (keymap-set map (number-to-string (1+ i))
                  #'gptel-improve-text-pick-nth-variant))
    map)
  "Keys on the picker's variant sections.
Rides on the `keymap' text property, which Emacs consults before evil's
state maps - digits and RET stay ours even in normal state.")

(defun gptel-improve-text-parse-variants (text)
  "Split TEXT into the variants the model separated with \"---\" lines.
Tolerates ragged separators - extra hyphens, stray blanks, missing
surrounding blank lines.  Hyphen runs inside a line stay untouched.
Returns trimmed non-empty variants in order."
  (thread-last
    (split-string text "^[ \t]*-\\{3,\\}[ \t]*$")
    (mapcar #'string-trim)
    (seq-remove #'string-empty-p)))

(defun gptel-improve-text-show-variants (variants region)
  "Pop a picker buffer offering VARIANTS for the REGION markers.
Each variant renders under a numbered banner and carries its index as
the `gptel-improve-text-variant' text property, so both digits and RET
at point resolve a choice.  The header line advertises the keys."
  (let ((buf (generate-new-buffer "*improve-text variants*")))
    (with-current-buffer buf
      (seq-do-indexed
       (lambda (variant i)
         (let ((start (point)))
           (insert (propertize
                    (format " %d " (1+ i))
                    'face '(:weight bold :inherit font-lock-function-name-face))
                   "\n" variant "\n\n")
           (put-text-property start (point)
                              'gptel-improve-text-variant (1+ i))))
       variants)
      (add-text-properties (point-min) (point-max)
                           (list 'keymap gptel-improve-text-variants-map))
      (setq gptel-improve-text-variants variants
            gptel-improve-text-origin region)
      (setq header-line-format
            (keymap-hint-line
             gptel-improve-text-variants-map
             '((gptel-improve-text-pick-variant "pick this" success)
               (gptel-improve-text-pick-nth-variant "pick nth" warning)
               (gptel-improve-text-dismiss-variants "dismiss" error))))
      (visual-line-mode 1)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (switch-to-buffer-other-window buf)))

(defun gptel-improve-text-pick-variant (&optional n)
  "Send variant N (default: the one at point) into a sentence review.
The picker closes and the variant lands on the origin region as
per-sentence smerge conflicts - the same review the in-place rewrite
produces.  A variant that only differs in whitespace keeps the picker
open for another choice."
  (interactive)
  (let* ((n (or n (get-text-property (point) 'gptel-improve-text-variant)
                (user-error "No variant at point")))
         (variant (or (nth (1- n) gptel-improve-text-variants)
                      (user-error "No variant %d" n)))
         (picker (current-buffer)))
    (pcase-let ((`(,beg . ,end) gptel-improve-text-origin))
      (unless (and (markerp beg) (marker-buffer beg)
                   (buffer-live-p (marker-buffer beg)))
        (user-error "The buffer this text came from is gone"))
      (if-let* ((mbeg (with-current-buffer (marker-buffer beg)
                        (gptel-rewrite-merge-review
                         beg end variant (format "variant %d" n)))))
          (progn
            (if-let* ((win (get-buffer-window picker)))
                (quit-window 'kill win)
              (kill-buffer picker))
            (pop-to-buffer (marker-buffer beg))
            (set-marker beg nil)
            (set-marker end nil)
            (gptel-rewrite-review-go mbeg))
        (message "improve-text: variant %d only differs in whitespace" n)))))

(defun gptel-improve-text-pick-nth-variant ()
  "Pick the variant numbered by the digit key that invoked this."
  (interactive)
  (gptel-improve-text-pick-variant (- last-command-event ?0)))

(defun gptel-improve-text-dismiss-variants ()
  "Kill the picker, restoring the window it borrowed."
  (interactive)
  (quit-window 'kill))

;;;###autoload
(transient-define-prefix gptel-improve-text-transient ()
  "Improve region with gptel."
  [:description
   (lambda ()
     (concat
      (or gptel-improve-text-prompt
          (car gptel-improve-text-prompts-history)) "\n"))
   [(gptel--infix-provider)
    (gptel-improve-text--infix-prompt)
    (gptel-improve-text--write-own-prompt)]
   [""
    ("C-<return>" "Let's go" gptel-improve-text)]]
  [:hide always
   :class transient-subgroups
   :setup-children
   (lambda (_)
     "easy toggling prompt variations"
     (transient-parse-suffixes
      'gptel-improve-text-transient
      (thread-last
        (seq-take gptel-improve-text-prompts-history 5)
        (seq-map-indexed
         (lambda (prompt idx)
           (let ((n (number-to-string (1+ idx))))
             (list
              n (format "Use prompt %s" n)
              (lambda ()
                (interactive)
                (setq gptel-improve-text-prompt
                      prompt))
              :transient t)))))))])

(defun gptel-improve-text-handle-response (resp info)
  "Route improve-text RESP to the variant picker or a side buffer.
Only the aside prompts (variants, code explanations) land here; the
in-place prompts ride on `gptel-rewrite'.  A response whose request
captured an origin region (the variants prompt) opens the picker;
everything else dumps into a markdown side buffer.  Per the
`gptel-request' callback contract, cons cells carry reasoning/tool
chunks (ignored), and nil means the request failed - its cause lives
in INFO."
  (cond
   ((stringp resp)
    (if-let* ((region (plist-get (plist-get info :context) :improve-region))
              (variants (gptel-improve-text-parse-variants resp)))
        (gptel-improve-text-show-variants variants region)
      (let* ((model (or (let-plist info .data.model)
                        (and (boundp 'gptel-model) gptel-model)))
             (buf (generate-new-buffer (format "* %s *" model))))
        (with-current-buffer buf
          (markdown-mode)
          (insert resp))
        (switch-to-buffer-other-window buf))))
   ;; reasoning / tool-call / tool-result chunks: not the response
   ((consp resp) nil)
   ((eq resp 'abort) (message "gptel-improve-text: request aborted"))
   (t (message "gptel-improve-text failed: %s" (plist-get info :status)))))

;;;###autoload
(defun gptel-improve-text (&optional _arg)
  (interactive "P")
  (unless (region-active-p)
    ;; a finished review dropped its overlay; re-arm over the same spot
    (pcase-let ((`(,beg . ,end) gptel-improve-text-last-region))
      (unless (and beg (marker-position beg) (marker-position end))
        (user-error "no selection"))
      (goto-char end)
      (push-mark beg t t)
      (message "improve-text: reusing the last reviewed region")))
  (setq gptel-improve-text-prompt (or gptel-improve-text-prompt
                                      (car gptel-improve-text-prompts-history)))
  (let ((in-place? (string-match-p
                    "fix mistakes\\|correct mistakes\\|simplify"
                    gptel-improve-text-prompt)))
    (message "beep-bop... checking your crap with %s" gptel-model)
    (if in-place?
        (let ((instruction
               "Apply the directive. Output only the final replacement text."))
          (require 'gptel-rewrite nil t)
          ;; Buffer-local so the overlay's iterate resends both.  Iterate
          ;; reads `gptel--rewrite-message' - nil would become an empty
          ;; content block, which Anthropic rejects with HTTP 400.
          (setq-local gptel--rewrite-directive gptel-improve-text-prompt)
          (setq-local gptel--rewrite-message instruction)
          (gptel--suffix-rewrite instruction))
      ;; the variants prompt comes back through the picker into a review
      ;; of the origin region - marked now, since the response arrives
      ;; after point moved on (last-region only exists after a review)
      (gptel-request (buffer-substring-no-properties
                      (region-beginning) (region-end))
        :system gptel-improve-text-prompt
        :context (when (string-match-p "variant\\|variation"
                                       gptel-improve-text-prompt)
                   (list :improve-region
                         (cons (copy-marker (region-beginning))
                               (copy-marker (region-end) t))))
        :callback #'gptel-improve-text-handle-response))))

;;; Chat buffer helpers

;;;###autoload
(defun gptel-clear-buffer ()
  (interactive)
  (let* ((beg-marker (concat "^" (alist-get gptel-default-mode gptel-prompt-prefix-alist)))
         (keep-line (save-excursion
                      (goto-char (point-max))
                      (when (re-search-backward beg-marker nil t)
                        (unless (save-excursion
                                  (forward-line)
                                  (re-search-forward beg-marker nil t))
                          (point))))))
    (delete-region (point-min) keep-line)
    (evil-insert-state)))

;;;###autoload
(defun open-gptel (&optional arg)
  (interactive "P")
  ;; gptel-mode unbound = gptel never loaded = no chat buffers to find
  (let ((last-b (unless (or arg (not (boundp 'gptel-mode)))
                  (thread-last
                    (buffer-list)
                    (seq-filter
                     (lambda (buf)
                       (and
                        (buffer-local-value 'gptel-mode buf)
                        (buffer-file-name buf)
                        (not (string-match-p
                              ".*quick.org$"
                              (buffer-file-name buf))))))
                    (seq-sort
                     (lambda (a b)
                       (string> (buffer-name a) (buffer-name b))))
                    (seq-first)))))
    (if last-b
        (progn
          (display-buffer last-b)
          (switch-to-buffer last-b))
      (call-interactively #'gptel-agent))))

;;;###autoload
(defun gptel-inline-project-chat-buffer (buf)
  "Return or create a per-project gptel-inline chat buffer for BUF.
Like `gptel-inline-project-buffer', but never matches indirect buffers:
abandoned gptel-inline prompts are indirect gptel-mode clones living in
the project root, and treating one as the chat nests clones and drifts
buffer names (\"*gptel:proj*<2><2>\")."
  (with-current-buffer buf
    (and-let* ((proj (project-current))
               (root (expand-file-name (project-root proj))))
      (if-let* ((matching
                 (match-buffers
                  (lambda (b-or-n)
                    (and-let* ((b (get-buffer b-or-n)))
                      (and (buffer-local-value 'gptel-mode b)
                           (not (buffer-base-buffer b))
                           (not (= (aref (buffer-name b) 0) 32))
                           (string= (expand-file-name
                                     (buffer-local-value 'default-directory b))
                                    root)))))))
          (buffer-name (car matching))
        (list (format "*gptel:%s*"
                      (file-name-nondirectory (substring root nil -1)))
              root)))))

;;;###autoload
(defun gptel-inline-visit-last-chat ()
  "Pop to the chat session backing gptel-inline in this buffer.
The response overlay is only a viewport on the latest exchange; the full
conversation lives in a background chat buffer that gptel-inline
remembers per origin buffer, surviving overlay dismissal."
  (interactive)
  (if-let* ((buf (bound-and-true-p gptel-inline--last))
            ((buffer-live-p buf)))
      (pop-to-buffer buf)
    (user-error "No gptel-inline session for this buffer yet")))

(defvar-local gptel-inline-stashed-overlay nil
  "Last ESC-dismissed response overlay: (OVERLAY START-MARKER END-MARKER).
One slot per buffer - dismissing another overlay replaces it, and
`gptel-inline-dwim' re-attaches it.")

;;;###autoload
(defun gptel-inline-dismiss-overlay-h ()
  "Stash-dismiss the visible gptel-inline response overlay.
For `doom-escape-hook': claims the ESC press only while an overlay is on
screen, falling through to the other escape handlers otherwise.  The
overlay is detached, not destroyed, so `gptel-inline-dwim' can bring it
back.  A response still streaming while detached keeps landing in the
chat buffer, not the overlay."
  (when (bound-and-true-p gptel-inline--response-overlay-mode)
    (when-let* ((ov (gptel-inline--response-overlay-at-point)))
      (setq gptel-inline-stashed-overlay
            (list ov
                  (copy-marker (overlay-start ov))
                  (copy-marker (overlay-end ov))))
      (delete-overlay ov)
      (gptel-inline--response-overlay-mode -1)
      t)))

;;;###autoload
(defun gptel-inline-restore-overlay ()
  "Re-attach this buffer's stashed response overlay and re-arm its UI.
The visibility tracker and minor mode removed themselves when the
overlay detached, so both are re-armed here.  Jumps to the overlay when
it lands outside the window (an evil jump, so \\[evil-jump-backward]
returns)."
  (interactive)
  (pcase gptel-inline-stashed-overlay
    (`(,ov ,beg ,end)
     (setq gptel-inline-stashed-overlay nil)
     (move-overlay ov beg end (current-buffer))
     (set-marker beg nil)
     (set-marker end nil)
     (gptel-inline--setup-response-overlay-keymap ov)
     (gptel-inline--response-overlay-mode 1)
     (gptel-inline--response-overlay-render ov)
     (unless (pos-visible-in-window-p (overlay-start ov))
       (evil-set-jump)
       (goto-char (overlay-start ov))
       (recenter)))
    (_ (user-error "No dismissed gptel-inline overlay in this buffer"))))

;;;###autoload
(defun gptel-inline-dwim ()
  "Single-key gptel-inline lifecycle.
Re-attach the stashed (ESC-dismissed) overlay if there is one; open the
action menu on a visible overlay (what upstream puts on M-RET, which
mode maps like dired's shadow); otherwise open the prompt strip, which
resumes this buffer's session or starts one."
  (interactive)
  (if (car-safe gptel-inline-stashed-overlay)
      (gptel-inline-restore-overlay)
    (if-let* (((fboundp 'gptel-inline--response-overlay-at-point))
              (ov (gptel-inline--response-overlay-at-point)))
        (gptel-inline--response-overlay-dispatch ov)
      (gptel-inline))))

;;;###autoload
(defun gptel-chat-quadrant-buffer-p (buffer-or-name _action)
  "Non-nil for gptel chat buffers that belong in the quadrant window.
Excludes indirect buffers: gptel-inline's prompt is an indirect clone
carrying the chat buffer's name, and matching it here would override the
small below-selected window the package asks for."
  (when-let* ((buf (get-buffer buffer-or-name)))
    (and (string-match-p (rx bos (or "*Claude" "*ChatGPT" "gptel-"))
                         (buffer-name buf))
         (not (buffer-base-buffer buf)))))

;;;###autoload
(defun gptel-persist-history (_beg _end)
  "Save gptel dedicated buffer to disk.
Only acts on buffers with `gptel-mode' active, skipping transient
uses like `gptel-send' from scratch."
  (when (and (bound-and-true-p gptel-mode)
             (not (buffer-file-name (current-buffer))))
    (let ((suffix (format-time-string "%Y-%m-%d-%T" (current-time)))
          (chat-dir (concat org-default-folder "/gptel"))
          (ext (replace-regexp-in-string "-mode$" "" (symbol-name gptel-default-mode)))
          (dir default-directory))
      (unless (file-directory-p chat-dir)
        (make-directory chat-dir :parents))
      (write-file
       (expand-file-name (concat "gptel-" suffix "." ext) chat-dir))
      ;; write-file re-homes default-directory to chat-dir; keep the chat
      ;; anchored to where it was born - gptel-inline routes per-project
      ;; sessions and agent tools resolve paths through this directory.
      (setq default-directory dir))))

;;;###autoload
(defun gptel-quick-question-buffer ()
  "Opens `a quick question` buffer - an `inbox` buffer for uncategorized
gptel conversations."
  (interactive)
  (let ((buf
         (find-file (concat org-default-folder "/gptel/quick.org"))))
    (when (not (buffer-modified-p buf))
      (org-goto-bottommost-heading)
      (org-narrow-to-subtree)
      (gptel-mode +1))))

;;;###autoload
(defun gptel-log-find ()
  "Grep for things in gptel log files."
  (interactive)
  ;; consult-ripgrep-args is only bound once consult loads
  (require 'consult)
  (let* ((dir (concat org-default-folder "/gptel"))
         (initial (if (use-region-p)
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))
                    (or (when-let ((s (symbol-at-point)))
                          (symbol-name s))
                        "^")))
         (consult-ripgrep-args
          (concat consult-ripgrep-args " --sortr=modified"))
         (consult-async-min-input 0))  ; Show results immediately
    (consult-ripgrep dir initial)))