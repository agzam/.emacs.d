;;; tests/ai/gptel-tests.el --- ai/autoload/gptel.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; let-plist is a lab macro (elisp module); load it first so eager
;; macroexpansion of gptel-improve-text resolves cleanly.
(load-module-file "modules/elisp/autoload/let-plist.el")
(load-module-file "modules/ai/autoload/gptel.el")

(describe "mcp-servers-from-eca-config"
  :var (config-file)
  (before-all
    (setq config-file (make-temp-file "eca-config" nil ".json"))
    (with-temp-file config-file
      (insert "{\"mcpServers\": {"
              "\"slack\": {\"command\": \"/x/slack.bb\"},"
              "\"dead\": {\"command\": \"/x/dead.bb\", \"disabled\": true},"
              "\"envy\": {\"command\": \"/x/envy.bb\", \"env\": {\"FOO\": \"bar\"}}}}")))
  (after-all
    (delete-file config-file))

  (it "converts enabled servers to the mcp-hub-servers shape"
    (let ((servers (mcp-servers-from-eca-config config-file)))
      (expect (length servers) :to-equal 2)
      (expect (assoc "slack" servers)
              :to-equal '("slack" :command "/x/slack.bb"))))

  (it "skips disabled servers"
    (expect (assoc "dead" (mcp-servers-from-eca-config config-file))
            :to-be nil))

  (it "carries env entries over as keyword plists"
    (let ((envy (assoc "envy" (mcp-servers-from-eca-config config-file))))
      (expect (plist-get (cdr envy) :env) :to-equal '(:FOO "bar"))))

  (it "returns nil when the config file is missing"
    (expect (mcp-servers-from-eca-config "/nonexistent/eca/config.json")
            :to-be nil)))

(describe "eca-agents-md-content"
  (it "returns file content as a string"
    (let ((f (make-temp-file "agents" nil ".md")))
      (unwind-protect
          (progn
            (with-temp-file f (insert "# AGENTS\nbe nice\n"))
            (expect (eca-agents-md-content f) :to-equal "# AGENTS\nbe nice\n"))
        (delete-file f))))

  (it "returns nil when the file does not exist"
    (expect (eca-agents-md-content "/nonexistent/AGENTS.md") :to-be nil)))

(describe "mcp-schema->gptel-args"
  :var (schema)
  (before-all
    (let ((props (make-hash-table :test 'equal))
          (q (make-hash-table :test 'equal))
          (n (make-hash-table :test 'equal)))
      (puthash :type "string" q)
      (puthash :description "the query" q)
      (puthash :type "integer" n)
      (puthash :query q props)
      (puthash :count n props)
      (setq schema (make-hash-table :test 'equal))
      (puthash :properties props schema)
      (puthash :required ["query"] schema)))

  (it "converts properties into gptel arg plists"
    (let* ((args (mcp-schema->gptel-args schema))
           (q (seq-find (lambda (a) (equal (plist-get a :name) "query")) args)))
      (expect (length args) :to-equal 2)
      (expect (plist-get q :type) :to-equal "string")
      (expect (plist-get q :description) :to-equal "the query")))

  (it "marks non-required args :optional and defaults missing descriptions"
    (let* ((args (mcp-schema->gptel-args schema))
           (q (seq-find (lambda (a) (equal (plist-get a :name) "query")) args))
           (n (seq-find (lambda (a) (equal (plist-get a :name) "count")) args)))
      (expect (plist-get n :optional) :to-be t)
      (expect (plist-get n :description) :to-equal "")
      (expect (plist-member q :optional) :to-be nil))))

(describe "extract-tool-defs-from-bb"
  (it "parses a collected (def tools [...]) vector"
    (let ((f (make-temp-file "server" nil ".bb")))
      (unwind-protect
          (progn
            (with-temp-file f
              (insert "(ns server)\n"
                      "(def tools\n"
                      "  [{:name \"do-thing\" :description \"Does it\"\n"
                      "    :inputSchema {:type \"object\"\n"
                      "                  :properties {:query {:type \"string\"}}\n"
                      "                  :required [\"query\"]}}\n"
                      "   {:name \"other-thing\"}])\n"))
            (let ((defs (extract-tool-defs-from-bb f)))
              (expect (length defs) :to-equal 2)
              (expect (gethash :name (car defs)) :to-equal "do-thing")
              (expect (hash-table-p (gethash :inputSchema (car defs))) :to-be t)))
        (delete-file f))))

  (it "falls back to individual (def x-tool {...}) forms"
    (let ((f (make-temp-file "server" nil ".bb")))
      (unwind-protect
          (progn
            (with-temp-file f
              (insert "(ns server)\n"
                      "(def ping-tool {:name \"ping\"})\n"
                      "(def pong-tool {:name \"pong\"})\n"))
            (let ((defs (extract-tool-defs-from-bb f)))
              (expect (mapcar (lambda (d) (gethash :name d)) defs)
                      :to-equal '("ping" "pong"))))
        (delete-file f))))

  (it "returns nil for unreadable or non-string commands"
    (expect (extract-tool-defs-from-bb "/nonexistent/server.bb") :to-be nil)
    (expect (extract-tool-defs-from-bb nil) :to-be nil)))

(describe "gptel-improve-text-handle-response"
  (it "ignores reasoning and tool chunks"
    ;; current gptel forwards (reasoning . TEXT) & friends to custom callbacks
    (spy-on 'switch-to-buffer-other-window)
    (expect (gptel-improve-text-handle-response '(reasoning . "") nil) :to-be nil)
    (expect (gptel-improve-text-handle-response '(tool-call . nil) nil) :to-be nil)
    (expect 'switch-to-buffer-other-window :not :to-have-been-called))

  (it "surfaces request failures from the info plist"
    (spy-on 'message)
    (gptel-improve-text-handle-response nil '(:status "429 Too Many Requests"))
    (expect 'message :to-have-been-called-with
            "gptel-improve-text failed: %s" "429 Too Many Requests"))

  (it "reports aborted requests"
    (spy-on 'message)
    (gptel-improve-text-handle-response 'abort '(:status "abort"))
    (expect 'message :to-have-been-called-with
            "gptel-improve-text: request aborted"))

  (it "shows string responses in a markdown side buffer"
    (spy-on 'markdown-mode)
    (spy-on 'switch-to-buffer-other-window)
    (gptel-improve-text-handle-response
     "variant 1\n\n---\n\nvariant 2" '(:data (:model "claude-x")))
    (let ((shown (car (spy-calls-args-for 'switch-to-buffer-other-window 0))))
      (expect (buffer-live-p shown) :to-be t)
      (expect (with-current-buffer shown (buffer-string))
              :to-equal "variant 1\n\n---\n\nvariant 2")
      (kill-buffer shown))))

(describe "gptel-rewrite-ready-banner"
  :var (captured)
  (before-each
    (setq captured nil)
    (defvar gptel-rewrite-actions-map nil)
    (setq gptel-rewrite-actions-map
          (define-keymap
            "<mouse-1>" 'gptel--rewrite-accept
            "C-c C-a" 'gptel--rewrite-accept
            "C-c C-m" 'gptel-rewrite-merge-sentences
            "C-c C-d" 'gptel--rewrite-diff
            "C-c C-e" 'gptel--rewrite-ediff
            "C-c C-r" 'gptel--rewrite-iterate
            "C-c C-k" 'gptel--rewrite-reject)))

  (it "renders every action with its key, skipping mouse bindings"
    (cl-letf (((symbol-function 'gptel--rewrite-update-status)
               (lambda (_ov msg &rest _) (setq captured msg))))
      (gptel-rewrite-ready-banner 'fake-ov)
      ;; C-c C-m canonicalizes to C-c RET (C-m = RET)
      (dolist (chunk '("C-c C-a accept" "C-c RET merge" "C-c C-d diff"
                       "C-c C-e ediff" "C-c C-r iterate" "C-c C-k reject"))
        (expect captured :to-match (regexp-quote chunk)))
      (expect captured :not :to-match "mouse")))

  (it "falls back to ? for unbound actions"
    (setq gptel-rewrite-actions-map (define-keymap))
    (cl-letf (((symbol-function 'gptel--rewrite-update-status)
               (lambda (_ov msg &rest _) (setq captured msg))))
      (gptel-rewrite-ready-banner 'fake-ov)
      (expect captured :to-match (regexp-quote "? accept")))))

(describe "keymap-hint-segment"
  (it "prefers the shortest key, squeezing single-char evil sequences"
    (let ((map (make-sparse-keymap)))
      (keymap-set map "C-c ^ n" 'fake-next)
      (keymap-set map "<normal-state> ] ]" 'fake-next)
      (expect (substring-no-properties
               (keymap-hint-segment map 'fake-next "next" 'success))
              :to-equal "]] next")))

  (it "keeps separator spaces between multi-char tokens"
    (let ((map (make-sparse-keymap)))
      (keymap-set map "C-c ^ n" 'fake-next)
      (expect (substring-no-properties
               (keymap-hint-segment map 'fake-next "next" 'success))
              :to-equal "C-c ^ n next")))

  (it "ignores non-normal evil state bindings"
    (let ((map (make-sparse-keymap)))
      (keymap-set map "<visual-state> x" 'fake-cmd)
      (expect (substring-no-properties
               (keymap-hint-segment map 'fake-cmd "cmd" 'success))
              :to-equal "? cmd"))))

(describe "gptel-rewrite-sentence-split"
  (it "concatenates back to the exact input"
    (dolist (text '("One. Two. Three."
                    "Single line paragraph. With two sentences.\n\nSecond paragraph here. Also two sentences."
                    "A sentence\nhard-wrapped across lines. Another one."
                    "No trailing newline at all"
                    "* Org heading\nBody sentence one. Body sentence two.\n"
                    "Double  spaced.  Sentences  here."))
      (expect (apply #'concat (gptel-rewrite-sentence-split text))
              :to-equal text)))

  (it "keeps inter-sentence whitespace on the preceding chunk"
    (expect (gptel-rewrite-sentence-split "One. Two.")
            :to-equal '("One. " "Two."))))

(describe "gptel-rewrite-sentence-hunks"
  (it "returns nil for identical lists"
    (expect (gptel-rewrite-sentence-hunks '("a" "b") '("a" "b")) :to-be nil))

  (it "reports a change hunk"
    (expect (gptel-rewrite-sentence-hunks '("a" "b" "c") '("a" "x" "c"))
            :to-equal '((2 2 2 2))))

  (it "reports a deletion as an empty new side"
    (expect (gptel-rewrite-sentence-hunks '("a" "b" "c") '("a" "c"))
            :to-equal '((2 2 2 1))))

  (it "reports an insertion as an empty old side"
    (expect (gptel-rewrite-sentence-hunks '("a" "c") '("a" "b" "c"))
            :to-equal '((2 1 2 2)))))

(describe "gptel-rewrite-sentence-conflicts"
  (it "returns nil when only whitespace differs"
    (expect (gptel-rewrite-sentence-conflicts "A.  B." "A. B." "M") :to-be nil))

  (it "conflicts only the changed sentences, equal runs verbatim"
    (let ((merged (gptel-rewrite-sentence-conflicts
                   "One is fine. Twoo bad. Three is fine. Fourr bad."
                   "One is fine. Two bad. Three is fine. Four bad."
                   "M")))
      (expect (substring-no-properties merged) :to-equal
              (concat "One is fine. \n"
                      "<<<<<<< original\nTwoo bad. \n=======\nTwo bad. \n>>>>>>> M\n"
                      "Three is fine. \n"
                      "<<<<<<< original\nFourr bad.\n=======\nFour bad.\n>>>>>>> M\n"))))

  (it "marks every injected newline with the glue property"
    (let ((merged (gptel-rewrite-sentence-conflicts "Aa. Bb." "Aa. Xx." "M")))
      (dotimes (i (length merged))
        (when (eq (aref merged i) ?\n)
          (expect (get-text-property i 'gptel-rewrite-glue merged) :to-be t)))))

  (it "renders deletions with an empty lower side"
    (let ((merged (gptel-rewrite-sentence-conflicts "Keep. Drop me. End." "Keep. End." "M")))
      (expect (substring-no-properties merged)
              :to-match "<<<<<<< original\nDrop me. \n=======\n>>>>>>> M")))

  (it "renders insertions with an empty upper side"
    (let ((merged (gptel-rewrite-sentence-conflicts "Keep. End." "Keep. Add me. End." "M")))
      (expect (substring-no-properties merged)
              :to-match "<<<<<<< original\n=======\nAdd me. \n>>>>>>> M")))

  (it "starts with a marker when the first sentence changed"
    (expect (substring-no-properties
             (gptel-rewrite-sentence-conflicts "Bad start. Fine." "Good start. Fine." "M"))
            :to-match "\\`<<<<<<< original\n")))

(describe "sentence merge smerge roundtrips"
  :var (buf)
  (before-all (require 'smerge-mode))
  (before-each (setq buf (generate-new-buffer " *roundtrip*")))
  (after-each (when (buffer-live-p buf) (kill-buffer buf)))

  (cl-flet ((resolve-all (original improved keeps)
              ;; KEEPS: list of smerge commands, one per conflict in order
              (with-current-buffer buf
                (insert (gptel-rewrite-sentence-conflicts original improved "M"))
                (smerge-mode 1)
                (gptel-rewrite-arm-review-cleanup (point-min) (point-max))
                (dolist (keep keeps)
                  ;; land inside the first remaining conflict; smerge-next
                  ;; would skip a conflict sitting at point
                  (goto-char (point-min))
                  (re-search-forward "^<<<<<<< " nil t)
                  (funcall keep)
                  (run-hooks 'post-command-hook))
                (buffer-string))))

    (it "keep-lower everywhere reproduces the improved text"
      (expect (resolve-all "Aa bad. Bb fine. Cc bad."
                           "Aa good. Bb fine. Cc good."
                           (list #'smerge-keep-lower #'smerge-keep-lower))
              :to-equal "Aa good. Bb fine. Cc good."))

    (it "keep-upper everywhere reproduces the original exactly"
      (expect (resolve-all "Aa bad. Bb fine. Cc bad."
                           "Aa good. Bb fine. Cc good."
                           (list #'smerge-keep-upper #'smerge-keep-upper))
              :to-equal "Aa bad. Bb fine. Cc bad."))

    (it "cherry-picks per sentence"
      (expect (resolve-all "Aa bad. Bb fine. Cc bad."
                           "Aa good. Bb fine. Cc good."
                           (list #'smerge-keep-lower #'smerge-keep-upper))
              :to-equal "Aa good. Bb fine. Cc bad."))

    (it "rejoins a multi-paragraph region preserving paragraph breaks"
      (expect (resolve-all "Para one baad.\n\nPara two fine. Tail baad."
                           "Para one good.\n\nPara two fine. Tail good."
                           (list #'smerge-keep-lower #'smerge-keep-lower))
              :to-equal "Para one good.\n\nPara two fine. Tail good.")))

  (it "leaves the buffer alone until the last conflict resolves"
    (with-current-buffer buf
      (insert (gptel-rewrite-sentence-conflicts
               "Aa bad. Bb fine. Cc bad." "Aa good. Bb fine. Cc good." "M"))
      (smerge-mode 1)
      (gptel-rewrite-arm-review-cleanup (point-min) (point-max))
      (goto-char (point-min))
      (re-search-forward "^<<<<<<< " nil t)
      (smerge-keep-lower)
      (run-hooks 'post-command-hook)
      (expect (buffer-string) :to-match "^<<<<<<< original")
      (expect smerge-mode :to-be-truthy)
      (expect (car-safe header-line-format) :to-equal " REVIEW "))))

(describe "gptel-rewrite-merge-sentences"
  :var (buf ov rejected)
  (before-each
    (setq rejected nil)
    (setq buf (generate-new-buffer " *merge-cmd*")))
  (after-each
    (when (buffer-live-p buf) (kill-buffer buf)))

  (it "replaces the region with conflicts and enables smerge"
    (with-current-buffer buf
      (insert "prefix line\nAa bad. Bb fine.\nsuffix line")
      (setq ov (make-overlay 13 29))    ;the middle line
      (overlay-put ov 'gptel-rewrite "Aa good. Bb fine.")
      (cl-letf (((symbol-function 'gptel--rewrite-reject)
                 (lambda (ovs) (setq rejected ovs))))
        (gptel-rewrite-merge-sentences (list ov)))
      (expect (buffer-string) :to-match "^<<<<<<< original\nAa bad. \n")
      (expect (buffer-string) :to-match "^Aa good. \n")
      (expect smerge-mode :to-be-truthy)
      (expect rejected :to-equal (list ov))
      ;; surrounding lines untouched
      (expect (buffer-string) :to-match "\\`prefix line\n")
      (expect (buffer-string) :to-match "suffix line\\'")))

  (it "survives duplicate and evaporated overlay entries"
    ;; iterating re-pushes the same overlay; the first merge evaporates it
    (with-current-buffer buf
      (insert "Aa bad. Bb fine.")
      (setq ov (make-overlay (point-min) (point-max)))
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'gptel-rewrite "Aa good. Bb fine.")
      (cl-letf (((symbol-function 'gptel--rewrite-reject) #'ignore))
        (gptel-rewrite-merge-sentences (list ov ov)))
      (expect (count-matches "^<<<<<<<" (point-min) (point-max)) :to-equal 1)))

  (it "keeps the original on whitespace-only rewrites"
    (with-current-buffer buf
      (insert "Aa fine.  Bb fine.")
      (setq ov (make-overlay (point-min) (point-max)))
      (overlay-put ov 'gptel-rewrite "Aa fine. Bb fine.")
      (cl-letf (((symbol-function 'gptel--rewrite-reject)
                 (lambda (ovs) (setq rejected ovs))))
        (gptel-rewrite-merge-sentences (list ov)))
      (expect (buffer-string) :to-equal "Aa fine.  Bb fine.")
      (expect rejected :to-equal (list ov))))

  (it "pushes a mid-line leading conflict onto its own line"
    (with-current-buffer buf
      (insert "head: Aa bad. Bb fine.")
      (setq ov (make-overlay 7 (point-max))) ;region starts mid-line
      (overlay-put ov 'gptel-rewrite "Aa good. Bb fine.")
      (cl-letf (((symbol-function 'gptel--rewrite-reject) #'ignore))
        (gptel-rewrite-merge-sentences (list ov)))
      (expect (buffer-string) :to-match "\\`head: \n<<<<<<< original\n")
      ;; resolving rejoins the head fragment
      (goto-char (point-min))
      (re-search-forward "^<<<<<<< " nil t)
      (smerge-keep-lower)
      (run-hooks 'post-command-hook)
      (expect (buffer-string) :to-equal "head: Aa good. Bb fine.")
      ;; the finished review is remembered for a no-selection re-run
      (expect (buffer-substring-no-properties
               (car gptel-improve-text-last-region)
               (cdr gptel-improve-text-last-region))
              :to-equal "Aa good. Bb fine."))))

(describe "gptel-improve-text"
  :var (buf)
  (before-each
    (defvar gptel-model nil)
    (defvar gptel--rewrite-directive nil)
    (setq buf (generate-new-buffer "improve-src")))
  (after-each
    (setq gptel-improve-text-prompt nil)
    (when (buffer-live-p buf) (kill-buffer buf)))

  (it "routes in-place prompts through gptel-rewrite with the prompt as directive"
    (with-current-buffer buf
      (insert "sum txt to fix")
      (let (rewrite-args seen-directive requested)
        (cl-letf (((symbol-function 'gptel--suffix-rewrite)
                   (lambda (&rest args)
                     (setq rewrite-args args
                           seen-directive gptel--rewrite-directive)))
                  ((symbol-function 'gptel-request)
                   (lambda (&rest _) (setq requested t))))
          (let ((transient-mark-mode t))
            (push-mark (point-min) t t)
            (goto-char (point-max))
            (gptel-improve-text)))
        ;; default prompt says "Correct mistakes" -> in-place -> rewrite
        (expect rewrite-args :to-be-truthy)
        (expect seen-directive :to-equal (car gptel-improve-text-prompts-history))
        ;; iterate resends the buffer-local instruction; nil would 400
        (expect (buffer-local-value 'gptel--rewrite-message buf)
                :to-equal (car rewrite-args))
        (expect requested :to-be nil))))

  (it "reuses the last reviewed region when no selection is active"
    (with-current-buffer buf
      (insert "Aa bad. Bb fine.")
      (setq-local gptel-improve-text-last-region
                  (cons (copy-marker 1) (copy-marker 9 t)))
      (let (seen-region)
        (cl-letf (((symbol-function 'gptel--suffix-rewrite)
                   (lambda (&rest _)
                     (setq seen-region (cons (region-beginning) (region-end))))))
          (let ((transient-mark-mode t))
            (deactivate-mark)
            (gptel-improve-text)))
        (expect seen-region :to-equal '(1 . 9)))))

  (it "routes aside prompts through gptel-request onto the side-buffer handler"
    (with-current-buffer buf
      (insert "sum txt to riff on")
      (let (rewrite-called request-args)
        (cl-letf (((symbol-function 'gptel--suffix-rewrite)
                   (lambda (&rest _) (setq rewrite-called t)))
                  ((symbol-function 'gptel-request)
                   (lambda (text &rest args) (setq request-args (cons text args)))))
          (setq gptel-improve-text-prompt
                (nth 2 gptel-improve-text-prompts-history)) ;variants
          (let ((transient-mark-mode t))
            (push-mark (point-min) t t)
            (goto-char (point-max))
            (gptel-improve-text)))
        (expect (car request-args) :to-equal "sum txt to riff on")
        (expect (plist-get (cdr request-args) :system)
                :to-equal (nth 2 gptel-improve-text-prompts-history))
        (expect (plist-get (cdr request-args) :callback)
                :to-be #'gptel-improve-text-handle-response)
        (expect rewrite-called :to-be nil))))

  (it "refuses to run without a selection"
    (with-current-buffer buf
      (insert "no region here")
      (let ((transient-mark-mode t))
        (deactivate-mark)
        (expect (gptel-improve-text) :to-throw 'user-error)))))

(describe "open-gptel"
  :var (buf-old buf-new buf-quick buf-plain switched agent-called)
  (before-each
    ;; simulate gptel loaded: the minor-mode var exists with a nil default
    (defvar gptel-mode nil)
    (setq switched nil agent-called nil)
    (setq buf-old (generate-new-buffer "gptel-2026-01-01"))
    (setq buf-new (generate-new-buffer "gptel-2026-02-02"))
    (setq buf-quick (generate-new-buffer "quick"))
    (setq buf-plain (generate-new-buffer "plain"))
    (dolist (pair `((,buf-old . "/tmp/gptel-2026-01-01.org")
                    (,buf-new . "/tmp/gptel-2026-02-02.org")
                    (,buf-quick . "/tmp/quick.org")))
      (with-current-buffer (car pair)
        (setq-local gptel-mode t)
        (setq buffer-file-name (cdr pair)))))
  (after-each
    (dolist (b (list buf-old buf-new buf-quick buf-plain))
      (with-current-buffer b
        (setq buffer-file-name nil)
        (set-buffer-modified-p nil))
      (kill-buffer b)))

  (it "switches to the lexicographically-latest gptel file buffer"
    (cl-letf (((symbol-function 'display-buffer) (lambda (b &rest _) b))
              ((symbol-function 'switch-to-buffer) (lambda (b) (setq switched b)))
              ((symbol-function 'gptel-agent)
               (lambda () (interactive) (setq agent-called t))))
      (open-gptel)
      (expect switched :to-be buf-new)
      (expect agent-called :to-be nil)))

  (it "skips quick.org and falls through to gptel-agent with a prefix arg"
    (cl-letf (((symbol-function 'display-buffer) (lambda (b &rest _) b))
              ((symbol-function 'switch-to-buffer) (lambda (b) (setq switched b)))
              ((symbol-function 'gptel-agent)
               (lambda () (interactive) (setq agent-called t))))
      (open-gptel t)
      (expect switched :to-be nil)
      (expect agent-called :to-be t)))

  (it "calls gptel-agent when no gptel buffers exist"
    (dolist (b (list buf-old buf-new))
      (with-current-buffer b (setq-local gptel-mode nil)))
    (cl-letf (((symbol-function 'display-buffer) (lambda (b &rest _) b))
              ((symbol-function 'switch-to-buffer) (lambda (b) (setq switched b)))
              ((symbol-function 'gptel-agent)
               (lambda () (interactive) (setq agent-called t))))
      (open-gptel)
      (expect agent-called :to-be t))))

(describe "gptel-inline-visit-last-chat"
  :var (chat popped)
  (before-each
    (defvar gptel-inline--last nil)
    (setq popped nil))

  (it "pops to the remembered session"
    (setq chat (generate-new-buffer "*gptel:proj*"))
    (unwind-protect
        (with-temp-buffer
          (setq-local gptel-inline--last chat)
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (b &rest _) (setq popped b))))
            (gptel-inline-visit-last-chat))
          (expect popped :to-be chat))
      (kill-buffer chat)))

  (it "errors when there is no session for the buffer"
    (with-temp-buffer
      (expect (gptel-inline-visit-last-chat) :to-throw 'user-error)))

  (it "errors when the remembered session is dead"
    (setq chat (generate-new-buffer "doomed"))
    (kill-buffer chat)
    (with-temp-buffer
      (setq-local gptel-inline--last chat)
      (expect (gptel-inline-visit-last-chat) :to-throw 'user-error))))

(describe "gptel-inline-dismiss-overlay-h"
  :var (buf ov)
  (before-each
    (defvar gptel-inline--response-overlay-mode nil)
    (setq buf (generate-new-buffer "dismiss-src"))
    (with-current-buffer buf
      (insert "some buffer text here")
      (setq ov (make-overlay 5 10))
      (overlay-put ov 'gptel-inline '(:marker nil))))
  (after-each
    (when (buffer-live-p buf) (kill-buffer buf)))

  (it "stays inert while the overlay mode is off"
    (with-current-buffer buf
      (expect (gptel-inline-dismiss-overlay-h) :to-be nil)))

  (it "detaches the overlay into the stash and claims the key"
    (with-current-buffer buf
      (setq-local gptel-inline--response-overlay-mode t)
      (cl-letf (((symbol-function 'gptel-inline--response-overlay-at-point)
                 (lambda () ov))
                ((symbol-function 'gptel-inline--response-overlay-mode)
                 (lambda (&rest _))))
        (expect (gptel-inline-dismiss-overlay-h) :to-be t))
      (expect (overlay-buffer ov) :to-be nil)
      (pcase-let ((`(,sov ,beg ,end) gptel-inline-stashed-overlay))
        (expect sov :to-be ov)
        (expect (marker-position beg) :to-equal 5)
        (expect (marker-position end) :to-equal 10))))

  (it "falls through when no overlay is in view"
    (with-current-buffer buf
      (setq-local gptel-inline--response-overlay-mode t)
      (cl-letf (((symbol-function 'gptel-inline--response-overlay-at-point)
                 (lambda () nil)))
        (expect (gptel-inline-dismiss-overlay-h) :to-be nil)))))

(describe "gptel-inline-restore-overlay"
  :var (buf ov rearmed)
  (before-each
    (setq rearmed nil)
    (setq buf (generate-new-buffer "restore-src"))
    (with-current-buffer buf (insert "0123456789abcdefghij")))
  (after-each
    (when (buffer-live-p buf) (kill-buffer buf)))

  (it "re-attaches the stash at its markers and re-arms the UI"
    (with-current-buffer buf
      (setq ov (make-overlay 4 9))
      (delete-overlay ov)
      (setq-local gptel-inline-stashed-overlay
                  (list ov (copy-marker 4) (copy-marker 9)))
      (cl-letf (((symbol-function 'gptel-inline--setup-response-overlay-keymap)
                 (lambda (_) (push 'keymap rearmed)))
                ((symbol-function 'gptel-inline--response-overlay-mode)
                 (lambda (_) (push 'mode rearmed)))
                ((symbol-function 'gptel-inline--response-overlay-render)
                 (lambda (_) (push 'render rearmed)))
                ((symbol-function 'evil-set-jump) (lambda (&rest _)))
                ((symbol-function 'recenter) (lambda (&rest _))))
        (gptel-inline-restore-overlay))
      (expect (overlay-buffer ov) :to-be buf)
      (expect (overlay-start ov) :to-equal 4)
      (expect (overlay-end ov) :to-equal 9)
      (expect gptel-inline-stashed-overlay :to-be nil)
      (expect (memq 'render rearmed) :to-be-truthy)
      (expect (memq 'keymap rearmed) :to-be-truthy)))

  (it "errors when nothing is stashed"
    (with-temp-buffer
      (expect (gptel-inline-restore-overlay) :to-throw 'user-error))))

(describe "gptel-inline-dwim"
  :var (calls)
  (before-each (setq calls nil))

  (it "restores the stash first"
    (with-temp-buffer
      (setq-local gptel-inline-stashed-overlay '(fake-ov nil nil))
      (cl-letf (((symbol-function 'gptel-inline-restore-overlay)
                 (lambda () (push 'restore calls))))
        (gptel-inline-dwim))
      (expect calls :to-equal '(restore))))

  (it "opens the action menu on a visible overlay"
    (with-temp-buffer
      (cl-letf (((symbol-function 'gptel-inline--response-overlay-at-point)
                 (lambda () 'the-ov))
                ((symbol-function 'gptel-inline--response-overlay-dispatch)
                 (lambda (ov) (push (list 'menu ov) calls))))
        (gptel-inline-dwim))
      (expect calls :to-equal '((menu the-ov)))))

  (it "opens the prompt strip otherwise"
    (with-temp-buffer
      (cl-letf (((symbol-function 'gptel-inline--response-overlay-at-point)
                 (lambda () nil))
                ((symbol-function 'gptel-inline)
                 (lambda (&optional ov) (push (list 'inline ov) calls))))
        (gptel-inline-dwim))
      (expect calls :to-equal '((inline nil))))))

(describe "gptel-inline-project-chat-buffer"
  :var (root origin chat clone)
  (before-each
    (defvar gptel-mode nil)
    (setq root (file-name-as-directory (make-temp-file "gi-proj" t)))
    (setq origin (generate-new-buffer "origin"))
    (with-current-buffer origin (setq default-directory root)))
  (after-each
    (dolist (b (list clone chat origin))
      (when (buffer-live-p b) (kill-buffer b)))
    (setq chat nil clone nil)
    (delete-directory root t))

  (it "returns the project's live chat buffer"
    (setq chat (generate-new-buffer "*gptel:proj*"))
    (with-current-buffer chat
      (setq-local gptel-mode t)
      (setq default-directory root))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) 'proj))
              ((symbol-function 'project-root) (lambda (_) root)))
      (expect (gptel-inline-project-chat-buffer origin)
              :to-equal (buffer-name chat))))

  (it "never matches an abandoned indirect prompt clone"
    (setq chat (generate-new-buffer "*gptel:proj*"))
    (with-current-buffer chat (setq-local gptel-mode t)) ;base lives elsewhere
    (setq clone (make-indirect-buffer chat "*gptel:proj*<2>"))
    (with-current-buffer clone
      (setq-local gptel-mode t)
      (setq default-directory root))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) 'proj))
              ((symbol-function 'project-root) (lambda (_) root)))
      (expect (gptel-inline-project-chat-buffer origin)
              :to-equal (list (format "*gptel:%s*"
                                      (file-name-nondirectory (substring root nil -1)))
                              root))))

  (it "proposes a fresh name when the project has no chat"
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) 'proj))
              ((symbol-function 'project-root) (lambda (_) root)))
      (expect (gptel-inline-project-chat-buffer origin)
              :to-equal (list (format "*gptel:%s*"
                                      (file-name-nondirectory (substring root nil -1)))
                              root)))))

(describe "gptel-chat-quadrant-buffer-p"
  :var (chat clone plain)
  (before-each
    (setq chat (generate-new-buffer "*Claude-test*"))
    (setq clone (make-indirect-buffer
                 chat (generate-new-buffer-name (buffer-name chat))))
    (setq plain (generate-new-buffer "irrelevant")))
  (after-each
    (dolist (b (list clone chat plain))
      (when (buffer-live-p b) (kill-buffer b))))

  (it "matches chat buffers by name"
    (expect (gptel-chat-quadrant-buffer-p chat nil) :to-be-truthy)
    (expect (gptel-chat-quadrant-buffer-p (buffer-name chat) nil)
            :to-be-truthy))

  (it "rejects indirect clones sharing the chat's name"
    (expect (gptel-chat-quadrant-buffer-p clone nil) :to-be nil))

  (it "rejects unrelated buffer names"
    (expect (gptel-chat-quadrant-buffer-p plain nil) :to-be nil)))

(describe "gptel-persist-history"
  :var (chat-root start-dir buf)
  (before-each
    (defvar gptel-mode nil)
    (defvar org-default-folder nil)
    (defvar gptel-default-mode nil)
    (setq chat-root (make-temp-file "gptel-chats" t))
    (setq start-dir (file-name-as-directory (make-temp-file "gptel-project" t)))
    (setq buf (generate-new-buffer "*gptel:project*"))
    (with-current-buffer buf
      (setq-local gptel-mode t)
      (setq default-directory start-dir)
      (insert "* prompt\nresponse\n")))
  (after-each
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq buffer-file-name nil)
        (set-buffer-modified-p nil))
      (kill-buffer buf))
    (delete-directory chat-root t)
    (delete-directory start-dir t))

  (it "writes the chat under org-default-folder/gptel"
    (let ((org-default-folder chat-root)
          (gptel-default-mode 'org-mode))
      (with-current-buffer buf (gptel-persist-history 0 0))
      (expect (length (directory-files (expand-file-name "gptel" chat-root)
                                       nil "\\`gptel-.*\\.org\\'"))
              :to-equal 1)))

  (it "preserves default-directory across the save"
    (let ((org-default-folder chat-root)
          (gptel-default-mode 'org-mode))
      (with-current-buffer buf (gptel-persist-history 0 0))
      (expect (buffer-local-value 'default-directory buf) :to-equal start-dir)))

  (it "skips buffers already visiting a file"
    (let ((org-default-folder chat-root)
          (gptel-default-mode 'org-mode))
      (with-current-buffer buf
        (setq buffer-file-name "/tmp/existing.org")
        (gptel-persist-history 0 0))
      (expect (file-directory-p (expand-file-name "gptel" chat-root))
              :to-be nil))))