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
  :var (buf)
  (before-each
    (setq buf (generate-new-buffer "improve-src"))
    (with-current-buffer buf (insert "teh text stays")))
  (after-each
    (when (buffer-live-p buf) (kill-buffer buf)))

  (it "ignores reasoning and tool chunks without touching the buffer"
    ;; current gptel forwards (reasoning . TEXT) & friends to custom callbacks
    (gptel-improve-text-handle-response
     '(reasoning . "") '(:data (:model "m")) buf 1 9 "teh text" t)
    (gptel-improve-text-handle-response
     '(tool-call . nil) '(:data (:model "m")) buf 1 9 "teh text" t)
    (expect (with-current-buffer buf (buffer-string))
            :to-equal "teh text stays"))

  (it "surfaces request failures from the info plist"
    (spy-on 'message)
    (gptel-improve-text-handle-response
     nil '(:status "429 Too Many Requests") buf 1 9 "teh text" t)
    (expect 'message :to-have-been-called-with
            "gptel-improve-text failed: %s" "429 Too Many Requests")
    (expect (with-current-buffer buf (buffer-string))
            :to-equal "teh text stays"))

  (it "reports aborted requests"
    (spy-on 'message)
    (gptel-improve-text-handle-response 'abort '(:status "abort") buf 1 9 "teh text" t)
    (expect 'message :to-have-been-called-with
            "gptel-improve-text: request aborted"))

  (it "replaces the region and diffs in-place responses"
    (spy-on 'gptel-improve-text-show-diff)
    (gptel-improve-text-handle-response
     "the text" '(:data (:model "claude-x")) buf 1 9 "teh text" t)
    (expect (with-current-buffer buf (buffer-string))
            :to-equal "the text\n stays")
    (expect 'gptel-improve-text-show-diff :to-have-been-called-with
            "teh text" "the text" "claude-x"))

  (it "shows non-in-place responses in a side buffer"
    (spy-on 'markdown-mode)
    (spy-on 'switch-to-buffer-other-window)
    (gptel-improve-text-handle-response
     "variant 1\n\n---\n\nvariant 2" '(:data (:model "claude-x")) buf 1 9 "teh text" nil)
    (expect (with-current-buffer buf (buffer-string))
            :to-equal "teh text stays")
    (let ((shown (car (spy-calls-args-for 'switch-to-buffer-other-window 0))))
      (expect (buffer-live-p shown) :to-be t)
      (expect (with-current-buffer shown (buffer-string))
              :to-equal "variant 1\n\n---\n\nvariant 2")
      (kill-buffer shown))))

(describe "gptel-improve-text"
  :var (buf captured)
  (before-each
    (defvar gptel-model nil)
    (setq buf (generate-new-buffer "improve-mark"))
    (setq captured nil))
  (after-each
    (setq gptel-improve-text-prompt nil)
    (when (buffer-live-p buf) (kill-buffer buf)))

  (it "aims the replacement with markers, surviving mid-flight edits"
    (with-current-buffer buf
      (insert "prefix sum txt")
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (_text &rest args)
                   (setq captured (plist-get args :callback))))
                ((symbol-function 'gptel-improve-text-show-diff)
                 (lambda (&rest _))))
        (let ((transient-mark-mode t))
          (push-mark 8 t t)
          (goto-char 15)
          (gptel-improve-text))
        (expect captured :to-be-truthy)
        ;; the buffer shifts while the request is in flight
        (goto-char (point-min))
        (insert "XX ")
        (funcall captured "sum text" '(:data (:model "m")))
        (expect (buffer-string) :to-equal "XX prefix sum text\n"))))

  (it "refuses to run without a selection"
    (with-current-buffer buf
      (insert "no region here")
      (let ((transient-mark-mode t))
        (deactivate-mark)
        (expect (gptel-improve-text) :to-throw 'user-error)))))

(describe "gptel-improve-text-show-diff"
  (it "produces a cleaned-up diff in a dedicated buffer"
    (spy-on 'display-buffer)
    (let ((diff-buf (gptel-improve-text-show-diff
                     "teh text\n" "the text\n" "claude-x")))
      (unwind-protect
          (with-current-buffer diff-buf
            (expect (buffer-name) :to-equal "*improve-text diff*")
            (expect (buffer-string) :to-match "teh text")
            (expect (buffer-string) :to-match "the text")
            ;; the "diff ..." command line got scrubbed
            (expect (buffer-string) :not :to-match "\\`diff "))
        (kill-buffer diff-buf))))

  (it "cleans up its scratch buffers"
    (spy-on 'display-buffer)
    (let ((before (buffer-list))
          (diff-buf (gptel-improve-text-show-diff "a\n" "b\n" "m")))
      (unwind-protect
          (expect (seq-filter
                   (lambda (b) (string-match-p "\\* m [12] \\*" (buffer-name b)))
                   (buffer-list))
                  :to-be nil)
        (kill-buffer diff-buf)
        (ignore before)))))

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