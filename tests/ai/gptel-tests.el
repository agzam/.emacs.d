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