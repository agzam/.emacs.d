;;; tests/ai/eca-tests.el --- ai/autoload/eca.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; Loading registers advice against not-yet-defined eca functions; that is
;; the boot-time behavior too (advice takes effect when eca loads).
(load-module-file "modules/ai/autoload/eca.el")

;; eca is absent here; declare the per-chat selection the seeding advice
;; overrides, which eca-chat.el would otherwise provide.
(defvar eca-chat--selected-agent nil)
(defvar eca-chat--selected-trust nil)

;; config.el's defcustom; the module file only declares it, and a bodiless
;; `defvar' marks a variable special just inside its own file, so binding it
;; from here needs the global declaration.
(defvar eca-archive-dir nil)

(describe "eca-chat-delete-confirm-a"
  (it "lets the delete through once confirmed"
    (spy-on 'yes-or-no-p :and-return-value t)
    (expect (eca-chat-delete-confirm-a) :to-be-truthy))

  ;; :before-while, so a nil here means `eca-chat-delete' never runs
  (it "stops the delete when declined"
    (spy-on 'yes-or-no-p :and-return-value nil)
    (expect (eca-chat-delete-confirm-a) :to-be nil))

  (it "guards the command itself"
    (expect (advice-member-p 'eca-chat-delete-confirm-a 'eca-chat-delete)
            :to-be-truthy)))

(describe "eca-archive--slugify"
  (it "returns empty string for nil or blank input"
    (expect (eca-archive--slugify nil) :to-equal "")
    (expect (eca-archive--slugify "  ") :to-equal ""))

  (it "collapses whitespace and filename-illegal characters into hyphens"
    (expect (eca-archive--slugify "a b/c:d*e?f") :to-equal "a-b-c-d-e-f"))

  (it "trims stray hyphens and dots at both ends"
    (expect (eca-archive--slugify "..hello world--") :to-equal "hello-world"))

  (it "drops text properties"
    (expect (eca-archive--slugify (propertize "hi there" 'face 'bold))
            :to-equal "hi-there"))

  (it "caps length at 72 characters"
    (expect (length (eca-archive--slugify (make-string 100 ?a)))
            :to-equal 72)))

(describe "eca-archive--chat-file-name"
  (it "includes the slugified title when non-empty"
    (expect (eca-archive--chat-file-name "proj" "My: Chat" "abc12345")
            :to-equal "proj__My-Chat_abc12345.md"))

  (it "falls back to project and id when the title slugifies away"
    (expect (eca-archive--chat-file-name "proj" nil "abc12345")
            :to-equal "proj_abc12345.md")
    (expect (eca-archive--chat-file-name "proj" "///" "abc12345")
            :to-equal "proj_abc12345.md")))

;; chat/history items shaped like the server's: a stored text starts with a
;; newline, and tool outputs are a vector of text contents.
(defun eca-tests--item (role content)
  "A chat/history item from ROLE carrying CONTENT."
  (list :chatId "c1" :role role :content content))

(defun eca-tests--prompt (text)
  "The prompt TEXT as the server stores it."
  (eca-tests--item "user" (list :type "text" :contentId "u1" :text (concat "\n" text))))

(defun eca-tests--answer (text)
  "The assistant's answer TEXT."
  (eca-tests--item "assistant" (list :type "text" :text (concat "\n" text))))

(defun eca-tests--call (name &rest props)
  "A finished call of tool NAME; PROPS add to its content or override it."
  (eca-tests--item "assistant" (append props (list :type "toolCalled" :name name :server "eca"))))

(defun eca-tests--output (&rest texts)
  "Tool outputs holding TEXTS."
  (apply #'vector (mapcar (lambda (text) (list :type "text" :text text)) texts)))

(describe "eca-archive--prompt"
  (it "keeps what the user typed and drops what eca stored after it"
    (expect (eca-archive--prompt
             (eca-tests--prompt
              (concat "Fix the parser\nplease\n"
                      "<editor-state description=\"x\">\n<cursor path=\"/a\"/>\n</editor-state>\n"
                      "<additionalContext>\nstyle reminder\n</additionalContext>")))
            :to-equal "Fix the parser\nplease"))

  ;; an attached file can hold anything, its closing tag included
  (it "drops attached contexts whatever they hold"
    (expect (eca-archive--prompt
             (eca-tests--prompt "Look at @a.el\n<contexts description=\"x\">\n</contexts>\n</contexts>"))
            :to-equal "Look at @a.el"))

  (it "is nil for what tools send in the user role"
    (dolist (text '("[eca-ghostel] *eca-ghostel: ci* exited 0 after 18.7s. Read it with (eca-ghostel-output \"*eca-ghostel: ci*\")."
                    "Background job job-1 (`make test`) completed with exit code 0."
                    "The conversation was compacted/summarized, consider this summary:\nwe fixed it"))
      (expect (eca-archive--prompt (eca-tests--prompt text)) :to-be nil)))

  (it "is nil for anything but typed user text"
    (expect (eca-archive--prompt (eca-tests--answer "Done.")) :to-be nil)
    (expect (eca-archive--prompt (eca-tests--call "grep")) :to-be nil)
    (expect (eca-archive--prompt (eca-tests--prompt "<editor-state>\n</editor-state>"))
            :to-be nil)))

(describe "eca-archive--turns"
  (it "puts each item under the prompt before it"
    (let ((turns (eca-archive--turns
                  (list (eca-tests--prompt "one") (eca-tests--answer "a1")
                        (eca-tests--prompt "two") (eca-tests--call "grep")
                        (eca-tests--answer "a2")))))
      (expect (mapcar #'car turns) :to-equal '("one" "two"))
      (expect (mapcar #'length turns) :to-equal '(2 3))))

  ;; a job report arrives between the tool calls of the turn that started it
  (it "keeps a tool's message inside the turn it arrived in"
    (expect (mapcar #'car (eca-archive--turns
                           (list (eca-tests--prompt "one")
                                 (eca-tests--prompt "Background job job-2 (`x`) completed with exit code 0.")
                                 (eca-tests--answer "a1"))))
            :to-equal '("one")))

  (it "leaves out subagent items and items ahead of the first prompt"
    (expect (eca-archive--turns
             (list (eca-tests--answer "orphan")
                   (eca-tests--prompt "one")
                   (append (eca-tests--answer "inner") '(:parentChatId "sub"))
                   (eca-tests--answer "a1")))
            :to-equal (list (list "one" (eca-tests--answer "a1"))))))

(describe "eca-archive--activity"
  (it "counts the calls per tool, the most used first"
    (expect (eca-archive--activity
             (list (eca-tests--call "grep") (eca-tests--call "read_file")
                   (eca-tests--call "read_file")))
            :to-equal "Tools: 3 calls - read_file 2, grep 1"))

  (it "counts neither eca's task list nor a call that is still running"
    (expect (eca-archive--activity
             (list (eca-tests--call "task") (eca-tests--call "grep" :type "toolCallRunning")))
            :to-be nil))

  (it "sums what each file gained and lost, leaving out a failed edit"
    (expect (eca-archive--activity
             (list (eca-tests--call "edit_file" :details '(:type "fileChange" :path "/w/a.el"
                                                           :linesAdded 3 :linesRemoved 1))
                   (eca-tests--call "edit_file" :details '(:type "fileChange" :path "/w/a.el"
                                                           :linesAdded 2 :linesRemoved 0))
                   (eca-tests--call "write_file" :details '(:type "fileChange" :path "/w/b.el"
                                                            :linesAdded 9 :linesRemoved 0))
                   (eca-tests--call "edit_file" :error t :outputs (eca-tests--output "no match")
                                    :details '(:type "fileChange" :path "/w/c.el"
                                               :linesAdded 1 :linesRemoved 1))))
            :to-equal (string-join '("Tools: 4 calls - edit_file 3, write_file 1"
                                     "Changed:"
                                     "- /w/a.el (+5 -1)"
                                     "- /w/b.el (+9 -0)"
                                     "- failed: edit_file: no match")
                                   "\n")))

  ;; git and shell calls lead their output with an exit code and a header
  (it "gives a failure the line that says why it failed"
    (expect (eca-archive--activity
             (list (eca-tests--call "git" :summary "Git commit: fix" :error t
                                    :outputs (eca-tests--output
                                              "Exit code: 128"
                                              "Stderr:\nfatal: Unable to create 'index.lock': File exists."))))
            :to-equal (concat "Tools: 1 call - git 1\n"
                              "- failed: Git commit: fix: fatal: Unable to create 'index.lock': File exists.")))

  (it "names a rejected call"
    (expect (eca-archive--activity
             (list (eca-tests--call "shell_command" :type "toolCallRejected"
                                    :summary "$ docker run x")))
            :to-equal "Tools: 1 call - shell_command 1\n- rejected: $ docker run x")))

(describe "eca-archive--render"
  (it "heads each turn with its prompt, sums up its tools, then gives the answers"
    (expect (eca-archive--render
             (list (eca-tests--prompt "Fix it")
                   (eca-tests--item "assistant" '(:type "reasonStarted" :id "r1"))
                   (eca-tests--item "assistant" '(:type "reasonFinished" :id "r1" :totalTimeMs 0))
                   (eca-tests--answer "Looking.")
                   (eca-tests--item "assistant" '(:type "toolCallRun" :name "grep" :server "eca"))
                   (eca-tests--call "grep")
                   (eca-tests--answer "Fixed.")
                   (eca-tests--prompt "Thanks")))
            :to-equal "# Fix it\n\nTools: 1 call - grep 1\n\nLooking.\n\nFixed.\n\n# Thanks\n"))

  (it "quotes a prompt in full when the heading holds only its first line"
    (expect (eca-archive--render (list (eca-tests--prompt "First line\n\nsecond line")))
            :to-equal "# First line\n\n> First line\n> \n> second line\n"))

  (it "cuts a long first line short in the heading"
    (expect (car (split-string (eca-archive--render
                                (list (eca-tests--prompt (make-string 200 ?a))))
                               "\n"))
            :to-equal (concat "# " (make-string 87 ?a) "..."))))

(describe "eca-chat-buffer-name"
  (it "falls back to Empty chat"
    (expect (eca-chat-buffer-name) :to-equal "eca-chat - Empty chat"))
  (it "embeds the title"
    (expect (eca-chat-buffer-name "Fix parser") :to-equal "eca-chat - Fix parser")))

(describe "eca-archive--read-meta"
  (it "reads the metadata plist from the first line"
    (let ((f (make-temp-file "eca-archive" nil ".md")))
      (unwind-protect
          (progn
            (with-temp-file f
              (insert "<!-- eca: (:id \"abc\" :workspace \"/w/\" :model \"m\") -->\n\nbody\n"))
            (let ((meta (eca-archive--read-meta f)))
              (expect (plist-get meta :id) :to-equal "abc")
              (expect (plist-get meta :workspace) :to-equal "/w/")))
        (delete-file f))))

  (it "returns nil when the file has no metadata line"
    (let ((f (make-temp-file "eca-archive" nil ".md")))
      (unwind-protect
          (progn
            (with-temp-file f (insert "just a markdown file\n"))
            (expect (eca-archive--read-meta f) :to-be nil))
        (delete-file f)))))

(defun eca-tests--archive-dir (&rest files)
  "A throwaway archive dir holding FILES, each (NAME META-PLIST . BODY).
A nil META writes no metadata line.  Files are stamped a minute apart in
the order given, oldest first, so recency is deterministic."
  (let ((dir (file-name-as-directory (make-temp-file "eca-archive" t)))
        (stamp (encode-time '(0 0 12 1 1 2020 nil -1 nil))))
    (pcase-dolist (`(,name ,meta . ,body) files)
      (let ((file (expand-file-name name dir)))
        (with-temp-file file
          (when meta (insert (format "<!-- eca: %S -->\n\n" meta)))
          (insert (or body "body\n")))
        (set-file-times file (time-add stamp (* 60 (cl-position name (mapcar #'car files)
                                                               :test #'equal))))))
    dir))

(describe "eca-archive--parse-name"
  (it "splits project, title and id"
    (expect (eca-archive--parse-name "/a/emacs.d__Fixing-the-thing_1b22794a.md")
            :to-equal '("emacs.d" . "Fixing-the-thing")))

  (it "reads a name whose title slugified away"
    (expect (eca-archive--parse-name "/a/emacs.d_1b22794a.md")
            :to-equal '("emacs.d" . nil)))

  ;; project names carry underscores, so the first `__' has to win
  (it "keeps an underscore inside the project name out of the title"
    (expect (eca-archive--parse-name "/a/my_proj__Some-title_1b22794a.md")
            :to-equal '("my_proj" . "Some-title")))

  (it "falls back to the whole name when nothing matches"
    (expect (eca-archive--parse-name "/a/notes.md") :to-equal '("notes" . nil))))

(describe "eca-archive--entry"
  (it "prefers the title and project the metadata records"
    (let* ((dir (eca-tests--archive-dir
                 '("proj__Slugged-title_abc12345.md"
                   (:id "abc12345-full" :workspace "/w/" :model "m"
                    :project "real-proj" :title "Real Title: with punctuation"))))
           (entry (eca-archive--entry
                   (expand-file-name "proj__Slugged-title_abc12345.md" dir))))
      (expect (plist-get entry :id) :to-equal "abc12345-full")
      (expect (plist-get entry :project) :to-equal "real-proj")
      (expect (plist-get entry :title) :to-equal "Real Title: with punctuation")
      (expect (plist-get entry :workspace) :to-equal "/w/")))

  ;; the 300-odd archives written before the metadata carried them
  (it "falls back to the file name when the metadata predates them"
    (let* ((dir (eca-tests--archive-dir
                 '("proj__Slugged-title_abc12345.md"
                   (:id "abc12345-full" :workspace "/w/" :model "m"))))
           (entry (eca-archive--entry
                   (expand-file-name "proj__Slugged-title_abc12345.md" dir))))
      (expect (plist-get entry :project) :to-equal "proj")
      (expect (plist-get entry :title) :to-equal "Slugged-title")))

  (it "ignores a markdown file that records no chat"
    (let ((dir (eca-tests--archive-dir '("stray.md" nil))))
      (expect (eca-archive--entry (expand-file-name "stray.md" dir)) :to-be nil))))

(describe "eca-archive-entries"
  (it "lists the most recently written chat first"
    (let ((eca-archive-dir
           (eca-tests--archive-dir
            '("proj_aaaaaaaa.md" (:id "aaaaaaaa" :title "oldest"))
            '("proj_bbbbbbbb.md" (:id "bbbbbbbb" :title "middle"))
            '("proj_cccccccc.md" (:id "cccccccc" :title "newest")))))
      (expect (mapcar (lambda (e) (plist-get e :title)) (eca-archive-entries))
              :to-equal '("newest" "middle" "oldest"))))

  (it "skips files that are not archived chats"
    (let ((eca-archive-dir
           (eca-tests--archive-dir '("proj_aaaaaaaa.md" (:id "aaaaaaaa" :title "kept"))
                                   '("README.md" nil))))
      (expect (length (eca-archive-entries)) :to-equal 1)))

  (it "returns nothing when the archive dir does not exist"
    (let ((eca-archive-dir "/definitely/not/here/"))
      (expect (eca-archive-entries) :to-be nil))))

(describe "eca-archive--table"
  (it "keeps the entries in the order given"
    (let ((table (eca-archive--table
                  '((:id "aaaaaaaa1" :project "p" :title "second")
                    (:id "bbbbbbbb1" :project "p" :title "first")))))
      (expect (mapcar (lambda (row) (string-trim (car row))) table)
              :to-equal '("p  second" "p  first"))))

  ;; the point of the columns: every title starts in the same place, whatever
  ;; the project name beside it
  (it "pads the project column so the titles line up"
    (let ((table (eca-archive--table
                  '((:id "a" :project "short" :title "one")
                    (:id "b" :project "a-much-longer-project" :title "two")))))
      (expect (mapcar (lambda (row) (string-match-p "one\\|two" (car row))) table)
              :to-equal '(22 22))))

  (it "gives every row the same width, so annotations cannot go ragged"
    (let* ((table (eca-archive--table
                   '((:id "a" :project "p" :title "a short one")
                     (:id "b" :project "longer-project" :title "a considerably longer title"))))
           (widths (mapcar (lambda (row) (string-width (car row))) table)))
      (expect (car widths) :to-equal (cadr widths))))

  ;; one enormous title would otherwise push every date below it out of line
  (it "truncates a title past the column cap"
    (let* ((long (make-string 100 ?x))
           (label (caar (eca-archive--table (list (list :id "a" :project "p" :title long))))))
      (expect (string-width label) :to-equal 63)
      (expect label :not :to-match long)))

  ;; a repeated label would otherwise make `assoc' hand back the wrong chat
  (it "disambiguates a repeated project and title with the chat id"
    (let ((table (eca-archive--table
                  '((:id "aaaaaaaa-1111" :project "p" :title "same")
                    (:id "bbbbbbbb-2222" :project "p" :title "same")))))
      (expect (mapcar (lambda (row) (string-trim (car row))) table)
              :to-equal '("p  same" "p  same  [bbbbbbbb]"))
      (expect (plist-get (cdr (nth 1 table)) :id) :to-equal "bbbbbbbb-2222")))

  (it "names an untitled chat rather than showing a blank"
    (expect (string-trim (caar (eca-archive--table '((:id "a" :project "p" :title nil)))))
            :to-equal "p  untitled")))

(describe "eca-archive--annotation-function"
  (it "starts every date in the same column, including a uniquified row"
    (let* ((table (eca-archive--table
                   '((:id "aaaaaaaa-1" :project "p" :title "same")
                     (:id "bbbbbbbb-2" :project "p" :title "same")
                     (:id "cccccccc-3" :project "p" :title "other"))))
           (annotate (eca-archive--annotation-function table))
           (columns (mapcar (lambda (row)
                              (+ (string-width (car row))
                                 (- (string-width (funcall annotate (car row)))
                                    (string-width (string-trim (funcall annotate (car row)))))))
                            table)))
      (expect (seq-uniq columns) :to-have-same-items-as (list (car columns)))))

  (it "dates each row from its own archive" ; not from whatever row was first
    (let* ((table (eca-archive--table
                   (list (list :id "a" :project "p" :title "one"
                               :time (encode-time '(0 0 12 1 1 2020 nil -1 nil)))
                         (list :id "b" :project "p" :title "two"
                               :time (encode-time '(0 0 12 2 2 2021 nil -1 nil))))))
           (annotate (eca-archive--annotation-function table)))
      (expect (funcall annotate (car (nth 0 table))) :to-match "2020-01-01")
      (expect (funcall annotate (car (nth 1 table))) :to-match "2021-02-02")))

  (it "ignores a label it does not know"
    (expect (funcall (eca-archive--annotation-function nil) "nope") :to-be nil)))

(describe "eca-archive--completion-table"
  (it "tells completion not to reorder the candidates"
    (let ((metadata (funcall (eca-archive--completion-table '("b" "a"))
                             "" nil 'metadata)))
      (expect (alist-get 'display-sort-function (cdr metadata)) :to-be #'identity)
      (expect (alist-get 'cycle-sort-function (cdr metadata)) :to-be #'identity)))

  (it "still completes over the labels"
    (expect (funcall (eca-archive--completion-table '("proj  one" "proj  two"))
                     "proj" nil t)
            :to-have-same-items-as '("proj  one" "proj  two"))))

;; eca is absent from the batch harness, so the session plumbing the
;; continue path drives is stood in for here.  The e2e scenario asserts the
;; real symbols exist and still take these shapes, so a rename upstream
;; cannot leave these specs passing against a fiction.
(cl-defstruct (eca--session (:constructor eca-tests--session))
  id status workspace-folders chats last-chat-buffer project-name)

(defvar eca--sessions nil)
(defvar eca-chat-history-page-size 50)

(defun eca-vals (plist)
  (cl-loop for (_k v) on plist by #'cddr collect v))
(defun eca-get (plist key) (plist-get plist key #'equal))
(defun eca-info (&rest _))
(defun eca-warn (&rest _))
;; upstream's eca-error only messages - it does not signal, so nothing may
;; use it to abort
(defun eca-error (fmt &rest args) (apply #'message fmt args))
(defun eca-assert-session-running (_session))
(defun eca-api-request-async (&rest _))
(defun eca-create-session (&rest _))
(defun eca-process-start (&rest _))
(defun eca--initialize (&rest _))
(defun eca--handle-message (&rest _))


(defun eca-tests--resume-annotation (model count age)
  "Upstream's resume annotation for MODEL, COUNT and AGE, faces and all."
  (concat (when model (propertize (concat "  " model) 'face 'shadow))
          (when count (propertize (format "  %d msgs" count) 'face 'shadow))
          (when age (propertize (concat "  " age) 'face 'eca-chat-elapsed-time-face))))

(describe "eca-resume--fields"
  ;; model and count share the shadow face, so they arrive as one run
  (it "separates the model from the message count beside it"
    (expect (eca-resume--fields
             (eca-tests--resume-annotation "anthropic/claude-opus-5" 686 "1m ago"))
            :to-equal '("anthropic/claude-opus-5" "686 msgs" "1m ago")))

  (it "reads a model name containing digits"
    (expect (car (eca-resume--fields
                  (eca-tests--resume-annotation "anthropic/claude-opus-4-8" 46 "56d ago")))
            :to-equal "anthropic/claude-opus-4-8"))

  ;; ages read like "3 days ago", so whitespace cannot be the separator
  (it "keeps a multi-word age whole"
    (expect (nth 2 (eca-resume--fields
                    (eca-tests--resume-annotation "m" 1 "3 days ago")))
            :to-equal "3 days ago"))

  (it "tolerates any field being absent"
    (expect (eca-resume--fields (eca-tests--resume-annotation nil 5 nil))
            :to-equal '(nil "5 msgs" nil))
    (expect (eca-resume--fields (eca-tests--resume-annotation "m" nil "1m ago"))
            :to-equal '("m" nil "1m ago"))
    (expect (eca-resume--fields "") :to-equal '(nil nil nil))))

(describe "eca-resume--rows"
  (defun eca-tests--annotate (alist)
    (lambda (label) (cdr (assoc label alist))))

  (it "starts the model column at the same place on every row"
    (let* ((rows (eca-resume--rows
                  '("short" "a considerably longer chat title")
                  (eca-tests--annotate
                   (list (cons "short" (eca-tests--resume-annotation "m1" 1 "1m ago"))
                         (cons "a considerably longer chat title"
                               (eca-tests--resume-annotation "m2" 2 "2m ago"))))))
           (columns (mapcar (lambda (row) (string-match-p "m[12]" (car row))) rows)))
      (expect (car columns) :to-equal (cadr columns))))

  (it "right-aligns the message counts so the digits line up"
    (let* ((rows (eca-resume--rows
                  '("a" "b")
                  (eca-tests--annotate
                   (list (cons "a" (eca-tests--resume-annotation "m" 7 "1m ago"))
                         (cons "b" (eca-tests--resume-annotation "m" 694 "2m ago"))))))
           (ends (mapcar (lambda (row)
                           (+ (string-match-p "msgs" (car row)) 4))
                         rows)))
      (expect (car ends) :to-equal (cadr ends))))

  ;; upstream indexes its chats by the label it built, so that exact string
  ;; has to come back out - a padded one looks up nothing
  (it "keeps the original label as the value behind the padded row"
    (let ((rows (eca-resume--rows
                 '("a chat")
                 (eca-tests--annotate
                  (list (cons "a chat" (eca-tests--resume-annotation "m" 1 "1m ago")))))))
      (expect (cdar rows) :to-equal "a chat")
      (expect (caar rows) :to-match "\\`a chat +m +1 msgs  1m ago\\'"))))

(describe "eca-resume--completing-read"
  (it "returns the label upstream knows, not the row it displayed"
    (let* ((labels '("first chat" "second chat"))
           (collection (lambda (string pred action)
                         (if (eq action 'metadata)
                             `(metadata (annotation-function
                                         . ,(lambda (_) (eca-tests--resume-annotation "m" 3 "1m ago"))))
                           (complete-with-action action labels string pred))))
           ;; the real completing-read, standing in for the user picking row 2
           (read (lambda (_prompt table &rest _)
                   (nth 1 (all-completions "" table)))))
      (expect (eca-resume--completing-read read "Resume: " collection nil t)
              :to-equal "second chat")))

  (it "leaves a collection that annotates nothing alone"
    (let* ((collection '("plain" "candidates"))
           (seen nil)
           (read (lambda (_prompt table &rest _) (setq seen table) "plain")))
      (expect (eca-resume--completing-read read "P: " collection nil t) :to-equal "plain")
      (expect seen :to-be collection))))

(describe "eca-archive--session-for-root"
  (it "matches a session whose root is recorded unexpanded"
    (let ((eca--sessions (list :s (eca-tests--session
                                   :workspace-folders '("~/.emacs.d/")))))
      (expect (eca-archive--session-for-root (expand-file-name "~/.emacs.d"))
              :not :to-be nil)))

  (it "matches whichever of several roots the chat was archived under"
    (let ((eca--sessions (list :s (eca-tests--session
                                   :workspace-folders '("/a/" "/b/")))))
      (expect (eca-archive--session-for-root "/b") :not :to-be nil)))

  (it "returns nothing for an unknown root, or none at all"
    (let ((eca--sessions (list :s (eca-tests--session :workspace-folders '("/a/")))))
      (expect (eca-archive--session-for-root "/elsewhere/") :to-be nil)
      (expect (eca-archive--session-for-root nil) :to-be nil)
      (expect (eca-archive--session-for-root "") :to-be nil))))

(describe "eca-archive--when-started"
  (it "runs the callback at once for a started session"
    (let ((got nil)
          (session (eca-tests--session :status 'started)))
      (eca-archive--when-started session (lambda (s) (setq got s)))
      (expect got :to-be session)))

  ;; a dead process would otherwise be polled until the deadline
  (it "gives up immediately when the session stopped"
    (expect (eca-archive--when-started (eca-tests--session :status 'stopped)
                                       #'ignore)
            :to-throw 'error))

  (it "gives up once the deadline passes rather than rescheduling forever"
    (expect (eca-archive--when-started (eca-tests--session :status 'starting)
                                       #'ignore
                                       (time-add nil -1))
            :to-throw 'error))

  (it "keeps waiting while the session is still starting"
    (spy-on 'run-at-time)
    (eca-archive--when-started (eca-tests--session :status 'starting) #'ignore)
    (expect 'run-at-time :to-have-been-called)))

(describe "eca-archive--ensure-session"
  (it "reuses a running session for the recorded workspace"
    (let* ((session (eca-tests--session :status 'started
                                        :workspace-folders '("/w/")))
           (eca--sessions (list :s session))
           (got nil))
      (spy-on 'eca-create-session)
      (eca-archive--ensure-session "/w/" (lambda (s) (setq got s)))
      (expect got :to-be session)
      (expect 'eca-create-session :not :to-have-been-called)))

  ;; resuming normally happens after a restart, with nothing running yet
  (it "starts a session for the workspace when none is running"
    (let* ((dir (file-name-as-directory (make-temp-file "eca-ws" t)))
           (eca--sessions nil)
           (started (eca-tests--session :status 'started)))
      (unwind-protect
          (progn
            (spy-on 'eca-create-session :and-return-value started)
            (spy-on 'eca-process-start)
            (eca-archive--ensure-session dir #'ignore)
            (expect 'eca-create-session :to-have-been-called-with (list dir))
            (expect 'eca-process-start :to-have-been-called))
        (delete-directory dir t))))

  (it "refuses a workspace that is no longer a directory"
    (let ((eca--sessions nil))
      (spy-on 'eca-create-session)
      (expect (eca-archive--ensure-session "/gone/" #'ignore) :to-throw 'error)
      (expect (eca-archive--ensure-session nil #'ignore) :to-throw 'error)
      (expect 'eca-create-session :not :to-have-been-called))))

(describe "eca-archive--continue-in"
  (before-each
    (spy-on 'eca-archive--attach-chat)
    (spy-on 'eca-archive--gone))

  (defun eca-tests--continue (open-res &optional buffer error?)
    "Drive the chat/open callback with OPEN-RES, BUFFER registered for the chat."
    (let* ((session (eca-tests--session
                     :status 'started
                     :chats (when buffer (list "chat-1" buffer))))
           (entry '(:id "chat-1" :file "/a.md" :title "t")))
      ;; session comes first, the keywords after it
      (spy-on 'eca-api-request-async
              :and-call-fake
              (lambda (_session &rest args)
                (funcall (plist-get args (if error? :error-callback :success-callback))
                         open-res)))
      (eca-archive--continue-in session entry)))

  (it "reopens the chat itself when the server still has it"
    (with-temp-buffer
      (eca-tests--continue '(:found? t) (current-buffer))
      (expect 'eca-archive--attach-chat :to-have-been-called)
      (expect 'eca-archive--gone :not :to-have-been-called)))

  ;; a new chat fed the transcript is a different conversation, so a chat
  ;; the server no longer holds has to fail rather than be approximated
  (it "reports a chat the server no longer has, opening nothing"
    (eca-tests--continue '(:found? nil))
    (expect 'eca-archive--gone :to-have-been-called)
    (expect 'eca-archive--attach-chat :not :to-have-been-called))

  ;; found, but nothing was registered - attaching would surface a dead buffer
  (it "reports when the server claims the chat but no buffer appeared"
    (eca-tests--continue '(:found? t))
    (expect 'eca-archive--gone :to-have-been-called)
    (expect 'eca-archive--attach-chat :not :to-have-been-called))

  (it "reports when the request itself fails"
    (eca-tests--continue '(:code -1) nil t)
    (expect 'eca-archive--gone :to-have-been-called)
    (expect 'eca-archive--attach-chat :not :to-have-been-called)))

(describe "eca-archive--gone"
  (it "names the chat and the archive that outlived it"
    (let (said)
      (spy-on 'eca-error :and-call-fake (lambda (fmt &rest args)
                                          (setq said (apply #'format fmt args))))
      (eca-archive--gone '(:id "abcdef1234" :file "/a/chat.md"))
      (expect said :to-match "abcdef12")
      (expect said :to-match "chat.md")))

  ;; it runs inside the process filter, where a signal only wraps the message
  ;; in `error in process filter' - and nothing downstream may depend on it
  (it "reports without signalling"
    (spy-on 'eca-error)
    (expect (eca-archive--gone '(:id "abcdef1234" :file "/a/chat.md"))
            :not :to-throw)))

;; The chat buffer state `eca-archive-chat' reads, stood in like the session
;; above and checked against the real eca by the same e2e scenario.
(define-derived-mode eca-chat-mode fundamental-mode "ECA")
(defvar-local eca-chat--id nil)
(defvar-local eca-chat--title nil)
(defvar-local eca-chat--custom-title nil)
(defvar-local eca-chat--selected-model nil)
(defun eca-chat-title () (or eca-chat--custom-title eca-chat--title))

(describe "eca-archive-chat"
  :var (dir)
  (before-each
    (setq dir (file-name-as-directory (make-temp-file "eca-archive" t))))
  (after-each
    (delete-directory dir t))

  (defun eca-tests--archive (reply &optional id)
    "Archive chat ID, titled \"Fix it\", whose `chat/history' answers REPLY.
REPLY :fail fails the request itself."
    (with-temp-buffer
      (eca-chat-mode)
      (setq-local eca-chat--id (or id "abcdef1234")
                  eca-chat--title "Fix it"
                  eca-chat--selected-model "m")
      (let ((eca-archive-dir dir)
            (eca--sessions (list :s (eca-tests--session
                                     :project-name ".proj" :workspace-folders '("/w/")
                                     :chats (list eca-chat--id (current-buffer))))))
        (spy-on 'eca-api-request-async :and-call-fake
                (lambda (_session &rest args)
                  (if (eq reply :fail)
                      (funcall (plist-get args :error-callback) "timeout")
                    (funcall (plist-get args :success-callback) reply))))
        (eca-archive-chat))))

  (defun eca-tests--read (file)
    "FILE's contents."
    (with-temp-buffer (insert-file-contents file) (buffer-string)))

  (it "writes the chat's history under the metadata line"
    (let ((file (eca-tests--archive
                 (list :contents (vector (eca-tests--prompt "Fix it")
                                         (eca-tests--answer "Done."))))))
      (expect (file-name-nondirectory file) :to-equal "proj__Fix-it_abcdef12.md")
      (expect (eca-tests--read file)
              :to-equal (concat "<!-- eca: (:id \"abcdef1234\" :workspace \"/w/\" :model \"m\""
                                " :project \"proj\" :title \"Fix it\") -->\n\n"
                                "# Fix it\n\nDone.\n"))))

  ;; a resumed chat's buffer holds only the newest page of its history
  (it "asks the server for the whole history, not a page of it"
    (eca-tests--archive (list :contents []))
    (expect (plist-get (cdr (spy-calls-args-for 'eca-api-request-async 0)) :params)
            :to-equal '(:chatId "abcdef1234")))

  (it "drops the file the chat had under an earlier title"
    (let ((old (expand-file-name "proj_abcdef12.md" dir)))
      (with-temp-file old (insert "old"))
      (eca-tests--archive (list :contents []))
      (expect (file-exists-p old) :to-be nil)))

  (it "leaves the archive as it was when the request fails"
    (let ((file (expand-file-name "proj__Fix-it_abcdef12.md" dir)))
      (with-temp-file file (insert "earlier turns"))
      (eca-tests--archive :fail)
      (expect (eca-tests--read file) :to-equal "earlier turns")))

  (it "leaves the archive as it was when the server cannot find the chat"
    (let ((file (expand-file-name "proj__Fix-it_abcdef12.md" dir)))
      (with-temp-file file (insert "earlier turns"))
      (eca-tests--archive (list :error '(:code "chat_not_found" :message "gone")))
      (expect (eca-tests--read file) :to-equal "earlier turns")))

  (it "archives neither a subagent chat nor a buffer that is no chat"
    (expect (eca-tests--archive (list :contents []) "subagent-1") :to-be nil)
    (expect (with-temp-buffer (eca-archive-chat)) :to-be nil)
    (expect 'eca-api-request-async :not :to-have-been-called)))

(describe "eca-compact-modeline-icons-h"
  ;; Regression: ECA's trust/elapsed mode-line segments use color emoji
  ;; taller than the text font.  With doom-modeline's height bar pinned to
  ;; 1px nothing clamps the line, so eca windows floated to ~33px while
  ;; ordinary windows stayed 26px.  This hook caps those faces per-buffer.
  (it "adds a 0.7 :height remap for each emoji-bearing face"
    (with-temp-buffer
      (eca-compact-modeline-icons-h)
      (dolist (face '(eca-chat-trust-on-face
                      eca-chat-trust-off-face
                      eca-chat-elapsed-time-face))
        (let ((entry (assq face face-remapping-alist)))
          (expect entry :not :to-be nil)
          (expect (seq-some (lambda (s)
                              (and (consp s) (equal (plist-get s :height) 0.7)))
                            (cdr entry))
                  :to-be-truthy)))))

  (it "is idempotent - repeat runs never stack remaps (the hook fires twice)"
    (with-temp-buffer
      (dotimes (_ 3) (eca-compact-modeline-icons-h))
      (dolist (face '(eca-chat-trust-on-face
                      eca-chat-trust-off-face
                      eca-chat-elapsed-time-face))
        (let* ((entry (assq face face-remapping-alist))
               (heights (seq-filter (lambda (s)
                                      (and (consp s) (plist-member s :height)))
                                    (cdr entry))))
          (expect (length heights) :to-equal 1)))))

  (it "keeps the remap buffer-local so minibuffer/other buffers are untouched"
    (with-temp-buffer
      (eca-compact-modeline-icons-h)
      (with-temp-buffer
        (expect (assq 'eca-chat-trust-on-face face-remapping-alist) :to-be nil)))))

(describe "eca-chat-seed-code-and-trust-a"
  ;; Every new chat copies its selection from the session defaults, and a
  ;; per-chat switch writes back into them - so one plan chat, or resuming
  ;; an untrusted one, used to decide how every later chat starts.
  (it "overrides the selection the new chat just copied"
    (with-temp-buffer
      (setq-local eca-chat--selected-agent "plan"
                  eca-chat--selected-trust nil)
      (eca-chat-seed-code-and-trust-a nil)
      (expect eca-chat--selected-agent :to-equal "code")
      (expect eca-chat--selected-trust :to-be t)))

  (it "stays buffer-local, so a chat switched to plan keeps it"
    (with-temp-buffer
      (eca-chat-seed-code-and-trust-a nil)
      (with-temp-buffer
        (expect (local-variable-p 'eca-chat--selected-agent) :to-be nil))))

  (it "hooks the seeding call itself, so nothing else has to run first"
    (expect (advice-member-p 'eca-chat-seed-code-and-trust-a
                             'eca-chat--initialize-selection-state)
            :to-be-truthy)))


;; The side window and the command that pops it up both come from eca.
(defvar eca-workspaces-buffer-name "*eca-workspaces*")
(defun eca-workspaces ())

(describe "eca-toggle-workspaces"
  ;; Real windows here, not a `with-temp-buffer' replica: which window holds
  ;; the point is the whole decision.
  (before-each
    (spy-on 'eca-workspaces))

  (after-each
    (when-let* ((buf (get-buffer eca-workspaces-buffer-name)))
      (kill-buffer buf)))

  (it "pops up the side window when it is not shown"
    (eca-toggle-workspaces)
    (expect 'eca-workspaces :to-have-been-called))

  (it "focuses the window when the point is elsewhere"
    (save-window-excursion
      (let* ((other (selected-window))
             (win (split-window other)))
        (set-window-buffer win (get-buffer-create eca-workspaces-buffer-name))
        (select-window other)
        (eca-toggle-workspaces)
        (expect (selected-window) :to-be win)
        (expect (window-live-p win) :to-be t)
        (expect 'eca-workspaces :not :to-have-been-called))))

  (it "dismisses the window when the point is in it"
    (save-window-excursion
      (let* ((win (split-window)))
        (set-window-buffer win (get-buffer-create eca-workspaces-buffer-name))
        (select-window win)
        (eca-toggle-workspaces)
        (expect (window-live-p win) :to-be nil)
        (expect 'eca-workspaces :not :to-have-been-called))))

  ;; the leader key can come before anything has loaded eca-workspaces.el,
  ;; which is what defines the buffer name
  (it "opens the dashboard before eca-workspaces.el has loaded"
    (let ((name eca-workspaces-buffer-name))
      (makunbound 'eca-workspaces-buffer-name)
      (unwind-protect
          (progn (eca-toggle-workspaces)
                 (expect 'eca-workspaces :to-have-been-called))
        (setq eca-workspaces-buffer-name name)))))

(describe "eca-workspaces-drop-button-local-map-a"
  (it "strips local-map and keeps the keymap that runs the button"
    (with-temp-buffer
      (let ((button-map (make-sparse-keymap)))
        (insert (propertize "▼ project" 'local-map button-map 'keymap button-map)
                "  1 chat\n")
        ;; the dashboard is a `special-mode' buffer
        (setq buffer-read-only t)
        (set-buffer-modified-p nil)
        (eca-workspaces-drop-button-local-map-a)
        (expect (text-property-not-all (point-min) (point-max) 'local-map nil)
                :to-be nil)
        (expect (get-text-property (point-min) 'keymap) :to-be button-map)
        (expect (buffer-modified-p) :to-be nil))))

  (it "runs after every render of the dashboard"
    (expect (advice-member-p 'eca-workspaces-drop-button-local-map-a
                             'eca-workspaces--render)
            :to-be-truthy)))


;; eca-ghostel.el is not part of this config, so its `eca-ghostel-list'
;; entries are written out here; the e2e scenario presses the real key.
(defun eca-tests--run (buffer origin &rest props)
  "An `eca-ghostel-list' entry for BUFFER, started from the chat named ORIGIN.
PROPS override the defaults."
  (append props
          (list :buffer buffer :status "exited 0 after 1.0s" :command "make test"
                :origin origin :started "12:00:00" :log nil)))

(defun eca-tests--fake-consult (pick)
  "Make `consult--read' answer with its PICK-th candidate.
Return a function giving what the read saw: its candidates, its options
and `display-buffer-overriding-action'."
  (let (seen)
    (spy-on 'consult--buffer-preview :and-return-value 'buffer-preview)
    (spy-on 'consult--read :and-call-fake
            (lambda (candidates &rest options)
              (setq seen (list candidates options display-buffer-overriding-action))
              (nth pick candidates)))
    (lambda () seen)))

(describe "eca-chat-ghostel-buffers"
  ;; the stand-in picks the second row, so a picker that dropped the
  ;; order, or offered another chat's run, hands back a different buffer
  (it "offers only the runs this chat started, in the order eca-ghostel lists them"
    (with-temp-buffer
      (let ((chat (buffer-name)))
        (spy-on 'eca-ghostel-list :and-return-value
                (list (eca-tests--run "*eca-ghostel: zeta*" chat)
                      (eca-tests--run "*eca-ghostel: theirs*" "eca-chat - other")
                      (eca-tests--run "*eca-ghostel: alpha*" chat)
                      (eca-tests--run "*eca-ghostel: mid*" chat)))
        (spy-on 'eca-ghostel-visit)
        (let ((seen (eca-tests--fake-consult 1)))
          (with-fake-feature 'consult
            (eca-chat-ghostel-buffers))
          (expect (car (funcall seen))
                  :to-equal '("*eca-ghostel: zeta*" "*eca-ghostel: alpha*"
                              "*eca-ghostel: mid*"))
          (expect 'eca-ghostel-visit :to-have-been-called-with "*eca-ghostel: alpha*")))))

  (it "keeps that order, annotates each row and previews it as the selection moves"
    (with-temp-buffer
      (spy-on 'eca-ghostel-list :and-return-value
              (list (eca-tests--run "*eca-ghostel: a*" (buffer-name))))
      (spy-on 'eca-ghostel-visit)
      (let ((seen (eca-tests--fake-consult 0)))
        (with-fake-feature 'consult
          (eca-chat-ghostel-buffers))
        (let ((options (nth 1 (funcall seen))))
          (expect (plist-member options :sort) :to-be-truthy)
          (expect (plist-get options :sort) :to-be nil)
          (expect (funcall (plist-get options :annotate) "*eca-ghostel: a*")
                  :to-match "exited 0 after 1\\.0s +12:00:00  make test\\'")
          ;; `any' previews with no key to press
          (expect (plist-get options :preview-key) :to-be 'any)
          (expect (plist-get options :state) :to-be 'buffer-preview)))))

  ;; the read itself runs unoverridden, or vertico-buffer would land in the
  ;; window the previews need
  (it "shows the picked buffer in the window that previewed it"
    (with-temp-buffer
      (let (action)
        (spy-on 'eca-ghostel-list :and-return-value
                (list (eca-tests--run "*eca-ghostel: a*" (buffer-name))))
        (spy-on 'eca-ghostel-visit :and-call-fake
                (lambda (_name) (setq action display-buffer-overriding-action)))
        (let ((seen (eca-tests--fake-consult 0)))
          (with-fake-feature 'consult
            (eca-chat-ghostel-buffers))
          (expect action :to-equal '(display-buffer-same-window))
          (expect (nth 2 (funcall seen))
                  :to-equal (default-value 'display-buffer-overriding-action))))))

  (it "says so when the chat started no run"
    (with-temp-buffer
      (spy-on 'eca-ghostel-list :and-return-value
              (list (eca-tests--run "*eca-ghostel: theirs*" "eca-chat - other")))
      (spy-on 'consult--read)
      (expect (eca-chat-ghostel-buffers) :to-throw 'user-error)
      (expect 'consult--read :not :to-have-been-called)))

  ;; a fresh Emacs, before the elisp-eval MCP has loaded eca-ghostel.el
  (it "says so before eca-ghostel has loaded"
    (expect (fboundp 'eca-ghostel-list) :to-be nil)
    (with-temp-buffer
      (expect (eca-chat-ghostel-buffers) :to-throw 'user-error))))

(describe "eca-chat-ghostel--annotations"
  (it "starts every status and every start time in the same column"
    (let* ((rows (eca-chat-ghostel--annotations
                  (list (eca-tests--run "*eca-ghostel: a-long-label*" "c"
                                        :status "running 3.0s" :started "12:00:01")
                        (eca-tests--run "*eca-ghostel: b*" "c"
                                        :status "exited 1 after 120.5s" :started "11:59:00"))))
           (columns (lambda (regexp)
                      (seq-uniq (mapcar (lambda (row)
                                          (string-match-p regexp (concat (car row) (cdr row))))
                                        rows)))))
      (expect (length (funcall columns "running\\|exited")) :to-equal 1)
      (expect (length (funcall columns "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]")) :to-equal 1)))

  (it "shows each run's own status, start time and command"
    (let ((rows (eca-chat-ghostel--annotations
                 (list (eca-tests--run "*eca-ghostel: a*" "c" :status "running 3.0s"
                                       :started "12:00:01" :command "bb test")
                       (eca-tests--run "*eca-ghostel: b*" "c" :status "exited 2 after 9.0s"
                                       :started "11:00:00" :command "bb e2e")))))
      (expect (cdr (assoc "*eca-ghostel: a*" rows))
              :to-match "running 3\\.0s +12:00:01  bb test\\'")
      (expect (cdr (assoc "*eca-ghostel: b*" rows))
              :to-match "exited 2 after 9\\.0s +11:00:00  bb e2e\\'")))

  ;; vertico prints an annotation as is, so a newline in it splits the row
  (it "puts a multi-line command on one line"
    (let ((annotation (cdar (eca-chat-ghostel--annotations
                             (list (eca-tests--run "*eca-ghostel: a*" "c"
                                                   :command "set -x\n  cd /tmp &&\n\tmake"))))))
      (expect annotation :to-match "set -x cd /tmp && make\\'")
      (expect annotation :not :to-match "\n"))))