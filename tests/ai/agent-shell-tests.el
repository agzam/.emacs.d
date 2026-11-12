;;; tests/ai/agent-shell-tests.el --- ai/autoload/agent-shell.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/ai/autoload/agent-shell.el")

(defun agent-shell-tests--subdir (cwd subdir)
  "What `agent-shell-data-subdir' returns for SUBDIR in a shell started in CWD."
  ;; agent-shell is absent here
  (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () cwd)))
    (agent-shell-data-subdir subdir)))

(describe "agent-shell-data-subdir"
  (it "puts a shell's data under the data dir, in a directory named after the project"
    (expect (agent-shell-tests--subdir "/Users/me/work/repo/" "transcripts")
            :to-equal (file-name-concat doom-data-dir "agent-shell"
                                        "Users-me-work-repo" "transcripts")))

  (it "gives each project its own directory, trailing slash or not"
    (expect (agent-shell-tests--subdir "/w/a/" "transcripts")
            :not :to-equal (agent-shell-tests--subdir "/w/b/" "transcripts"))
    (expect (agent-shell-tests--subdir "/w/a" "transcripts")
            :to-equal (agent-shell-tests--subdir "/w/a/" "transcripts"))))
