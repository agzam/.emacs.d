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