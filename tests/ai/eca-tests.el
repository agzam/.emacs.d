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