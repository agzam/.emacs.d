;;; tests/org/org-roam-helpers-tests.el --- org/autoload/org-roam-helpers.el specs -*- lexical-binding: t; -*-
;; Drawer lint + superscripts run on built-in org; everything vulpea/db-bound
;; (backlinks, forward-links, count overlays, refile, db-verify) is smoke-only
;; (see MIGRATION coverage map).

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'org)

(load-module-file "modules/org/autoload/org-roam-helpers.el")

(defmacro with-org-content (content &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (delay-mode-hooks (org-mode))
     (insert ,content)
     (goto-char (point-min))
     ,@body))

(describe "org-roam--number-to-superscript"
  (it "converts digits"
    (expect (org-roam--number-to-superscript 12) :to-equal "¹²")
    (expect (org-roam--number-to-superscript 305) :to-equal "³⁰⁵")))

(describe "org-drawer-lint--regex-ids"
  (it "finds normal and collapsed IDs with line numbers"
    (with-org-content
        "* a\n:PROPERTIES:\n:ID: id-one\n:END:\n* b\n:PROPERTIES: :ID: id-two :END:\n"
      (expect (org-drawer-lint--regex-ids)
              :to-equal '((3 . "id-one") (6 . "id-two")))))
  (it "skips IDs inside blocks"
    (with-org-content
        "#+begin_src org\n:ID: fake\n#+end_src\n"
      (expect (org-drawer-lint--regex-ids) :to-be nil))))

(describe "org-drawer-lint-check"
  (it "flags collapsed drawers the parser misses"
    (with-org-content "* b\n:PROPERTIES: :ID: id-two :END:\n"
      (expect (org-drawer-lint-check) :to-equal '((2 . "id-two")))))
  (it "passes well-formed drawers"
    (with-org-content "* a\n:PROPERTIES:\n:ID: id-one\n:END:\n"
      (expect (org-drawer-lint-check) :to-be nil))))

(describe "org-drawer-lint-fix"
  (it "expands collapsed single-line drawers"
    (with-org-content "* b\n:PROPERTIES: :ID: id-two :CREATED: [2026-01-01] :END:\n"
      (expect (org-drawer-lint-fix) :to-be-greater-than 0)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal
              "* b\n:PROPERTIES:\n:ID: id-two\n:CREATED: [2026-01-01]\n:END:\n")
      (expect (org-drawer-lint-check) :to-be nil)))
  (it "removes blank lines between heading and drawer"
    (with-org-content "* b\n\n\n:PROPERTIES:\n:ID: x\n:END:\n"
      (org-drawer-lint-fix)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "* b\n:PROPERTIES:\n:ID: x\n:END:\n")))
  (it "removes indentation from drawer lines"
    (with-org-content "* b\n  :PROPERTIES:\n  :ID: x\n  :END:\n"
      (org-drawer-lint-fix)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "* b\n:PROPERTIES:\n:ID: x\n:END:\n")))
  (it "leaves clean buffers alone"
    (with-org-content "* a\n:PROPERTIES:\n:ID: id-one\n:END:\nbody\n"
      (expect (org-drawer-lint-fix) :to-equal 0))))

(describe "outline-collapsed?"
  (it "returns nil at top level of a fully visible buffer"
    (with-org-content "* a\nbody\n"
      (search-forward "body")
      (expect (outline-collapsed?) :to-be nil))))
