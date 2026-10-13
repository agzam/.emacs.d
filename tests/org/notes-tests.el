;;; tests/org/notes-tests.el --- org/autoload/notes.el specs -*- lexical-binding: t; -*-
;; Drawer lint and backlink counts run on built-in org, the count query
;; stubbed; backlinks, forward-links and db-verify need the live vulpea db
;; and stay smoke-only (see MIGRATION coverage map).

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'org)

(load-module-file "modules/org/autoload/notes.el")

;; owned by the org module config, which the batch tier never loads
(defvar org-default-folder)

(defmacro with-org-content (content &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (delay-mode-hooks (org-mode))
     (insert ,content)
     (goto-char (point-min))
     ,@body))

(defun notes-tests--count-overlays ()
  "Return the backlink count overlays of the current buffer."
  (seq-filter (lambda (ov) (overlay-get ov 'backlink-count))
              (overlays-in (point-min) (point-max))))

(describe "backlink-count--superscript"
  (it "converts digits"
    (expect (backlink-count--superscript 12) :to-equal "¹²")
    (expect (backlink-count--superscript 305) :to-equal "³⁰⁵")))

(describe "backlink-count--query"
  (it "loads vulpea before it opens the database"
    (let (loaded)
      (cl-letf (((symbol-function 'require)
                 (lambda (feature &rest _) (push feature loaded)))
                ((symbol-function 'vulpea-db) #'ignore)
                ((symbol-function 'emacsql)
                 (lambda (&rest _) (list (list (if (memq 'vulpea loaded) 4 0))))))
        (expect (backlink-count--query "x") :to-equal 4)))))

(describe "backlink-count-mode"
  (it "marks the title of each heading note that has backlinks"
    (cl-letf (((symbol-function 'backlink-count--query)
               (lambda (id) (if (equal id "x") 3 0))))
      (with-org-content
          "* a\n:PROPERTIES:\n:ID: x\n:END:\n* b\n:PROPERTIES:\n:ID: y\n:END:\n"
        (backlink-count-mode 1)
        (let ((ovs (notes-tests--count-overlays)))
          (expect (length ovs) :to-equal 1)
          (expect (overlay-start (car ovs)) :to-equal 3)
          (expect (overlay-get (car ovs) 'after-string) :to-equal "³")))))

  (it "marks the title of a file note"
    (cl-letf (((symbol-function 'backlink-count--query) (lambda (_) 12)))
      (with-org-content ":PROPERTIES:\n:ID: f\n:END:\n#+title: Note\n"
        (backlink-count-mode 1)
        (let ((ovs (notes-tests--count-overlays)))
          (expect (length ovs) :to-equal 1)
          (expect (overlay-start (car ovs))
                  :to-equal (- (point-max) (length "Note\n")))
          (expect (overlay-get (car ovs) 'after-string) :to-equal "¹²")))))

  (it "stops redrawing on save and clears the counts when turned off"
    (cl-letf (((symbol-function 'backlink-count--query) (lambda (_) 1)))
      (with-org-content "* a\n:PROPERTIES:\n:ID: x\n:END:\n"
        (backlink-count-mode 1)
        (expect (memq #'backlink-count--make-overlays after-save-hook)
                :to-be-truthy)
        (backlink-count-mode -1)
        (expect (memq #'backlink-count--make-overlays after-save-hook)
                :to-be nil)
        (expect (notes-tests--count-overlays) :to-be nil)))))

(describe "org-drawer-lint-before-save-h"
  (it "fixes drawers in files under org-default-folder"
    (let ((org-default-folder "/notes/"))
      (with-org-content "* b\n:PROPERTIES: :ID: x :END:\n"
        (let ((buffer-file-name "/notes/a.org"))
          (org-drawer-lint-before-save-h))
        (expect (org-drawer-lint-check) :to-be nil))))

  (it "leaves org files outside org-default-folder alone"
    (let ((org-default-folder "/notes/"))
      (with-org-content "* b\n:PROPERTIES: :ID: x :END:\n"
        (let ((buffer-file-name "/elsewhere/a.org"))
          (org-drawer-lint-before-save-h))
        (expect (org-drawer-lint-check) :to-equal '((2 . "x")))))))

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
