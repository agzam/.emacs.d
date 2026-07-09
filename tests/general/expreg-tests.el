;;; tests/general/expreg-tests.el --- general/autoload/expreg.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; expreg.el defines a transient prefix at top level.
(require 'transient)
(load-module-file "modules/general/autoload/expreg.el")

;; expreg--line is visual-line based: batch frames are ~10 columns wide, so
;; lines wrap and end-of-visual-line diverges from real-window behavior -
;; display-dependent, smoke-covered (see MIGRATION coverage map).

(describe "expreg--markdown-subtree"
  (it "produces nothing outside markdown-mode"
    (with-temp-buffer
      (insert "# not really markdown\ntext\n")
      (expect (expreg--markdown-subtree) :to-be nil))))

(describe "expreg-transient"
  (it "is defined as a transient prefix"
    (expect (fboundp 'expreg-transient) :to-be-truthy)
    (expect (get 'expreg-transient 'transient--prefix) :to-be-truthy)))
