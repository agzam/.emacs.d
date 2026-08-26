;;; tests/general/expreg-tests.el --- general/autoload/expreg.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; expreg.el defines a transient prefix at top level and pulls the shared
;; bypass engine (transient-layout-keys) from lisp/transient-bypass.el;
;; the engine itself is spec'd in tests/lisp/transient-bypass-tests.el.
(require 'transient)
(require 'transient-bypass)
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
    (expect (get 'expreg-transient 'transient--prefix) :to-be-truthy))
  ;; rot net for the layout: any reintroduced or dropped suffix command
  ;; (vulpea, markdown, clojure, ...) has to be reflected here first
  (it "ships exactly the expreg + org + markdown + clojure commands"
    (expect (seq-uniq
             (seq-remove
              (lambda (s) (string-prefix-p "transient:" (symbol-name s)))
              (seq-filter #'symbolp
                          (transient-layout-commands
                           (get 'expreg-transient 'transient--layout)))))
            :to-have-same-items-as
            '(expreg-expand expreg-contract org-insert-link vulpea-insert
              expreg-transient--insert-browser-url
              ;; Markdown section restored with the writing module (2026-07).
              markdown-insert-bold markdown-insert-italic markdown-insert-code
              markdown-insert-strike-through markdown-insert-link
              markdown-wrap-code-generic markdown-wrap-collapsible
              markdown-toggle-blockquote
              ;; Clojure section restored with the clojure module (2026-07);
              ;; :if-gated to its mode but always present in the layout.
              clojure-wrap-rich-comment)))
  ;; key rot net: over org's set the Markdown section adds only "; <"
  ;; (its other keys duplicate org's and collapse under set comparison)
  (it "exposes exactly its static keys"
    (expect (transient-layout-keys 'expreg-transient)
            :to-have-same-items-as
            '("v" "V" "u" "C-r" "; *" "; b" "; /" "; i" "; _" "; =" "; `"
              "; +" "C-c l" "C-c L" "C-c i" "; l" "; L" "; q" "; c" "; <"))))
