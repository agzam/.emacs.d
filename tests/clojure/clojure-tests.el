;;; tests/clojure/clojure-tests.el --- clojure/autoload/clojure.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; set-lookup-handlers! comes from the lookup module in real boots.
(load-module-file "modules/lookup/autoload/lookup.el")
(load-module-file "modules/clojure/autoload/clojure.el")

(defvar clojure-align-forms-automatically nil)

(describe "clj-str delimiters (separedit glue)"
  (it "remove strips the per-line quotes of a (str ...) block body"
    (with-temp-buffer
      (insert "\"line one \"\n  \"line two\"")
      (clj-str-remove-delimiters nil)
      (expect (buffer-string) :to-equal "line one \nline two")))

  (it "restore round-trips a plain block back to quoted lines"
    (with-temp-buffer
      (insert "line one\nline two")
      (clj-str-restore-delimiters)
      (expect (buffer-string) :to-equal "\"line one \"\n\"line two\"")))

  (it "restore turns whitespace-only lines into literal newlines"
    (with-temp-buffer
      (insert "top\n   \nbottom")
      (clj-str-restore-delimiters)
      (expect (buffer-string) :to-equal "\"top \"\n\"\\n \"\n\"bottom\""))))

(describe "clojure-unalign"
  ;; NB \s- matches whitespace syntax only; in lisp/clojure newline is
  ;; comment-end syntax (>), so alignment runs collapse but line structure
  ;; is preserved.  Single-line input keeps the assertion unambiguous.
  (it "collapses in-line alignment whitespace and re-indents without lsp"
    (with-temp-buffer
      (lisp-mode)                       ; sexp-aware indent, no clojure-mode dep
      (insert "(:a    1    :bb   22)")
      (clojure-unalign (point-min) (point-max))
      (expect (buffer-string) :to-equal "(:a 1 :bb 22)")))

  (it "prefers lsp indentation in lsp buffers"
    (let (lsp-ranged)
      (cl-letf (((symbol-function 'lsp--indent-lines)
                 (lambda (beg end) (setq lsp-ranged (list beg end)))))
        (with-temp-buffer
          (setq-local lsp-mode t)
          (insert "(:a    1)")
          (clojure-unalign (point-min) (point-max))
          (expect lsp-ranged :to-be-truthy))))))

(describe "add-edn-imenu-regexp-h"
  (it "adds the keyword pattern in .edn buffers only"
    (with-temp-buffer
      (setq-local imenu-generic-expression nil)
      (cl-letf (((symbol-function 'buffer-file-name)
                 (lambda (&optional _) "/tmp/deps.edn")))
        (add-edn-imenu-regexp-h))
      (expect imenu-generic-expression :to-be-truthy))
    (with-temp-buffer
      (setq-local imenu-generic-expression nil)
      (cl-letf (((symbol-function 'buffer-file-name)
                 (lambda (&optional _) "/tmp/core.clj")))
        (add-edn-imenu-regexp-h))
      (expect imenu-generic-expression :to-be nil))))

(describe "clojure-lookup-handlers-h"
  (it "registers init fns for every clojure mode incl. the ts variants"
    (clojure-lookup-handlers-h)
    (unwind-protect
        (dolist (mode '(clojure-mode clojure-ts-mode cider-repl-mode
                        clojure-ts-clojurescript-mode))
          (expect (fboundp (intern (format "lookup--init-%s-handlers-h" mode)))
                  :to-be-truthy))
      (set-lookup-handlers! '(clojure-mode
                              clojurec-mode
                              clojurescript-mode
                              cider-clojure-interaction-mode
                              cider-repl-mode
                              clojure-ts-mode
                              clojure-ts-clojurec-mode
                              clojure-ts-clojurescript-mode)
        nil))))

(describe "clojure-wrap-rich-comment"
  ;; sp fns stubbed with elisp equivalents; lisp-mode gives the syntax.
  (defun clojure-tests--wrap-stubs (thunk)
    (cl-letf (((symbol-function 'sp-backward-up-sexp)
               (lambda (&rest _) (ignore-errors (backward-up-list) t)))
              ((symbol-function 'sp-mark-sexp)
               (lambda (&rest _)
                 (set-mark (point))
                 (forward-sexp)
                 (activate-mark)
                 (exchange-point-and-mark))))
      (funcall thunk)))

  (it "wraps the sexp at point in (comment ...)"
    (clojure-tests--wrap-stubs
     (lambda ()
       (with-temp-buffer
         (lisp-mode)
         (insert "(def x 1)")
         (goto-char 5)
         (clojure-wrap-rich-comment)
         (expect (buffer-string) :to-match "\\`(comment \n(def x 1))\\'")))))

  (it "unwraps when already inside a (comment ...) block"
    (clojure-tests--wrap-stubs
     (lambda ()
       (with-temp-buffer
         (lisp-mode)
         (insert "(comment (def x 1))")
         (goto-char 12)
         (clojure-wrap-rich-comment)
         (expect (string-trim (buffer-string)) :to-equal "(def x 1)"))))))
