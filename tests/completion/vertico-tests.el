;;; tests/completion/vertico-tests.el --- orderless dispatcher specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/completion/autoload/vertico.el")

;; orderless isn't loaded here; declare its var special so the let-bindings
;; below are dynamic, not lexical.
(defvar orderless-affix-dispatch-alist)

;; Mirrors the alist set in modules/completion/config.el; the dispatchers
;; read it at call time, so orderless itself isn't needed.
(defvar test-affix-alist
  '((?! . orderless-without-literal)
    (?& . orderless-annotation)
    (?% . char-fold-to-regexp)
    (?` . orderless-initialism)
    (?= . orderless-literal)
    (?^ . orderless-literal-prefix)
    (?~ . orderless-flex)))

(describe "vertico-orderless-dispatch"
  (it "dispatches on a prefix affix"
    (let ((orderless-affix-dispatch-alist test-affix-alist))
      (expect (vertico-orderless-dispatch "=exact" 0 1)
              :to-equal '(orderless-literal . "exact"))))
  (it "dispatches on a suffix affix"
    (let ((orderless-affix-dispatch-alist test-affix-alist))
      (expect (vertico-orderless-dispatch "fuzzy~" 0 1)
              :to-equal '(orderless-flex . "fuzzy"))))
  (it "ignores a lone dispatcher character"
    (let ((orderless-affix-dispatch-alist test-affix-alist))
      (expect (vertico-orderless-dispatch "=" 0 1) :to-be #'ignore)))
  (it "leaves escaped affixes alone"
    (let ((orderless-affix-dispatch-alist test-affix-alist))
      (expect (vertico-orderless-dispatch "literal\\~" 0 1) :to-be nil)))
  (it "does not dispatch plain patterns"
    (let ((orderless-affix-dispatch-alist test-affix-alist))
      (expect (vertico-orderless-dispatch "plain" 0 1) :to-be nil))))

(describe "vertico-orderless-disambiguation-dispatch"
  (it "rewrites $ to skip consult disambiguation suffixes"
    (let ((result (vertico-orderless-disambiguation-dispatch "end$" 0 1)))
      (expect (car result) :to-be 'orderless-regexp)
      (expect (cdr result) :to-match "^end")
      (expect (cdr result) :to-match "\\$$")))
  (it "stays out of the way without a $ suffix"
    (expect (vertico-orderless-disambiguation-dispatch "end" 0 1) :to-be nil)))
