;;; tests/elisp/let-plist-tests.el --- elisp/autoload/let-plist.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; must load before the `it' bodies are read - they expand the macro
(load-module-file "modules/elisp/autoload/let-plist.el")

(describe "let-plist"
  (it "binds dotted symbols to plist values"
    (expect (let-plist '(:a 1 :b 2) (+ .a .b)) :to-equal 3))

  (it "reaches into nested plists"
    (expect (let-plist '(:user (:name "rob" :id 7)) (list .user.name .user.id))
            :to-equal '("rob" 7)))

  (it "leaves plain dots alone"
    (expect (let-plist '(:a 5) (let ((\.raw 1)) (+ .a \.raw)))
            :to-equal 6))

  (it "returns nil for absent keys"
    (expect (let-plist '(:a 1) .missing) :to-be nil)))
