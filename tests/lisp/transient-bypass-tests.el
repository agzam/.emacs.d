;;; tests/lisp/transient-bypass-tests.el --- lisp/transient-bypass.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'transient)
(require 'transient-bypass)

;; A minimal prefix fixture with two explicit keys: enough to exercise the
;; layout walker and the conflict guard without pulling any module deps.
(transient-define-prefix transient-bypass-tests--fixture ()
  "fixture"
  [("v" "v" ignore)
   ("V" "V" ignore)])

(defun transient-bypass-tests--suffix-prop (suffix prop)
  "PROP from a parsed SUFFIX, across transient layout dialects."
  (if (keywordp (cadr suffix))
      (plist-get (cdr suffix) prop)   ; >= 0.8: (CLASS :key ...)
    (plist-get (nth 2 suffix) prop))) ; 0.7: (LEVEL CLASS (PLIST))

(describe "transient-layout-keys"
  (it "collects every static key from the layout, both dialects"
    (expect (transient-layout-keys 'transient-bypass-tests--fixture)
            :to-have-same-items-as '("v" "V"))))

(describe "transient-bypass-keys"
  (it "passes plain strings through as exiting suffixes"
    (let ((sfx (car (transient-bypass-keys
                     'transient-bypass-tests--fixture '("d")))))
      (expect (transient-bypass-tests--suffix-prop sfx :key) :to-equal "d")
      (expect (transient-bypass-tests--suffix-prop sfx :transient) :to-be nil)
      (expect (functionp (transient-bypass-tests--suffix-prop sfx :command))
              :to-be-truthy)))
  (it "keeps (KEY t) suffixes inside the transient"
    (let ((sfx (car (transient-bypass-keys
                     'transient-bypass-tests--fixture '(("j" t))))))
      (expect (transient-bypass-tests--suffix-prop sfx :transient) :to-be t)))
  (it "honors an explicit command"
    (let ((sfx (car (transient-bypass-keys
                     'transient-bypass-tests--fixture '(("#" nil ignore))))))
      (expect (transient-bypass-tests--suffix-prop sfx :command) :to-be 'ignore)))
  (it "rejects keys that collide with the static layout"
    (expect (transient-bypass-keys 'transient-bypass-tests--fixture '("v"))
            :to-throw 'error)))
