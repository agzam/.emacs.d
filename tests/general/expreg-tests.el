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
    (expect (get 'expreg-transient 'transient--prefix) :to-be-truthy))
  ;; the regression net for rot inside the layout: any reintroduced
  ;; suffix command (vulpea, markdown, ...) must show up here first
  (it "ships exactly the expreg + org commands in the static layout"
    (expect (seq-uniq
             (seq-remove
              (lambda (s) (string-prefix-p "transient:" (symbol-name s)))
              (seq-filter #'symbolp
                          (transient-layout-commands
                           (get 'expreg-transient 'transient--layout)))))
            :to-have-same-items-as
            '(expreg-expand expreg-contract org-insert-link vulpea-insert))))

(describe "transient-layout-keys"
  (it "collects every static key from the real layout"
    (expect (transient-layout-keys 'expreg-transient)
            :to-have-same-items-as
            '("v" "V" "u" "C-r" "; *" "; b" "; /" "; i" "; _" "; =" "; `"
              "; +" "C-c l" "C-c i" "; l" "; q" "; c"))))

(defun expreg-tests--suffix-prop (suffix prop)
  "PROP from a parsed SUFFIX, across transient layout dialects."
  (if (keywordp (cadr suffix))
      (plist-get (cdr suffix) prop)   ; >= 0.8: (CLASS :key ...)
    (plist-get (nth 2 suffix) prop))) ; 0.7: (LEVEL CLASS (PLIST))

(describe "transient-bypass-keys"
  (it "passes plain strings through as exiting suffixes"
    (let ((sfx (car (transient-bypass-keys 'expreg-transient '("d")))))
      (expect (expreg-tests--suffix-prop sfx :key) :to-equal "d")
      (expect (expreg-tests--suffix-prop sfx :transient) :to-be nil)
      (expect (functionp (expreg-tests--suffix-prop sfx :command))
              :to-be-truthy)))
  (it "keeps (KEY t) suffixes inside the transient"
    (let ((sfx (car (transient-bypass-keys 'expreg-transient '(("j" t))))))
      (expect (expreg-tests--suffix-prop sfx :transient) :to-be t)))
  (it "honors an explicit command"
    (let ((sfx (car (transient-bypass-keys
                     'expreg-transient '(("#" nil ignore))))))
      (expect (expreg-tests--suffix-prop sfx :command) :to-be 'ignore)))
  (it "rejects keys that collide with the static layout"
    (expect (transient-bypass-keys 'expreg-transient '("v"))
            :to-throw 'error)))
