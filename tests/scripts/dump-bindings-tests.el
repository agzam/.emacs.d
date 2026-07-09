;;; tests/scripts/dump-bindings-tests.el --- scripts/dump-bindings.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; KEYDUMP_OUT is unset here, so loading defines functions only.
(load-module-file "scripts/dump-bindings.el")

;; dynamic declarations for the collect specs (doom-keybinds isn't loaded
;; in the batch tier)
(defvar doom-leader-map)
(defvar doom-leader-key)

(describe "dump-bindings--unwrap"
  (it "unwraps menu-items"
    (expect (dump-bindings--unwrap '(menu-item "desc" ignore)) :to-be 'ignore))
  (it "unwraps legacy (STRING . DEF) conses"
    (expect (dump-bindings--unwrap '("desc" . ignore)) :to-be 'ignore))
  (it "resolves prefix-command symbols to their keymap"
    (let ((km (make-sparse-keymap)))
      (unwind-protect
          (progn
            (fset 'dump-bindings-test-prefix km)
            (expect (dump-bindings--unwrap 'dump-bindings-test-prefix)
                    :to-be km))
        (fmakunbound 'dump-bindings-test-prefix)))))

(describe "dump-bindings--name"
  (it "names commands by symbol"
    (expect (dump-bindings--name 'ignore) :to-equal "ignore"))
  (it "names anonymous functions opaquely"
    (expect (dump-bindings--name (lambda ())) :to-equal "<lambda>"))
  (it "renders keyboard macros"
    (expect (dump-bindings--name "abc") :to-equal "<kmacro> a b c")))

(describe "dump-bindings--walk"
  (it "collects commands with full key descriptions"
    (let ((km (make-sparse-keymap)))
      (define-key km (kbd "d") #'ignore)
      (expect (dump-bindings--walk "leader" km (kbd "SPC w") nil)
              :to-equal '(("leader" "SPC w d" "ignore" nil)))))
  (it "descends through nested prefix-command symbols"
    (let ((sub (make-sparse-keymap))
          (km (make-sparse-keymap)))
      (define-key sub (kbd "m") #'ignore)
      (unwind-protect
          (progn
            (fset 'dump-bindings-test-prefix sub)
            (define-key km (kbd "w") 'dump-bindings-test-prefix)
            (expect (dump-bindings--walk "leader" km [] nil)
                    :to-equal '(("leader" "w m" "ignore" nil))))
        (fmakunbound 'dump-bindings-test-prefix))))
  (it "flags bound-but-void commands"
    (let ((km (make-sparse-keymap)))
      (define-key km (kbd "d") 'dump-bindings-test-nonexistent-fn)
      (expect (dump-bindings--walk "leader" km [] nil)
              :to-equal '(("leader" "d" "dump-bindings-test-nonexistent-fn" t))))))

(describe "dump-bindings-collect"
  (it "demands a loaded config"
    (expect (dump-bindings-collect) :to-throw 'error))
  (it "sorts entries by key"
    (let ((doom-leader-map (make-sparse-keymap))
          (doom-leader-key "SPC"))
      (define-key doom-leader-map (kbd "b") #'ignore)
      (define-key doom-leader-map (kbd "a") #'ignore)
      (expect (mapcar #'cadr (dump-bindings-collect))
              :to-equal '("SPC a" "SPC b")))))

(describe "dump-bindings"
  (it "serializes the void flag"
    (let ((doom-leader-map (make-sparse-keymap))
          (doom-leader-key "SPC")
          (out (make-temp-file "keydump")))
      (unwind-protect
          (progn
            (define-key doom-leader-map (kbd "z") 'dump-bindings-test-nonexistent-fn)
            (dump-bindings out)
            (expect (with-temp-buffer
                      (insert-file-contents out)
                      (buffer-string))
                    :to-match ":void"))
        (delete-file out)))))
