;;; tests/lsp/autoload-tests.el --- lsp/autoload.el (booster) specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; Loading the eager file registers the json advice - undo it after; the
;; batch process must not keep a live advice on json-parse-buffer.
(load-module-file "modules/lsp/autoload.el")
(advice-remove (if (fboundp 'json-parse-buffer) 'json-parse-buffer 'json-read)
               #'lsp-booster--advice-json-parse)

(defvar lsp-use-plists)

(describe "lsp-booster--advice-json-parse"
  (it "evaluates bytecode payloads instead of calling the json parser"
    (with-temp-buffer
      (insert (prin1-to-string (byte-compile (lambda () '(:result 42)))))
      (goto-char (point-min))
      (expect (lsp-booster--advice-json-parse
               (lambda (&rest _) (error "json parser must not run")))
              :to-equal '(:result 42))))

  (it "falls through to the json parser for plain json"
    (with-temp-buffer
      (insert "{\"a\": 1}")
      (goto-char (point-min))
      (expect (lsp-booster--advice-json-parse
               (lambda (&rest _) 'parsed-by-json))
              :to-equal 'parsed-by-json))))

(describe "lsp-booster--advice-final-command"
  (before-each (setq lsp-use-plists t))

  (it "wraps the command when the booster binary exists"
    ;; json-rpc-connection (native jsonrpc) is undefined on 30.1 and this
    ;; 31 build - if a future Emacs defines it, the wrap branch dies by
    ;; design and this spec should fail loudly.
    (assume (not (functionp 'json-rpc-connection)))
    (cl-letf (((symbol-function 'executable-find) (lambda (bin) bin)))
      (let ((default-directory "/tmp/"))
        (expect (lsp-booster--advice-final-command
                 (lambda (cmd _) cmd) '("clojure-lsp") nil)
                :to-equal '("emacs-lsp-booster" "clojure-lsp")))))

  (it "passes through under test? (server presence checks)"
    (cl-letf (((symbol-function 'executable-find) (lambda (bin) bin)))
      (expect (lsp-booster--advice-final-command
               (lambda (cmd _) cmd) '("clojure-lsp") t)
              :to-equal '("clojure-lsp"))))

  (it "passes through without the booster binary"
    (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
      (let ((default-directory "/tmp/"))
        (expect (lsp-booster--advice-final-command
                 (lambda (cmd _) cmd) '("clojure-lsp") nil)
                :to-equal '("clojure-lsp")))))

  (it "passes through when plists are off (mismatch would break parsing)"
    (setq lsp-use-plists nil)
    (cl-letf (((symbol-function 'executable-find) (lambda (bin) bin)))
      (let ((default-directory "/tmp/"))
        (expect (lsp-booster--advice-final-command
                 (lambda (cmd _) cmd) '("clojure-lsp") nil)
                :to-equal '("clojure-lsp"))))))
