;;; tests/clojure/tools-tests.el --- clojure/autoload/tools.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/clojure/autoload/tools.el")

(describe "cider-load-alias"
  (it "offers only :alias lines and syncs the choice"
    (let (evaled)
      (cl-letf (((symbol-function 'cider-connected-p) (lambda () t))
                ((symbol-function 'cider-current-dir) (lambda () "/proj/"))
                ((symbol-function 'nrepl-dict-get)
                 (lambda (dict key)
                   (pcase key
                     ("out" ":dev (dev tools)\n:test (tests)\nnoise line\n")
                     ("value" "synced"))))
                ((symbol-function 'cider-sync-tooling-eval)
                 (lambda (form &rest _) (push form evaled) 'dict))
                ((symbol-function 'completing-read)
                 (lambda (_prompt coll &rest _)
                   (expect coll :to-equal '(":dev (dev tools)" ":test (tests)"))
                   ":dev (dev tools)"))
                ((symbol-function 'print) #'ignore))
        (cider-load-alias)
        (expect (car evaled) :to-equal "(sync-deps :aliases [:dev])"))))

  (it "demands a connection"
    (cl-letf (((symbol-function 'cider-connected-p) #'ignore))
      (expect (cider-load-alias) :to-throw 'user-error))))

(describe "lsp-clojure-project-tree-toggle"
  (it "pops to the tree buffer when visible"
    (let (switched)
      (defvar lsp-clojure--project-tree-buffer-name)
      (let ((lsp-clojure--project-tree-buffer-name "*Clojure Project Tree*"))
        (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'win))
                  ((symbol-function 'switch-to-buffer-other-window)
                   (lambda (buf) (setq switched buf))))
          (lsp-clojure-project-tree-toggle)
          (expect switched :to-equal "*Clojure Project Tree*")))))

  (it "invokes the package command otherwise"
    (let (called)
      (defvar lsp-clojure--project-tree-buffer-name)
      (let ((lsp-clojure--project-tree-buffer-name "*Clojure Project Tree*"))
        (cl-letf (((symbol-function 'get-buffer-window) #'ignore)
                  ((symbol-function 'call-interactively)
                   (lambda (cmd) (setq called cmd))))
          (lsp-clojure-project-tree-toggle)
          (expect called :to-be #'lsp-clojure-show-project-tree))))))

(describe "cider-storm-start-gui-styled"
  (it "sends local-connect with the configured theme and styles"
    (let (sent)
      (defvar cider-storm-flow-storm-theme)
      (defvar cider-storm-styles-path)
      (let ((cider-storm-flow-storm-theme 'light)
            (cider-storm-styles-path "/tmp/styles.css"))
        ;; the package macro isn't loaded in batch; a function stub in its
        ;; cell makes the interpreted body evaluate the eval-form eagerly
        (cl-letf (((symbol-function 'cider-storm--ensure-connected)
                   (lambda (&rest _) nil))
                  ((symbol-function 'cider-interactive-eval)
                   (lambda (form &rest _) (setq sent form))))
          (cider-storm-start-gui-styled)
          (expect sent :to-match "flow-storm.api/local-connect")
          (expect sent :to-match ":theme :light")
          (expect sent :to-match "/tmp/styles.css"))))))
