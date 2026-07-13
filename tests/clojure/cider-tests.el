;;; tests/clojure/cider-tests.el --- clojure/autoload/cider.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/clojure/autoload/cider.el")

(describe "cider-fqn-symbol-at-point"
  (it "resolves the syntax-quoted symbol through the tooling session"
    (cl-letf (((symbol-function 'cider-connected-p) (lambda () t))
              ((symbol-function 'cider-current-ns) (lambda () "foo.core"))
              ((symbol-function 'cider-symbol-at-point) (lambda (&optional _) "my-fn"))
              ((symbol-function 'cider-sync-tooling-eval)
               (lambda (form &optional _ns)
                 (when (string-match-p "`" form) '(dict "value" "(foo.core/my-fn)"))))
              ((symbol-function 'nrepl-dict-get)
               (lambda (_dict key) (when (equal key "value") "(foo.core/my-fn)"))))
      (expect (cider-fqn-symbol-at-point) :to-equal "foo.core/my-fn")))

  (it "returns nil when disconnected"
    (cl-letf (((symbol-function 'cider-connected-p) #'ignore))
      (expect (cider-fqn-symbol-at-point) :to-be nil))))

(describe "lsp-clojure-fqn-at-point (hover fallback)"
  ;; The cursorInfo/raw primary path needs lsp structs; force it to error
  ;; and pin the hover-parsing regexes - the valuable, fragile part.
  (defun cider-tests--hover (value)
    (cl-letf (((symbol-function 'lsp-request) (lambda (&rest _) (error "no cursorInfo")))
              ((symbol-function 'lsp--make-request) (lambda (&rest _) 'req))
              ((symbol-function 'lsp--send-request) (lambda (_) 'resp))
              ((symbol-function 'lsp--text-document-position-params) (lambda () nil))
              ((symbol-function 'lsp:hover-contents)
               (lambda (_) (list :value value))))
      (lsp-clojure-fqn-at-point)))

  (it "extracts a namespaced var from the hover code fence"
    (expect (cider-tests--hover "```clojure\nfoo.core/my-fn [x]\n```")
            :to-equal "foo.core/my-fn"))

  (it "falls back to a bare symbol"
    (expect (cider-tests--hover "```clojure\nmy-local\n```")
            :to-equal "my-local"))

  (it "returns nil without a clojure code fence"
    (expect (cider-tests--hover "plain text") :to-be nil)))

(describe "clojure-project-root-path-poly"
  (it "prefers the Polylith workspace root when present"
    (cl-letf (((symbol-function 'locate-dominating-file)
               (lambda (_file name) (when (equal name "workspace.edn") "/ws/")))
              ((symbol-function 'clojure-project-root-path)
               (lambda (&optional dir) (list 'root dir))))
      (expect (clojure-project-root-path-poly "/elsewhere/")
              :to-equal '(root "/ws/"))))

  (it "falls back to the vanilla resolution"
    (cl-letf (((symbol-function 'locate-dominating-file) #'ignore)
              ((symbol-function 'clojure-project-root-path)
               (lambda (&optional dir) (list 'root dir))))
      (expect (clojure-project-root-path-poly "/proj/")
              :to-equal '(root "/proj/")))))

(describe "kill-cider-buffers"
  (it "kills every *cider*/*nrepl* buffer with ARG, no questions asked"
    (let ((repl (generate-new-buffer "*cider-repl test*"))
          (nrepl (generate-new-buffer "*nrepl-server test*"))
          (other (generate-new-buffer "innocent-bystander")))
      (unwind-protect
          (progn
            (kill-cider-buffers 'all)
            (expect (buffer-live-p repl) :to-be nil)
            (expect (buffer-live-p nrepl) :to-be nil)
            (expect (buffer-live-p other) :to-be-truthy))
        (dolist (b (list repl nrepl other))
          (when (buffer-live-p b) (kill-buffer b)))))))

(describe "clojure-set-completion-at-point-h"
  (it "builds the combined capf, replaces the singles, keeps it local"
    (cl-letf (((symbol-function 'cape-capf-super)
               (lambda (&rest fns) (lambda () (list 'super fns))))
              ((symbol-function 'cape-completion-at-point-functions-h) #'ignore))
      (with-temp-buffer
        (setq-local completion-at-point-functions
                    '(lsp-completion-at-point cider-complete-at-point
                      yasnippet-capf t))
        (clojure-set-completion-at-point-h)
        (expect (memq 'cape-cider-lsp-yas completion-at-point-functions)
                :to-be-truthy)
        (expect (memq 'lsp-completion-at-point completion-at-point-functions)
                :to-be nil)
        (expect (memq 'cider-complete-at-point completion-at-point-functions)
                :to-be nil)
        (expect (car completion-styles) :to-equal 'orderless)))))

(describe "cider-complete-at-point-maybe"
  (it "nil when disconnected, delegates when connected"
    (cl-letf (((symbol-function 'cider-connected-p) #'ignore))
      (expect (cider-complete-at-point-maybe) :to-be nil))
    (cl-letf (((symbol-function 'cider-connected-p) (lambda () t))
              ((symbol-function 'cider-complete-at-point) (lambda () 'capf)))
      (expect (cider-complete-at-point-maybe) :to-equal 'capf))))
