;;; tests/python/python-tests.el --- python/autoload/python.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/python/autoload/python.el")

(describe "python--path-to-dotted"
  (it "converts a slashed path to dotted notation"
    (expect (python--path-to-dotted "foo/bar/baz") :to-equal "foo.bar.baz"))
  (it "strips a trailing __init__"
    (expect (python--path-to-dotted "foo/bar/__init__") :to-equal "foo.bar")))

(describe "python--resolve-project-module"
  (it "walks the __init__.py chain to a dotted module"
    (let* ((root (make-temp-file "pytest" t))
           (pkg (expand-file-name "pkg" root))
           (sub (expand-file-name "sub" pkg)))
      (unwind-protect
          (progn
            (make-directory sub t)
            (write-region "" nil (expand-file-name "__init__.py" pkg))
            (write-region "" nil (expand-file-name "__init__.py" sub))
            (write-region "" nil (expand-file-name "mod.py" sub))
            (expect (python--resolve-project-module (expand-file-name "mod.py" sub))
                    :to-equal "pkg.sub.mod")
            (expect (python--resolve-project-module
                     (expand-file-name "__init__.py" sub))
                    :to-equal "pkg.sub"))
        (delete-directory root t)))))

(describe "python--uri-to-module"
  ;; lsp--uri-to-path strips the file:// scheme; identity is enough here.
  (it "maps stdlib, site-packages and typestub paths"
    (cl-letf (((symbol-function 'lsp--uri-to-path) #'identity))
      (expect (python--uri-to-module "/x/site-packages/foo/bar.py")
              :to-equal "foo.bar")
      (expect (python--uri-to-module "/usr/lib/python3.11/os.py")
              :to-equal "os")
      (expect (python--uri-to-module "/x/typestubs-abc/stdlib/typing.pyi")
              :to-equal "typing"))))

(describe "python--resolve-module-ref"
  (it "resolves relative imports against the current package"
    (cl-letf (((symbol-function 'python--current-package) (lambda () "a.b.c")))
      ;; single dot -> same package
      (expect (python--resolve-module-ref ".config") :to-equal "a.b.c.config")
      ;; double dot -> parent package
      (expect (python--resolve-module-ref "..config") :to-equal "a.b.config")))
  (it "passes absolute imports through untouched"
    (cl-letf (((symbol-function 'python--current-package) (lambda () "a.b.c")))
      (expect (python--resolve-module-ref "os") :to-equal "os"))))

(describe "python-fix-all"
  (it "shells out to ruff F401 against the buffer file"
    (let (captured)
      (cl-letf (((symbol-function 'save-buffer) #'ignore)
                ((symbol-function 'revert-buffer) #'ignore)
                ((symbol-function 'shell-command)
                 (lambda (cmd &rest _) (setq captured cmd))))
        (with-temp-buffer
          (setq buffer-file-name "/tmp/foo.py")
          (python-fix-all)
          (expect captured
                  :to-equal "ruff check --select F401 --fix /tmp/foo.py"))))))

(describe "plus-free renames"
  (it "exposes the renamed commands"
    (expect (fboundp 'python-lookup-handlers-h) :to-be-truthy)
    (expect (fboundp 'python-fully-qualified-symbol-at-point) :to-be-truthy))
  (it "drops the doom.d +/py- names"
    (expect (fboundp '+python-mode-lookup-handlers) :to-be nil)
    (expect (fboundp 'py-fully-qualified-symbol-at-point) :to-be nil)))
