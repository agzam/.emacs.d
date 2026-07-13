;;; tests/lsp/lsp-tests.el --- lsp/autoload/lsp.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/lsp/autoload/lsp.el")

(defvar lsp-mode nil)

(describe "lsp!"
  (it "defers lsp startup when lsp-mode is off"
    (let (called)
      (cl-letf (((symbol-function 'lsp-deferred) (lambda () (setq called t))))
        (with-temp-buffer
          (lsp!)
          (expect called :to-be t)))))

  (it "no-ops when lsp-mode is already on"
    (let (called)
      (cl-letf (((symbol-function 'lsp-deferred) (lambda () (setq called t))))
        (with-temp-buffer
          (setq-local lsp-mode t)
          (lsp!)
          (expect called :to-be nil))))))

(describe "lsp-completion-at-point-maybe"
  (it "returns nil outside lsp buffers without touching lsp"
    (let (called)
      (cl-letf (((symbol-function 'lsp-completion-at-point)
                 (lambda () (setq called t) 'capf)))
        (with-temp-buffer
          (expect (lsp-completion-at-point-maybe) :to-be nil)
          (expect called :to-be nil)))))

  (it "delegates in lsp buffers"
    (cl-letf (((symbol-function 'lsp-completion-at-point) (lambda () 'capf)))
      (with-temp-buffer
        (setq-local lsp-mode t)
        (expect (lsp-completion-at-point-maybe) :to-equal 'capf)))))

(describe "lsp lookup handlers"
  (it "definition handler shows xrefs and reports 'deferred"
    (let (shown)
      (cl-letf (((symbol-function 'lsp-request) (lambda (&rest _) '(loc)))
                ((symbol-function 'lsp--text-document-position-params)
                 (lambda () '(:pos t)))
                ((symbol-function 'lsp--locations-to-xref-items)
                 (lambda (loc) (cons 'xrefs loc)))
                ((symbol-function 'lsp-show-xrefs)
                 (lambda (xrefs &rest _) (setq shown xrefs))))
        (expect (lsp-lookup-definition-handler) :to-equal 'deferred)
        (expect shown :to-equal '(xrefs loc)))))

  (it "definition handler returns nil when the server finds nothing"
    (cl-letf (((symbol-function 'lsp-request) (lambda (&rest _) nil))
              ((symbol-function 'lsp--text-document-position-params)
               (lambda () '(:pos t))))
      (expect (lsp-lookup-definition-handler) :to-be nil)))

  (it "references handler passes includeDeclaration through"
    (let (req)
      (cl-letf (((symbol-function 'lsp-request)
                 (lambda (_method params) (setq req params) '(loc)))
                ((symbol-function 'lsp--text-document-position-params)
                 (lambda () '(:pos t)))
                ((symbol-function 'lsp-json-bool) (lambda (x) (if x t :json-false)))
                ((symbol-function 'lsp--locations-to-xref-items) #'identity)
                ((symbol-function 'lsp-show-xrefs) #'ignore))
        (expect (lsp-lookup-references-handler t) :to-equal 'deferred)
        (expect (plist-get req :context) :to-equal '(:includeDeclaration t))))))

(describe "lsp-command-map-dispatch"
  (it "loads lsp-mode and installs the transient map"
    (let (armed)
      (defvar lsp-command-map)
      (let ((lsp-command-map (make-sparse-keymap)))
        (cl-letf (((symbol-function 'require)
                   (lambda (&rest _) t))
                  ((symbol-function 'set-transient-map)
                   (lambda (map &rest _) (setq armed map))))
          (lsp-command-map-dispatch)
          (expect armed :to-be lsp-command-map))))))
