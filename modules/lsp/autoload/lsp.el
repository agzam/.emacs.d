;;; modules/lsp/autoload/lsp.el -*- lexical-binding: t; -*-

;; The two lookup handlers are vendored from Doom :tools lsp
;; autoload/lsp-mode.el (plus-free); the completion module's
;; set-lookup-handlers! call and this module's were already written against
;; these names.  They return 'deferred so the lookup dispatcher treats them
;; as async-successful.

;;;###autoload
(defun lsp! ()
  "Start lsp-mode in this buffer, deferred until the file is visible.
Doom's dispatcher minus its eglot branch (lsp-mode is the only client here)."
  (unless (bound-and-true-p lsp-mode)
    (lsp-deferred)))

;;;###autoload
(defun lsp-command-map-dispatch ()
  "Enter `lsp-command-map', loading lsp-mode first when needed.
The map symbol itself has no function cell (plain defvar keymap), so the
SPC c l row can't bind it directly - this is doom.d's
+default/lsp-command-map merge, done properly."
  (interactive)
  (require 'lsp-mode)
  (set-transient-map lsp-command-map))

;;;###autoload
(defun lsp-lookup-definition-handler ()
  "Find definition of the symbol at point using LSP."
  (interactive)
  (when-let* ((loc (lsp-request "textDocument/definition"
                                (lsp--text-document-position-params))))
    (lsp-show-xrefs (lsp--locations-to-xref-items loc) nil nil)
    'deferred))

;;;###autoload
(defun lsp-lookup-references-handler (&optional include-declaration)
  "Find project-wide references of the symbol at point using LSP."
  (interactive "P")
  (when-let*
      ((loc (lsp-request "textDocument/references"
                         (append (lsp--text-document-position-params)
                                 (list
                                  :context `(:includeDeclaration
                                             ,(lsp-json-bool include-declaration)))))))
    (lsp-show-xrefs (lsp--locations-to-xref-items loc) nil t)
    'deferred))

;;;###autoload
(defun lsp-lookup-documentation (identifier)
  "Show documentation for the symbol at point via the LSP hover, in Emacs.
`lsp-describe-thing-at-point' renders the analyzer's hover into *lsp-help*.
When the workspace has no hover for point (or lsp is off) this falls back
to `consult-dash-doc', so the offline docsets still answer.  A
`lookup-documentation' handler; IDENTIFIER is forwarded to the fallback."
  (if (bound-and-true-p lsp-mode)
      (condition-case _
          ;; 'deferred: lsp-describe-thing-at-point owns its *lsp-help* window,
          ;; so the dispatcher must not restore the window config over it.
          (progn (lsp-describe-thing-at-point) 'deferred)
        ((user-error error) (consult-dash-doc identifier)))
    (consult-dash-doc identifier)))

;;;###autoload
(defun lsp-completion-at-point-maybe ()
  "`lsp-completion-at-point', or nil when lsp-mode is off in this buffer.
Safe to mix into capf combinators that outlive the lsp session."
  (when (bound-and-true-p lsp-mode)
    (funcall 'lsp-completion-at-point)))
