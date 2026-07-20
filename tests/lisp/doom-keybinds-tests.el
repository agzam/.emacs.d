;;; tests/lisp/doom-keybinds-tests.el --- map! which-key label specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; `lisp/doom-keybinds.el' can't load in the batch tier (top-level `general').
;; Read just `doom--map-def' out of it and eval that one defun - it only leans
;; on `doom-unquote' (doom-compat) and the `doom--map-batch-forms' accumulator.
(defvar doom--map-batch-forms nil)
(with-temp-buffer
  (insert-file-contents
   (expand-file-name "lisp/doom-keybinds.el" test-config-root))
  (goto-char (point-min))
  (search-forward "(defun doom--map-def ")
  (goto-char (match-beginning 0))
  (eval (read (current-buffer)) t))

(defun map-def-form (key def states desc)
  "Return the single def form `doom--map-def' emits for KEY/DEF/STATES/DESC."
  (let ((doom--map-batch-forms nil))
    (doom--map-def key def states desc)
    (nth 1 (cadar doom--map-batch-forms))))

(describe "doom--map-def :desc handling"
  (it "embeds a terminal key's label in the keymap as a (DESC . DEF) cons"
    ;; This is what survives embark showing a sub-keymap with the prefix
    ;; stripped: built-in which-key reads the car of the menu item directly,
    ;; where a global key-sequence replacement (keyed on the whole sequence)
    ;; would never match.
    (let ((form (map-def-form "s" '(function go-jira-search-slack-threads)
                              '(nil) "Slack Threads")))
      (expect (car form) :to-be 'cons)
      (expect form :to-equal
              '(cons "Slack Threads" (function go-jira-search-slack-threads)))))

  (it "keeps a prefix label as a global :ignore replacement, not a fresh keymap"
    ;; Rebinding the prefix to (cons DESC (make-sparse-keymap)) would drop the
    ;; keys another module already bound under the same leader prefix; a global
    ;; replacement leaves the prefix keymap intact so the trees still merge.
    (let ((form (map-def-form "" nil '(nil) "find")))
      (expect (car form) :to-be 'list)
      (expect form :to-equal '(list :ignore t :which-key "find"))))

  (it "still routes an extended (keyword-car) def through :which-key"
    (let ((form (map-def-form "k" '(:def bar) '(nil) "Edit")))
      (expect (car form) :to-be 'quote)
      (expect (plist-get (cadr form) :def) :to-be 'bar)
      (expect (plist-get (cadr form) :which-key) :to-equal "Edit")))

  (it "leaves a label-less key untouched"
    (expect (map-def-form "k" '(function foo) '(nil) nil)
            :to-equal '(function foo))))

(describe "the embedded label under embark's prefix strip"
  ;; The embark indicator resolves a pressed prefix to its bare sub-keymap
  ;; (`lookup-key' drops the menu-item label) and hands that to which-key with
  ;; no prefix context, so the label has to ride on the binding itself.
  (it "resolves the sub-keymap and keeps the command runnable"
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "f s") (cons "Slack Threads" #'ignore))
      (let ((submap (lookup-key map (kbd "f"))))
        (expect (keymapp submap) :to-be-truthy)
        (expect (lookup-key map (kbd "f s")) :to-be #'ignore)
        (expect (car (cdr (assq ?s (cdr submap)))) :to-equal "Slack Threads"))))

  (it "renders the label through which-key on the standalone sub-keymap"
    (assume (require 'which-key nil t) "which-key unavailable")
    (assume (fboundp 'which-key--get-bindings) "which-key internals changed")
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "f g") (cons "GH PRs" #'ignore))
      (define-key map (kbd "f s") (cons "Slack Threads" #'ignore))
      (let* ((submap (lookup-key map (kbd "f")))
             (which-key-replacement-alist nil)
             (shown (with-temp-buffer
                      (fundamental-mode)
                      (mapcar (lambda (b)
                                (cons (substring-no-properties (nth 0 b))
                                      (substring-no-properties (nth 2 b))))
                              (which-key--get-bindings nil submap nil nil)))))
        (expect (assoc "s" shown) :to-equal '("s" . "Slack Threads"))
        (expect (assoc "g" shown) :to-equal '("g" . "GH PRs"))))))

(provide 'doom-keybinds-tests)
;;; tests/lisp/doom-keybinds-tests.el ends here
