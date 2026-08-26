;;; tests/writing/markdown-tests.el --- writing/autoload/markdown.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; No evil/markdown-mode/prisma in the batch tier: evil-mode must be a bound
;; special (the wrap fns read it), prisma gets a fake feature so the
;; call-time (require 'prisma) no-ops, markdown buffers fake their
;; major-mode (derived-mode-p matches the bare symbol).
(defvar evil-mode nil)
(provide 'prisma)
(provide 'grip-mode)

(load-module-file "modules/writing/autoload/markdown.el")

(defmacro with-markup-buffer (mode &rest body)
  "Run BODY in a temp buffer posing as MODE ('markdown, 'org or nil)."
  (declare (indent 1))
  `(with-temp-buffer
     (pcase ,mode
       ('org (delay-mode-hooks (org-mode)))
       ('markdown (setq major-mode 'markdown-mode)))
     ,@body))

(describe "buffer-markup-format"
  (it "detects markdown, org, and neither"
    (expect (with-markup-buffer 'markdown (buffer-markup-format))
            :to-be 'markdown)
    (expect (with-markup-buffer 'org (buffer-markup-format))
            :to-be 'org)
    (expect (with-markup-buffer nil (buffer-markup-format))
            :to-be nil)))

(describe "paste-convert-kill"
  (before-each
    (setf (symbol-function 'prisma-parse) (lambda (_fmt text) (list :ast text))
          (symbol-function 'prisma-render)
          (lambda (_fmt ast) (concat "converted:" (cadr ast)))))

  (it "renders the parsed kill into the target format"
    (expect (paste-convert-kill "# hi" 'markdown 'org)
            :to-equal "converted:# hi"))

  (it "restores line-wise semantics the conversion dropped: newline + handler"
    ;; renderers routinely trim the trailing newline off line-wise kills
    (setf (symbol-function 'prisma-render)
          (lambda (_fmt ast) (concat "converted:" (string-trim-right (cadr ast)))))
    (let* ((text (propertize "# hi\n" 'yank-handler '(evil-yank-line-handler)))
           (converted (paste-convert-kill text 'markdown 'org)))
      (expect converted :to-equal "converted:# hi\n")
      (expect (get-text-property 0 'yank-handler converted)
              :to-equal '(evil-yank-line-handler)))))

(describe "yank-remember-format-a"
  (it "tags the fresh kill with the buffer's format and remembers a fallback"
    (with-markup-buffer 'org
      (let ((kill-ring (list (copy-sequence "* heading")))
            (yank-remember--last nil))
        (yank-remember-format-a 1 2)
        (expect (get-text-property 0 'yank-source-format (car kill-ring))
                :to-be 'org)
        (expect yank-remember--last :to-equal '("* heading" . org)))))

  (it "skips the black-hole register"
    (with-markup-buffer 'org
      (let ((kill-ring (list (copy-sequence "* heading")))
            (yank-remember--last nil))
        (yank-remember-format-a 1 2 nil ?_)
        (expect (get-text-property 0 'yank-source-format (car kill-ring))
                :to-be nil)
        (expect yank-remember--last :to-be nil))))

  (it "leaves kills from non-markup buffers untouched"
    (with-markup-buffer nil
      (let ((kill-ring (list (copy-sequence "plain")))
            (yank-remember--last nil))
        (yank-remember-format-a 1 2)
        (expect yank-remember--last :to-be nil)))))

(describe "paste-maybe-convert-a"
  ;; the recorder closes over these; lexical-binding makes the closure work
  :var (orig-calls seen-kill recorder)

  (before-each
    (setq orig-calls nil seen-kill nil
          recorder (lambda (&rest args)
                     (push args orig-calls)
                     (setq seen-kill (current-kill 0))))
    (setf (symbol-function 'prisma-parse) (lambda (_fmt text) (list :ast text))
          (symbol-function 'prisma-render)
          (lambda (_fmt ast) (concat "converted:" (cadr ast)))))

  (it "converts a tagged markdown kill pasted into org"
    (with-markup-buffer 'org
      (let ((kill-ring (list (propertize "# hi" 'yank-source-format 'markdown)))
            (interprogram-paste-function nil)
            (yank-remember--last nil))
        (paste-maybe-convert-a recorder nil)
        (expect seen-kill :to-equal "converted:# hi")
        (expect orig-calls :to-equal '((nil nil nil))))))

  (it "falls back to the raw kill when formats match"
    (with-markup-buffer 'markdown
      (let ((kill-ring (list (propertize "# hi" 'yank-source-format 'markdown)))
            (interprogram-paste-function nil)
            (yank-remember--last nil))
        (paste-maybe-convert-a recorder nil)
        (expect seen-kill :to-equal "# hi"))))

  (it "pastes verbatim on bare C-u, stripping the prefix from COUNT"
    (with-markup-buffer 'org
      (let ((kill-ring (list (propertize "# hi" 'yank-source-format 'markdown)))
            (interprogram-paste-function nil)
            (yank-remember--last nil))
        (paste-maybe-convert-a recorder '(4))
        (expect seen-kill :to-equal "# hi")
        (expect orig-calls :to-equal '((nil nil nil))))))

  (it "recovers the format from the fallback pair after property loss"
    (with-markup-buffer 'org
      (let ((kill-ring (list "# hi"))  ; property stripped (clipboard round-trip)
            (interprogram-paste-function nil)
            (yank-remember--last '("# hi" . markdown)))
        (paste-maybe-convert-a recorder nil)
        (expect seen-kill :to-equal "converted:# hi"))))

  (it "pastes as-is when conversion errors"
    (setf (symbol-function 'prisma-parse)
          (lambda (&rest _) (error "boom")))
    (with-markup-buffer 'org
      (let ((kill-ring (list (propertize "# hi" 'yank-source-format 'markdown)))
            (interprogram-paste-function nil)
            (yank-remember--last nil))
        (paste-maybe-convert-a recorder nil)
        (expect seen-kill :to-equal "# hi")
        (expect orig-calls :to-equal '((nil nil nil))))))

  (it "passes straight through while a nested paste is in flight"
    (with-markup-buffer 'org
      (let ((kill-ring (list (propertize "# hi" 'yank-source-format 'markdown)))
            (interprogram-paste-function nil)
            (paste-convert--in-flight t)
            (yank-remember--last nil))
        (paste-maybe-convert-a recorder 2 ?r)
        (expect orig-calls :to-equal '((2 ?r nil)))))))

(describe "markdown-wrap-code-generic"
  (it "wraps the region in a fence and leaves point after the opening one"
    (with-temp-buffer
      (insert "some code")
      (push-mark (point-min) t t)
      (goto-char (point-max))
      (markdown-wrap-code-generic)
      (expect (buffer-string) :to-equal "```\nsome code\n```\n")
      (expect (point) :to-equal 4))))

(defun toggle-blockquote-on (text)
  "Return TEXT after toggling the quote over a region covering it whole."
  (with-temp-buffer
    (insert text)
    (push-mark (point-min) t t)
    (goto-char (point-max))
    (markdown-toggle-blockquote)
    (buffer-substring-no-properties (point-min) (point-max))))

(describe "markdown-toggle-blockquote"
  (it "prefixes every line when none is quoted"
    (expect (toggle-blockquote-on "foo\nbar\nzap")
            :to-equal "> foo\n> bar\n> zap"))

  (it "strips the marker when every line already carries one"
    (expect (toggle-blockquote-on "> foo\n> bar\n> zap")
            :to-equal "foo\nbar\nzap"))

  (it "quotes when only some lines are marked"
    (expect (toggle-blockquote-on "> foo\nbar")
            :to-equal "> > foo\n> bar"))

  (it "keeps a blank line inside the quote, and blank on the way back"
    (expect (toggle-blockquote-on "foo\n\nbar")
            :to-equal "> foo\n>\n> bar")
    (expect (toggle-blockquote-on "> foo\n>\n> bar")
            :to-equal "foo\n\nbar"))

  (it "covers whole lines from a partial selection"
    (with-temp-buffer
      (insert "foo\nbar\n")
      (goto-char (point-min))
      (forward-char 2)
      (push-mark (point) t t)
      (goto-char (- (point-max) 2))
      (markdown-toggle-blockquote)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "> foo\n> bar\n")))

  (it "handles a selection ending at the next line's bol"
    ;; where evil's visual-line leaves region-end
    (expect (toggle-blockquote-on "foo\nbar\nzap\n")
            :to-equal "> foo\n> bar\n> zap\n"))

  (it "does nothing without a region"
    (with-temp-buffer
      (insert "foo")
      (markdown-toggle-blockquote)
      (expect (buffer-string) :to-equal "foo"))))

(describe "grip-preview-in-browser"
  (it "sends the preview to the external browser, then restores the default"
    (let (seen)
      (setf (symbol-function 'grip-browse-preview)
            (lambda () (setq seen grip-preview-in-webkit)))
      (let ((grip-preview-in-webkit t))
        (grip-preview-in-browser)
        ;; A lexical binding here would leave grip on the xwidget path.
        (expect seen :to-be nil)
        (expect grip-preview-in-webkit :to-be t)))))