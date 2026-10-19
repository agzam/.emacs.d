;;; tests/writing/markdown-tests.el --- writing/autoload/markdown.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; No evil/markdown-mode in the batch tier: evil-mode must be a bound
;; special (the wrap fns read it).
(defvar evil-mode nil)
(provide 'grip-mode)

(load-module-file "modules/writing/autoload/markdown.el")

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

(defun blockquote-bounds-text (text search)
  "Text of the blockquote bounds found with point after SEARCH in TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (search-forward search)
    (when-let* ((bounds (markdown-blockquote-bounds)))
      (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(describe "markdown-blockquote-bounds"
  (it "spans every quote line around point, final newline included"
    (expect (blockquote-bounds-text "intro\n\n> fooo\n> bar\n> zap\n\noutro\n" "bar")
            :to-equal "> fooo\n> bar\n> zap\n"))

  (it "takes indented, nested and blank quote lines as one quote"
    (expect (blockquote-bounds-text "- item\n  > a\n  >\n  > > b\nend\n" "b")
            :to-equal "  > a\n  >\n  > > b\n"))

  (it "stops at the buffer edges"
    (expect (blockquote-bounds-text "> a\n> b" "b") :to-equal "> a\n> b"))

  (it "finds nothing outside a quote"
    (expect (blockquote-bounds-text "intro\n> a\n" "intro") :to-be nil)))

(describe "markdown-blockquote-unquote"
  (it "strips one level of marks and one space from each line"
    (with-temp-buffer
      (insert "> fooo\n>\n> > nested\n  > indented\n>tight\n>   code\n")
      (markdown-blockquote-unquote)
      (expect (buffer-string)
              :to-equal "fooo\n\n> nested\nindented\ntight\n  code\n"))))

(describe "markdown-blockquote-requote"
  (it "marks each line and leaves blank ones without trailing space"
    (expect (markdown-blockquote-requote "fooo\n\n  \n> nested\n" "")
            :to-equal "> fooo\n>\n>\n> > nested\n"))

  (it "puts the indent back in front of each mark"
    (expect (markdown-blockquote-requote "a\nb" "  ") :to-equal "  > a\n  > b"))

  (it "leaves empty text empty"
    (expect (markdown-blockquote-requote "" "") :to-equal ""))

  (it "undoes markdown-blockquote-unquote"
    (let ((quote "> fooo\n>\n> > nested\n>   code\n"))
      (with-temp-buffer
        (insert quote)
        (markdown-blockquote-unquote)
        (expect (markdown-blockquote-requote (buffer-string) "")
                :to-equal quote)))))

(describe "markdown-blockquote-point"
  (it "lands on the same character once the marks are gone"
    (let (pos)
      (with-temp-buffer
        (insert "intro\n> fooo\n> bar baz\n")
        (goto-char (point-min))
        (search-forward "> bar b")
        (setq pos (markdown-blockquote-point (line-beginning-position 0))))
      (with-temp-buffer
        (insert "fooo\nbar baz\n")
        (markdown-blockquote-goto (point-min) pos)
        (expect (buffer-substring (point) (line-end-position)) :to-equal "az"))))

  (it "stops at the line start when point sat inside the mark"
    (with-temp-buffer
      (insert "bar\n")
      (markdown-blockquote-goto (point-min) '(0 . 5))
      (expect (point) :to-equal (point-min)))))

(describe "markdown-edit-blockquote"
  (it "refuses outside a quote"
    (with-fake-feature 'edit-indirect
      (with-temp-buffer
        (insert "intro\n")
        (expect (markdown-edit-blockquote) :to-throw 'user-error)))))

(defun edit-special-on (text in-fence)
  "Which editor `markdown-edit-special' picks on TEXT, IN-FENCE or not."
  (let (picked)
    (cl-letf (((symbol-function 'markdown-get-enclosing-fenced-block-construct)
               (lambda () in-fence))
              ((symbol-function 'markdown-edit-code-block)
               (lambda () (interactive) (setq picked 'code)))
              ((symbol-function 'markdown-edit-blockquote)
               (lambda () (interactive) (setq picked 'quote))))
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (markdown-edit-special)))
    picked))

(describe "markdown-edit-special"
  (it "edits the code block when point is inside a fence"
    (expect (edit-special-on "(foo)\n" t) :to-be 'code))

  (it "keeps a quote-like line inside a fence as code"
    (expect (edit-special-on "> prompt\n" t) :to-be 'code))

  (it "edits the blockquote outside code blocks"
    (expect (edit-special-on "> foo\n" nil) :to-be 'quote))

  (it "refuses elsewhere"
    (expect (edit-special-on "intro\n" nil) :to-throw 'user-error)))

(describe "markdown-blockquote-commit-h"
  :var (edit)
  (before-each
    (setq edit (generate-new-buffer " *blockquote-edit*"))
    ;; the blank line makes requoting touch the match data
    (with-current-buffer edit (insert "fooo\n\nbar baz\n")))
  (after-each (kill-buffer edit))

  (it "requotes from the edit buffer whatever ran on the region before"
    (with-temp-buffer
      ;; what edit-indirect copied, re-indented by markdown-mode's fixup
      (insert "- item\n  fooo\n\n  bar baz\nend\n")
      (let ((ov (make-overlay 8 26)))
        (overlay-put ov 'edit-indirect-buffer edit)
        (overlay-put ov 'blockquote-indent "  ")
        (markdown-blockquote-commit-h 8 26)
        (expect (buffer-string)
                :to-equal "- item\n  > fooo\n  >\n  > bar baz\nend\n")
        ;; the overlay must keep spanning the quote for the next commit
        (expect (buffer-substring (overlay-start ov) (overlay-end ov))
                :to-equal "  > fooo\n  >\n  > bar baz\n"))))

  (it "leaves commits of other edit buffers alone"
    (with-temp-buffer
      (insert "intro\nfooo\n\nbar baz\n")
      (overlay-put (make-overlay 7 21) 'edit-indirect-buffer edit)
      (markdown-blockquote-commit-h 7 21)
      (expect (buffer-string) :to-equal "intro\nfooo\n\nbar baz\n"))))

(describe "markdown-blockquote-commit-a"
  (it "puts the source point on the spot the edit buffer left off"
    (let ((source (generate-new-buffer " *blockquote-source*"))
          (edit (generate-new-buffer " *blockquote-edit*")))
      (unwind-protect
          (progn
            (with-current-buffer source (insert "intro\n  > fooo\n  > bar baz\n"))
            (with-current-buffer edit
              (insert "fooo\nbar baz\n")
              (goto-char (point-min))
              (search-forward "bar b")
              (setq markdown-blockquote-start
                    (with-current-buffer source (copy-marker 7)))
              ;; a real commit leaves the source point at the quote start
              (markdown-blockquote-commit-a
               (lambda () (with-current-buffer source (goto-char 7)))))
            (with-current-buffer source
              (expect (buffer-substring (point) (line-end-position))
                      :to-equal "az")))
        (kill-buffer edit)
        (kill-buffer source))))

  (it "only commits other edit buffers"
    (with-temp-buffer
      (let ((calls 0))
        (markdown-blockquote-commit-a (lambda () (setq calls (1+ calls))))
        (expect calls :to-equal 1)))))

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