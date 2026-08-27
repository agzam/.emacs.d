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