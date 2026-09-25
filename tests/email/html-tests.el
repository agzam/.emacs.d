;;; tests/email/html-tests.el --- HTML mail rendering specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/email/autoload/quotes.el")
(load-module-file "modules/email/autoload/html.el")

(defun html-tests-render (html &optional renderer)
  "HTML rendered as a text/html part by RENDERER, properties kept.
RENDERER defaults to `render-mail-html'."
  (let ((part (generate-new-buffer " *html-tests part*")))
    (unwind-protect
        (with-temp-buffer
          (with-current-buffer part (insert html))
          (let ((inhibit-read-only t))
            (funcall (or renderer #'render-mail-html)
                     (mm-make-handle part '("text/html" (charset . "utf-8")))))
          (buffer-string))
      (kill-buffer part))))

(defun html-tests-line-face (text line)
  "Face on the first character of LINE in TEXT."
  (get-text-property (string-match (concat "^" (regexp-quote line) "$") text) 'face text))

(defun html-tests-colors (text)
  "Every foreground and background color a face in TEXT sets."
  (let ((pos 0) colors)
    (while (< pos (length text))
      (let ((face (get-text-property pos 'face text)))
        (dolist (spec (if (keywordp (car-safe face)) (list face) (ensure-list face)))
          (when (keywordp (car-safe spec))
            (dolist (key '(:foreground :background))
              (when-let* ((color (plist-get spec key)))
                (push color colors))))))
      (setq pos (next-single-property-change pos 'face text (length text))))
    colors))

(defvar html-tests-reply
  (concat "<p>Top</p>"
          "<blockquote><p>one</p><p>again</p>"
          "<blockquote><p>two</p></blockquote></blockquote>"
          "<p>After</p>")
  "A reply quoting two levels deep.")

(defvar html-tests-table
  (concat "<table><tr><td>left cell</td><td>right</td></tr>"
          "<tr><td>a</td><td>longer right cell</td></tr></table><p>end</p>")
  "A table whose rows shr pads out to the widest cell.")

(describe "render-mail-html"
  (it "draws each blockquote level as a > marker"
    (expect (substring-no-properties (html-tests-render html-tests-reply))
            :to-equal "Top\n\n> one\n>\n> again\n>\n> > two\n\nAfter\n\n"))
  (it "faces each quoted line by its depth, markers included"
    (let ((text (html-tests-render html-tests-reply)))
      (expect (html-tests-line-face text "Top") :to-be nil)
      (expect (html-tests-line-face text "> one") :to-be 'message-cited-text-1)
      (expect (html-tests-line-face text "> > two") :to-be 'message-cited-text-2)
      (expect (html-tests-line-face text "After") :to-be nil)))
  (it "keeps a link's face in front of the quote's"
    (let ((text (html-tests-render
                 "<blockquote><p>see <a href=\"https://x.example\">link</a></p></blockquote>")))
      (expect (get-text-property (string-match "link" text) 'face text)
              :to-equal '(shr-link message-cited-text-1))))
  (it "faces a quote the sender typed as > lines"
    (let ((text (html-tests-render "<div>Earlier:<br>&gt; one<br>&gt; &gt; two</div>")))
      (expect (html-tests-line-face text "> one") :to-be 'message-cited-text-1)
      (expect (html-tests-line-face text "> > two") :to-be 'message-cited-text-2)))
  (it "leaves no depth bookkeeping on the text"
    (let ((text (html-tests-render html-tests-reply)))
      (expect (text-property-not-all 0 (length text) 'mail-quote-depth nil text)
              :to-be nil)))
  (it "drops the sender's colors, which eww keeps"
    ;; shr colors nothing on a display with fewer than 88 colors, and a
    ;; batch Emacs has none
    (cl-letf (((symbol-function 'display-color-cells) (lambda (&rest _) 16777216)))
      (let ((shr-use-colors t)
            (html "<p style=\"color:#ffffff;background-color:#000000\">boxed</p>"))
        (expect (html-tests-colors (html-tests-render html #'mm-shr))
                :to-have-same-items-as '("#ffffff" "#000000"))
        (expect (html-tests-colors (html-tests-render html)) :to-be nil))))
  (it "leaves paragraphs unfilled for the window to wrap, whatever shr was set to"
    ;; a session before eww loads fills with shr's own defaults
    (let ((shr-fill-text t)
          (shr-use-fonts t)
          (fill-column 20)
          (html (concat "<p>" (string-join (make-list 30 "word") " ") "</p>")))
      (expect (length (split-string (string-trim (html-tests-render html)) "\n"))
              :to-be 1)))
  (it "strips the spaces shr pads table rows with"
    (let ((shr-fill-text nil)
          (shr-use-fonts nil))
      (expect (string-match-p "[ \t]$" (html-tests-render html-tests-table #'mm-shr))
              :to-be-truthy))
    (expect (string-match-p "[ \t]$" (html-tests-render html-tests-table)) :to-be nil)
    (expect (substring-no-properties (html-tests-render html-tests-table))
            :to-match "left cell +right\n a +longer right cell\n"))
  (it "binds its blockquote renderer for mail alone"
    (let ((before shr-external-rendering-functions))
      (html-tests-render html-tests-reply)
      (expect shr-external-rendering-functions :to-equal before))
    (with-temp-buffer
      (shr-insert-document
       (with-temp-buffer
         (insert "<blockquote><p>one</p></blockquote>")
         (libxml-parse-html-region (point-min) (point-max))))
      (expect (string-trim (buffer-substring-no-properties (point-min) (point-max)))
              :to-equal "one"))))

;;; html-tests.el ends here
