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

(defun html-tests-faces (text string &optional offset)
  "Faces OFFSET characters into the first STRING in TEXT, as a list."
  (ensure-list (get-text-property (+ (string-search string text) (or offset 0)) 'face text)))

(defmacro html-tests-in-group (group parameters &rest body)
  "Run BODY while Gnus reads GROUP, with PARAMETERS as `gnus-parameters'."
  (declare (indent 2))
  `(let ((gnus-newsgroup-name ,group)
         (gnus-newsrc-hashtb (gnus-make-hashtable))
         (gnus-parameters ,parameters))
     ,@body))

(defmacro html-tests-with-theme (background text &rest body)
  "Run BODY under a theme of BACKGROUND and TEXT colors.
The code face gets its background back after."
  (declare (indent 2))
  `(let ((code (face-attribute 'mail-code :background)))
     (unwind-protect
         (cl-letf (((symbol-function 'face-background) (lambda (&rest _) ,background))
                   ((symbol-function 'face-foreground) (lambda (&rest _) ,text)))
           ,@body)
       (set-face-attribute 'mail-code nil :background code))))

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

(defvar html-tests-code
  (concat "<p>Run <code>(setq inline 1)</code> first:</p>\n"
          "<pre><code>(setq answer &quot;yes&quot;)\n  (indented)\n</code></pre>\n"
          "<p>after</p>")
  "Inline code and a code block, as Reddit writes them.")

(defvar html-tests-quoted-code
  (concat "<blockquote><pre><code>  (defun first ()\n    (body))\n</code></pre>"
          "<p>quoted</p><pre><code>later\n    indented\n</code></pre></blockquote>")
  "A quote opening on a code block, and a code block further in.")

(defvar html-tests-emacs-lisp
  '(("\\`nnmaildir\\+reddit:r\\.test\\'" (mail-code-major-mode . emacs-lisp-mode)))
  "Parameters that highlight the code blocks of r.test as Emacs Lisp.")

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
  (it "leaves no depth or preformatted bookkeeping on the text"
    (dolist (text (list (html-tests-render html-tests-reply)
                        (html-tests-render html-tests-quoted-code)))
      (dolist (property '(mail-quote-depth mail-preformatted))
        (expect (text-property-not-all 0 (length text) property nil text)
                :to-be nil))))
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
  (it "binds its blockquote, pre and code renderers for mail alone"
    (let ((before shr-external-rendering-functions))
      (html-tests-render html-tests-reply)
      (html-tests-render html-tests-code)
      (expect shr-external-rendering-functions :to-equal before))
    (with-temp-buffer
      (shr-insert-document
       (with-temp-buffer
         (insert "<blockquote><p>one</p></blockquote><pre><code>(two)</code></pre>")
         (libxml-parse-html-region (point-min) (point-max))))
      (expect (string-trim (buffer-substring-no-properties (point-min) (point-max)))
              :to-match "\\`one\n")
      (goto-char (point-min))
      (search-forward "(two)")
      (expect (ensure-list (get-text-property (match-beginning 0) 'face))
              :not :to-contain 'mail-code-block))))

(describe "code in render-mail-html"
  (it "faces inline code in mail-code and a code block in mail-code-block, the prose bare"
    (let ((text (html-tests-render html-tests-code)))
      (expect (substring-no-properties text)
              :to-equal (concat "Run (setq inline 1) first:\n\n"
                                "(setq answer \"yes\")\n  (indented)\n\nafter\n\n"))
      (expect (html-tests-faces text "Run") :to-be nil)
      (expect (html-tests-faces text "(setq inline") :to-equal '(mail-code))
      (expect (html-tests-faces text " first") :to-be nil)
      (expect (html-tests-faces text "(setq answer") :to-equal '(mail-code-block))
      (expect (html-tests-faces text "  (indented)") :to-equal '(mail-code-block))
      (expect (html-tests-faces text "after") :to-be nil)))
  (it "carries a code block's face onto the newline of each of its lines and no further"
    ;; the newline's face is what reaches the window edge
    (let ((text (html-tests-render html-tests-code)))
      (expect (html-tests-faces text "\"yes\")\n" 6) :to-equal '(mail-code-block))
      (expect (html-tests-faces text "(indented)\n" 10) :to-equal '(mail-code-block))
      (expect (html-tests-faces text "(indented)\n\n" 11) :to-be nil)
      (expect (html-tests-faces text "first:\n" 6) :to-be nil)
      (expect (face-attribute 'mail-code-block :extend) :to-be t)
      (expect (face-attribute 'mail-code :extend) :not :to-be t)))
  (it "sets code in fixed pitch, so it lines up in a variable pitch buffer"
    (expect (face-attribute 'mail-code :inherit) :to-be 'fixed-pitch)
    (expect (face-attribute 'mail-code-block :inherit) :to-be 'mail-code))
  (it "keeps a pre that holds no code as written, in fixed pitch on no background"
    (let ((text (html-tests-render "<pre>  plain\n    text\n</pre><p>after</p>")))
      (expect (substring-no-properties text) :to-equal "  plain\n    text\n\nafter\n\n")
      (expect (html-tests-faces text "  plain") :to-equal '(fixed-pitch))
      (expect (html-tests-faces text "after") :to-be nil)))
  (it "highlights a code block as the group's mail-code-major-mode, in front of the block's face"
    (html-tests-in-group "nnmaildir+reddit:r.test" html-tests-emacs-lisp
      (let ((text (html-tests-render html-tests-code)))
        (expect (html-tests-faces text "setq answer")
                :to-equal '(font-lock-keyword-face mail-code-block))
        (expect (html-tests-faces text "\"yes\"")
                :to-equal '(font-lock-string-face mail-code-block))
        (expect (html-tests-faces text "(setq answer") :to-equal '(mail-code-block))
        ;; inline code stays one face
        (expect (html-tests-faces text "setq inline") :to-equal '(mail-code)))))
  (it "highlights nothing in a group without mail-code-major-mode, or outside Gnus"
    (dolist (group '("nnmaildir+reddit:r.other" nil))
      (html-tests-in-group group html-tests-emacs-lisp
        (expect (html-tests-faces (html-tests-render html-tests-code) "setq answer")
                :to-equal '(mail-code-block)))))
  (it "keeps a quoted code block's own indentation, where shr indents its first line and not the rest"
    (expect (substring-no-properties (html-tests-render html-tests-quoted-code))
            :to-equal (concat ">   (defun first ()\n>     (body))\n>\n> quoted\n>\n"
                              "> later\n>     indented\n\n")))
  (it "faces quoted code by its depth behind the block's face, its marker bare of code"
    (let ((text (html-tests-render html-tests-quoted-code)))
      (expect (html-tests-faces text "(body)") :to-equal '(mail-code-block message-cited-text-1))
      (expect (html-tests-faces text ">     (body)") :to-equal '(message-cited-text-1))
      (expect (html-tests-faces text "(body))\n" 7) :to-equal '(mail-code-block)))))

(describe "mail-shade"
  (it "mixes the share of the text color into the background"
    (html-tests-with-theme "#000000" "#ffffff"
      (expect (mail-shade 0.2) :to-equal "#333333"))
    (html-tests-with-theme "#ffffff" "#000000"
      (expect (mail-shade 0.2) :to-equal "#cccccc")))
  (it "gives nothing where the theme's colors are unknown"
    (html-tests-with-theme "unspecified-bg" "unspecified-fg"
      (expect (mail-shade 0.2) :to-be nil))))

(describe "shade-mail-code-face"
  (it "sets the background of code its share of the way to the text, blocks included"
    (let ((mail-code-shade 0.2))
      (html-tests-with-theme "#000000" "#ffffff"
        (shade-mail-code-face)
        (expect (face-attribute 'mail-code :background) :to-equal "#333333")
        (expect (face-attribute 'mail-code-block :background nil t) :to-equal "#333333"))))
  (it "leaves code without a background where the theme's colors are unknown"
    (html-tests-with-theme "unspecified-bg" "unspecified-fg"
      (set-face-attribute 'mail-code nil :background 'unspecified)
      (shade-mail-code-face)
      (expect (face-attribute 'mail-code :background) :to-be 'unspecified))))

;;; html-tests.el ends here
