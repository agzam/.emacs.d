;;; tests/email/compose-tests.el --- Org compose specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; prisma is not a test dep: compose.el's require must no-op, and the
;; specs stand in for its two calls
(provide 'prisma)

(load-module-file "modules/email/autoload/compose.el")

(defvar org-confirm-babel-evaluate)

(defvar compose-tests-ran nil
  "Set by the src block a converted message must never run.")

(defmacro compose-tests-with-prisma (&rest body)
  "Run BODY with prisma turning Org bold into Markdown bold.
Like prisma, the stand-in keeps one blank line between blocks and trims."
  `(cl-letf (((symbol-function 'prisma-parse) (lambda (_format text) text))
             ((symbol-function 'prisma-render)
              (lambda (_format ast)
                (string-trim
                 (replace-regexp-in-string
                  "\n\n\n+" "\n\n"
                  (replace-regexp-in-string "\\*\\([^*\n]+\\)\\*" "**\\1**" ast))))))
     ,@body))

(defun compose-tests-count (string regexp)
  "How many times REGEXP matches in STRING."
  (let ((count 0)
        (start 0))
    (while (string-match regexp string start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(defun compose-tests-rgb (color &optional _frame)
  "RGB of the hex COLOR read exactly, as a graphical frame reads it."
  (when-let* ((values (color-values-from-color-spec color)))
    (mapcar (lambda (value) (/ value 65535.0)) values)))

(defmacro compose-tests-with-exact-colors (&rest body)
  "Run BODY with colors read exactly; batch Emacs reads #d7d5d1 as white."
  `(cl-letf (((symbol-function 'color-name-to-rgb) #'compose-tests-rgb))
     ,@body))

(defun compose-tests-contrast (a b)
  "WCAG contrast ratio of the hex colors A and B."
  (cl-flet ((luminance (color)
              (cl-loop for c in (compose-tests-rgb color)
                       for weight in '(0.2126 0.7152 0.0722)
                       sum (* weight (if (<= c 0.04045)
                                         (/ c 12.92)
                                       (expt (/ (+ c 0.055) 1.055) 2.4))))))
    (let ((x (luminance a))
          (y (luminance b)))
      (/ (+ (max x y) 0.05) (+ (min x y) 0.05)))))

(defun compose-tests-unreadable (colors)
  "Those of COLORS under 4.5:1 against the code block."
  (seq-filter (lambda (color)
                (< (compose-tests-contrast color mail-code-background) 4.5))
              colors))

(defun compose-tests-colors (html)
  "Every text color HTML sets, in order."
  (let ((start 0)
        (colors '()))
    (while (string-match "[^-]color: *\\(#[[:xdigit:]]+\\)" html start)
      (push (match-string 1 html) colors)
      (setq start (match-end 0)))
    (nreverse colors)))

(defun compose-tests-hue (color)
  "Hue of the hex COLOR, 0 to 1."
  (car (apply #'color-rgb-to-hsl (compose-tests-rgb color))))

(defmacro compose-tests-with-message (body &rest forms)
  "Run FORMS in a message buffer whose body is BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (delay-mode-hooks (message-mode))
     (insert "From: a@example.com\nTo: b@example.com\nSubject: s\n"
             mail-header-separator "\n" ,body)
     ,@forms))

(defun compose-tests-part (body type)
  "The TYPE part of the alternative BODY holds."
  (let ((start (string-search (format "<#part type=%s>\n" type) body)))
    (substring body (+ start (length (format "<#part type=%s>\n" type)))
               (or (string-search "<#part type=" body (1+ start))
                   (string-search "<#/multipart>" body start)))))

(defconst compose-tests-org
  "Hi,

Some *bold*, /italic/, _under_, ~code~, +strike+ and [[https://gnu.org][a link]].

* A heading
- one
  1. nested
- [X] done

| a | b |
|---+---|
| 1 | 2 |

#+begin_src emacs-lisp
(message \"hi\")
#+end_src

#+begin_quote
An Org quote.
#+end_quote

snake_case stays.

On Mon, Bob wrote:
> Quoted *stars* & <tags>
> > deeper
>
> back up

-- 
Best regards,
Ag
"
  "A reply body holding each element the conversion handles.")

(describe "split-mail-body"
  (it "takes the signature and every MML part out of the text"
    (pcase-let ((`(,text ,signature ,parts)
                 (split-mail-body
                  (concat "Hello\n"
                          "<#part type=\"text/plain\" filename=\"/tmp/a.txt\" disposition=attachment>\n<#/part>\n"
                          "between\n"
                          "<#mml type=message/rfc822 disposition=inline>\nFrom: x\n\nforwarded\n<#/mml>\n"
                          "<#multipart type=mixed>\n<#multipart type=alternative>\n<#part type=text/plain>\nin\n<#/part>\n<#/multipart>\nout\n<#/multipart>\n"
                          "-- \nBest regards,\nAg\n"))))
      (expect text :to-equal "Hello\nbetween")
      (expect signature :to-equal "Best regards,\nAg")
      (expect parts :to-equal
              (concat "<#part type=\"text/plain\" filename=\"/tmp/a.txt\" disposition=attachment>\n<#/part>\n"
                      "<#mml type=message/rfc822 disposition=inline>\nFrom: x\n\nforwarded\n<#/mml>\n"
                      "<#multipart type=mixed>\n<#multipart type=alternative>\n<#part type=text/plain>\nin\n<#/part>\n<#/multipart>\nout\n<#/multipart>\n"))))
  (it "has no signature without a separator"
    (expect (cadr (split-mail-body "Hello\n-- not a separator\n")) :to-be nil)))

(describe "map-mail-quotes"
  (it "hands FN each run of quoted lines whole"
    (let (runs)
      (expect (map-mail-quotes "a\n> x\n> > y\nb\n> z" (lambda (run) (push run runs) "Q\n"))
              :to-equal "a\nQ\nb\nQ\n")
      (expect (nreverse runs) :to-equal '("> x\n> > y\n" "> z"))))
  (it "replaces the whole run even when FN matches regexps"
    (expect (map-mail-quotes "a\n> x\n> y\nb" (lambda (run) (string-match "y" run) "Q\n"))
            :to-equal "a\nQ\nb")))

(describe "mail-quote-html"
  (it "nests a blockquote per level and escapes the text"
    (let ((html (mail-quote-html "> one & <two>\n> > deeper\n>\n> back\n")))
      (expect html :to-match "one &amp; &lt;two&gt;<br>")
      (expect (compose-tests-count html "<blockquote") :to-be 2)
      (expect (compose-tests-count html "</blockquote>") :to-be 2)
      (expect html :to-match "deeper<br>\n</blockquote>\n<br>\nback<br>\n</blockquote>\\'"))))

(describe "mail-markdown"
  (it "renders the text through prisma and keeps quoted runs as typed"
    (compose-tests-with-prisma
     (let ((markdown (mail-markdown "Some *bold* text, [[https://gnu.org]].\n\nOn Mon, Bob wrote:\n> Quoted *stars*\n> > deeper\n\nAfter *it*.")))
       (expect markdown :to-match "Some \\*\\*bold\\*\\* text, https://gnu.org\\.")
       (expect markdown :to-match "^> Quoted \\*stars\\*\n> > deeper$")
       (expect markdown :to-match "After \\*\\*it\\*\\*")
       (expect markdown :not :to-match "mailquote"))))
  (it "reduces a link without a description to its target"
    (expect (mail-plain-links "see [[https://gnu.org]], [[file:/tmp/pic.png]] and [[https://x.org][X]]")
            :to-equal "see https://gnu.org, pic.png and [[https://x.org][X]]")))

(describe "mail-body-to-html"
  (it "builds an alternative of Markdown and HTML, signature in both"
    (compose-tests-with-prisma
     (compose-tests-with-message compose-tests-org
       (setq mail-format 'html)
       (mail-body-to-html)
       (let* ((body (mail-body))
              (text (compose-tests-part body "text/plain"))
              (html (compose-tests-part body "text/html")))
         (expect body :to-match "\\`<#multipart type=alternative>\n")
         (expect text :to-match "Some \\*\\*bold\\*\\*")
         (expect text :to-match "^> Quoted \\*stars\\* & <tags>$")
         (expect text :to-match "\n-- \nBest regards,\nAg\n*\\'")
         (expect html :to-match "<p>-- <br>\nBest regards,<br>\nAg</p>\n*\\'")))))
  (it "converts each Org element to its HTML"
    (compose-tests-with-prisma
     (compose-tests-with-message compose-tests-org
       (setq mail-format 'html)
       (mail-body-to-html)
       (let ((html (compose-tests-part (mail-body) "text/html")))
         (dolist (element '("<b>bold</b>" "<i>italic</i>" "<u>under</u>" "<code>code</code>" "<del>strike</del>"
                            "<a href=\"https://gnu.org\">a link</a>" "<h2 id=\"[^\"]+\">A heading</h2>"
                            "<ul class=\"org-ul\">" "<ol class=\"org-ol\">" "&#x2611; done"
                            "<table " "<pre style=\"background-color: #f6f8fa;[^\"]*\" class=\"src src-emacs-lisp\">"
                            "<blockquote style=\"[^\"]*border-left[^\"]*\">\n<p>\nAn Org quote"
                            "snake_case stays"))
           (expect html :to-match element))
         (expect html :not :to-match "table-of-contents")
         (expect html :not :to-match "section-number")
         (expect html :not :to-match "<sub>")))))
  (it "puts quoted lines in nested blockquotes exactly as typed"
    (compose-tests-with-prisma
     (compose-tests-with-message compose-tests-org
       (setq mail-format 'html)
       (mail-body-to-html)
       (let ((html (compose-tests-part (mail-body) "text/html")))
         (expect html :to-match "gmail_quote[^>]*>\nQuoted \\*stars\\* &amp; &lt;tags&gt;<br>\n<blockquote")
         (expect (compose-tests-count html "Quoted") :to-be 1)
         (expect html :not :to-match "<b>stars</b>")))))
  (it "keeps a quoted star or hash line inside the blockquote"
    ;; a rendered HTML mail quotes its list items as "* " lines
    (compose-tests-with-prisma
     (compose-tests-with-message
         "Answer.\n\nOn Mon, Bob wrote:\n> * a bullet\n> #+end_export\n> after\n"
       (setq mail-format 'html)
       (mail-body-to-html)
       (let ((html (compose-tests-part (mail-body) "text/html")))
         (expect html :to-match "gmail_quote[^>]*>\n&#42; a bullet<br>\n&#35;\\+end_export<br>\nafter<br>\n</blockquote>")
         (expect html :not :to-match "<h2")
         (expect html :not :to-match "begin_export")))))
  (it "keeps attachments and forwarded mail after the alternative"
    (compose-tests-with-prisma
     (compose-tests-with-message
         "Hello *there*\n<#part type=\"text/plain\" filename=\"/tmp/a.txt\" disposition=attachment>\n<#/part>\n-- \nAg\n"
       (setq mail-format 'html)
       (mail-body-to-html)
       (expect (mail-body) :to-match
               "<#/multipart>\n+<#part type=\"text/plain\" filename=\"/tmp/a.txt\" disposition=attachment>\n<#/part>\n\\'"))))
  (it "never runs a src block"
    (setq compose-tests-ran nil)
    (compose-tests-with-prisma
     (compose-tests-with-message
         "Hi\n\n#+begin_src emacs-lisp :exports results\n(setq compose-tests-ran t)\n#+end_src\n"
       (setq mail-format 'html)
       (let ((org-confirm-babel-evaluate nil))
         (mail-body-to-html))))
    (expect compose-tests-ran :to-be nil)))

(describe "mail-signature-to-html"
  (it "keeps the separator and the lines, escaped"
    (expect (mail-signature-to-html "A & <B>\nC")
            :to-equal "<p>-- <br>\nA &amp; &lt;B&gt;<br>\nC</p>")))

(describe "readable-code-color"
  :var ((light '("#d7d5d1" "#838083" "#3a81c3" "#2aa1ae" "#da8b55" "#ff00ff" "#ffffff")))
  (it "darkens a color until it reads at 4.5:1 or better on the code block"
    (compose-tests-with-exact-colors
     (expect (compose-tests-unreadable (mapcar #'readable-code-color light)) :to-equal nil)))
  (it "darkens it no further than about 5.6:1"
    (compose-tests-with-exact-colors
     (expect (seq-filter (lambda (color)
                           (< 6 (compose-tests-contrast (readable-code-color color) mail-code-background)))
                         light)
             :to-equal nil)))
  (it "keeps the color's hue"
    (compose-tests-with-exact-colors
     (dolist (color '("#3a81c3" "#2aa1ae" "#da8b55"))
       (expect (abs (- (compose-tests-hue color) (compose-tests-hue (readable-code-color color))))
               :to-be-less-than 0.03))))
  (it "leaves a color that reads already"
    (compose-tests-with-exact-colors
     (expect (readable-code-color "#655370") :to-equal "#655370")))
  (it "leaves a value that names no color"
    (compose-tests-with-exact-colors
     (expect (readable-code-color "inherit") :to-equal "inherit"))))

(describe "finish-mail-html-h"
  (it "gives code blocks a light background, the theme's text color and readable colors"
    (compose-tests-with-exact-colors
     (cl-letf (((symbol-function 'face-foreground) (lambda (&rest _) "#d7d5d1")))
       (with-temp-buffer
         (insert "<pre class=\"src\">(<span style=\"color: #838083; font-style: italic;\">doc</span> "
                 "<span style=\"color: #545053;\">x</span>)</pre>")
         (let ((mail-signature-html nil))
           (finish-mail-html-h))
         (let ((html (buffer-string)))
           (expect html :to-match
                   "\\`<pre style=\"background-color: #f6f8fa; color: #[[:xdigit:]]\\{6\\}; padding: [^\"]*\" class=\"src\">(<span style=\"color: #[[:xdigit:]]\\{6\\}; font-style: italic;\">doc</span> <span style=\"color: #545053;\">x</span>)</pre>\\'")
           (expect (length (compose-tests-colors html)) :to-be 3)
           (expect (compose-tests-unreadable (compose-tests-colors html)) :to-equal nil))))))
  (it "leaves colors outside code blocks alone"
    (compose-tests-with-exact-colors
     (cl-letf (((symbol-function 'face-foreground) (lambda (&rest _) "#655370")))
       (with-temp-buffer
         (insert "<p style=\"color: #eeeeee;\">a</p>\n<pre>b</pre>\n<p style=\"color: #dddddd;\">c</p>")
         (let ((mail-signature-html nil))
           (finish-mail-html-h))
         (expect (buffer-string) :to-match
                 "\\`<p style=\"color: #eeeeee;\">a</p>\n<pre style=\"[^\"]*\">b</pre>\n<p style=\"color: #dddddd;\">c</p>\\'")))))
  (it "gives code blocks the light background when the theme names no text color"
    (compose-tests-with-exact-colors
     (cl-letf (((symbol-function 'face-foreground) (lambda (&rest _) "unspecified-fg")))
       (with-temp-buffer
         (insert "<pre class=\"src\">x</pre>")
         (let ((mail-signature-html nil))
           (finish-mail-html-h))
         (expect (buffer-string) :to-equal
                 "<pre style=\"background-color: #f6f8fa; padding: 0.6em; border-radius: 4px; overflow-x: auto;\" class=\"src\">x</pre>")))))
  (it "gives a quote block the reply quotes' border, and leaves those alone"
    (with-temp-buffer
      (insert "<blockquote>\n<p>x</p>\n</blockquote>\n<blockquote class=\"gmail_quote\" style=\"a\">y</blockquote>")
      (let ((mail-signature-html nil))
        (finish-mail-html-h))
      (expect (buffer-string) :to-equal
              (concat (format "<blockquote style=\"%s\">\n<p>x</p>\n</blockquote>\n"
                              org-mime-beautify-quoted-mail-style)
                      "<blockquote class=\"gmail_quote\" style=\"a\">y</blockquote>")))))

(describe "mail-body-to-markdown"
  (it "turns the body into Markdown, signature and parts after it"
    (compose-tests-with-prisma
     (compose-tests-with-message
         "Plain *bold*\n<#part type=\"text/plain\" filename=\"/tmp/a.txt\" disposition=attachment>\n<#/part>\n\n> quoted *line*\n-- \nAg\n"
       (mail-body-to-markdown)
       (expect (mail-body) :to-equal
               (concat "Plain **bold**\n\n> quoted *line*\n\n-- \nAg\n\n"
                       "<#part type=\"text/plain\" filename=\"/tmp/a.txt\" disposition=attachment>\n<#/part>\n"))))))

(describe "convert-mail-body"
  (it "converts a body once"
    (compose-tests-with-prisma
     (compose-tests-with-message "Hello *there*\n"
       (setq mail-format 'html)
       (convert-mail-body)
       (convert-mail-body)
       (expect (compose-tests-count (mail-body) "<#multipart type=alternative>") :to-be 1))))
  (it "converts again once the body changed"
    (compose-tests-with-prisma
     (compose-tests-with-message "Hello *there*\n"
       (setq mail-format 'plain)
       (convert-mail-body)
       (replace-mail-body "Again *now*\n")
       (convert-mail-body)
       (expect (mail-body) :to-equal "Again **now**\n")))))

(describe "send-converted-mail-a"
  (it "sends a message composed outside Gnus as typed"
    (compose-tests-with-message "Hello *there*\n"
      (let ((sent nil))
        (send-converted-mail-a (lambda () (setq sent (mail-body))))
        (expect sent :to-equal "Hello *there*\n"))))
  (it "asks before converting, so a no leaves the Org body"
    (compose-tests-with-prisma
     (compose-tests-with-message "Hello *there*\n"
       (setq mail-format 'html)
       (let ((message-confirm-send t)
             (sent nil))
         (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil))
                   ((symbol-function 'keyboard-quit) (lambda () (error "Quit"))))
           (expect (send-converted-mail-a (lambda () (setq sent t))) :to-throw 'error))
         (expect sent :to-be nil)
         (expect (mail-body) :to-equal "Hello *there*\n")))))
  (it "sends the converted body without asking a second time"
    (compose-tests-with-prisma
     (compose-tests-with-message "Hello *there*\n"
       (setq mail-format 'plain)
       (let ((message-confirm-send t)
             asked-again sent)
         (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
           (send-converted-mail-a (lambda ()
                                    (setq asked-again message-confirm-send
                                          sent (mail-body)))))
         (expect asked-again :to-be nil)
         (expect sent :to-equal "Hello **there**\n")))))
  (it "gives the Org body back when the send fails"
    (compose-tests-with-prisma
     (compose-tests-with-message "Hello *there*\n"
       (setq mail-format 'html)
       (let ((message-confirm-send nil))
         (expect (send-converted-mail-a (lambda () (error "SMTP down"))) :to-throw 'error)
         (expect (mail-body) :to-equal "Hello *there*\n")))))
  (it "gives the Org body back when the conversion fails"
    (compose-tests-with-message "Hello *there*\n> quoted\n"
      (setq mail-format 'html)
      (let ((message-confirm-send nil))
        (cl-letf (((symbol-function 'org-mime-htmlize) (lambda () (error "No such image"))))
          (compose-tests-with-prisma
           (expect (send-converted-mail-a #'ignore) :to-throw 'error)))
        (expect (mail-body) :to-equal "Hello *there*\n> quoted\n")))))

(describe "prepare-mail-format-h"
  :var (original)
  (before-each
    (setq original (get-buffer-create " *compose-tests original*")))
  (after-each
    (kill-buffer original))
  (it "sends news as plain text"
    (compose-tests-with-message ""
      (save-excursion
        (goto-char (point-min))
        (insert "Newsgroups: gmane.emacs.devel\n"))
      (prepare-mail-format-h)
      (expect mail-format :to-be 'plain)))
  (it "answers mail in the format it came in"
    (let ((gnus-original-article-buffer (buffer-name original)))
      (dolist (case '(("Content-Type: multipart/alternative; boundary=x\n\n--x\nContent-Type: text/plain\n\nhi\n--x\nContent-Type: text/html; charset=utf-8\n\n<p>hi</p>\n--x--\n" . html)
                      ("Content-Type: text/plain; charset=utf-8\n\nhi\n" . plain)))
        (with-current-buffer original
          (erase-buffer)
          (insert (car case)))
        (compose-tests-with-message ""
          (setq-local message-reply-headers (vector 1 "Re: s"))
          (prepare-mail-format-h)
          (expect mail-format :to-be (cdr case))))))
  (it "starts a new message as HTML, even with a mail on display"
    (let ((gnus-original-article-buffer (buffer-name original)))
      (with-current-buffer original
        (insert "Content-Type: text/plain\n\nhi\n"))
      (compose-tests-with-message ""
        (prepare-mail-format-h)
        (expect mail-format :to-be 'html))))
  (it "names the format after the mode's name"
    ;; batch Emacs formats no mode line; the e2e reads the real one
    (compose-tests-with-message ""
      (prepare-mail-format-h)
      (expect mode-name :to-equal '("Message" (:eval (mail-format-tag))))
      (expect (mail-format-tag) :to-equal " HTML")
      (toggle-mail-format)
      (expect (mail-format-tag) :to-equal " Plain")
      (prepare-mail-format-h)
      (expect mode-name :to-equal '("Message" (:eval (mail-format-tag)))))))

(describe "toggle-mail-format"
  (it "flips between HTML and plain text"
    (compose-tests-with-message ""
      (setq mail-format 'html)
      (toggle-mail-format)
      (expect mail-format :to-be 'plain)
      (toggle-mail-format)
      (expect mail-format :to-be 'html))))

;;; compose-tests.el ends here
