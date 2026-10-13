;;; modules/email/autoload/compose.el -*- lexical-binding: t; -*-
;;; Commentary:
;; A message body holds Org markup.  At send, org-mime turns it into HTML
;; with prisma's Markdown as the text part, or the whole body becomes that
;; Markdown.  Quoted lines and the signature reach both parts as typed.
;;; Code:

(require 'message)
(require 'subr-x)
(require 'color)
(require 'shr-color)
(require 'xml)
(require 'org-mime)
(require 'prisma)

(defvar gnus-original-article-buffer)

(defvar mail-export-options
  '(:with-toc nil :section-numbers nil :with-sub-superscript nil
    :with-latex verbatim :with-broken-links mark)
  "Org export options a message body converts with.")

(defvar-local mail-format nil
  "How the message leaves: `html', `plain', or nil to send it as typed.")

(defvar-local mail-sent-body nil
  "Body the last conversion produced, so a resend does not convert it again.")

(defvar mail-markdown-part nil
  "Text part of the message under conversion.")

(defvar mail-signature-html nil
  "Signature of the message under conversion, as HTML.")

(defvar mail-code-background "#f6f8fa"
  "Background of code blocks in HTML mail, whatever the theme.")

(defconst mail-part-regexp "<#\\(part\\|mml\\|external\\|multipart\\)\\b"
  "Opening of an MML part, which no conversion may touch.")

;;; Which format

(defun mail-answered-format ()
  "Format of the mail this message answers: `html', `plain', or nil."
  (when-let* ((article (and message-reply-headers
                            (bound-and-true-p gnus-original-article-buffer)
                            (get-buffer gnus-original-article-buffer))))
    (with-current-buffer article
      (save-excursion
        (goto-char (point-min))
        (let ((case-fold-search t))
          (if (re-search-forward "^content-type:[ \t]*text/html" nil t)
              'html
            'plain))))))

(defun mail-format-tag ()
  "Mode line text naming the format the message leaves in."
  (pcase mail-format
    ('html " HTML")
    ('plain " Plain")
    (_ "")))

(defun show-mail-format ()
  "Name the message's format in the mode line, after the mode's name."
  (unless (consp mode-name)
    (setq mode-name (list mode-name '(:eval (mail-format-tag))))))

(defconst mail-format-header "X-Mail-Format"
  "Header that carries `mail-format' through a queued message.")

;;;###autoload
(defun save-mail-format ()
  "Write `mail-format' into a header, for the reopened message to read."
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (message-remove-header mail-format-header)
      (goto-char (point-max))
      (insert mail-format-header ": " (symbol-name (or mail-format 'none)) "\n"))))

(defun take-saved-mail-format ()
  "Remove the format header and return what it names, nil without one.
The value is `html', `plain', or `none' for a message sent as typed."
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (when-let* ((value (message-fetch-field mail-format-header)))
        (message-remove-header mail-format-header)
        (pcase value
          ("html" 'html)
          ("plain" 'plain)
          (_ 'none))))))

;;;###autoload
(defun prepare-mail-format-h ()
  "Pick the format a message composed in Gnus leaves in.
A reopened message keeps the format it was saved with; otherwise news
goes as plain text, a reply as the mail it answers, the rest as HTML."
  (let ((saved (take-saved-mail-format)))
    (setq mail-format (cond ((eq saved 'none) nil)
                            (saved)
                            ((message-news-p) 'plain)
                            ((mail-answered-format))
                            (t 'html))))
  (show-mail-format))

;;;###autoload
(defun toggle-mail-format ()
  "Flip the message between HTML and plain text."
  (interactive nil message-mode)
  (setq mail-format (if (eq mail-format 'plain) 'html 'plain))
  (show-mail-format)
  (force-mode-line-update)
  (message "The message leaves as %s"
           (if (eq mail-format 'plain) "plain text" "HTML")))

;;; The body

(defun mail-body ()
  "The message body."
  (save-excursion
    (message-goto-body)
    (buffer-substring-no-properties (point) (point-max))))

(defun replace-mail-body (text)
  "Make TEXT the message body."
  (save-excursion
    (message-goto-body)
    (delete-region (point) (point-max))
    (insert text)))

(defun mail-part-end (kind)
  "End of the KIND part whose opening tag starts before point.
The end takes the newline after the closing tag."
  (let ((depth 1)
        (tag (format "<#\\(/\\)?%s\\b[^>]*>" kind)))
    (while (and (< 0 depth) (re-search-forward tag nil 'move))
      (setq depth (if (match-beginning 1) (1- depth) (1+ depth))))
    (if (eq (char-after) ?\n) (1+ (point)) (point))))

(defun split-mail-body (body)
  "Split BODY into (TEXT SIGNATURE PARTS).
PARTS joins the MML parts BODY holds, SIGNATURE is what follows the
signature separator, nil without one, and TEXT is the rest."
  (with-temp-buffer
    (insert body)
    (let ((parts '())
          signature)
      (goto-char (point-min))
      (while (re-search-forward mail-part-regexp nil t)
        (let* ((start (match-beginning 0))
               (end (mail-part-end (match-string 1))))
          (push (buffer-substring start end) parts)
          (delete-region start end)))
      (goto-char (point-max))
      (when (re-search-backward message-signature-separator nil t)
        (setq signature (string-trim (buffer-substring (line-beginning-position 2) (point-max))))
        (delete-region (point) (point-max)))
      (list (string-trim-right (buffer-string))
            signature
            (string-join (nreverse parts))))))

(defun map-mail-quotes (text fn)
  "TEXT with each run of quoted lines replaced by FN called on the run."
  ;; the replacement goes where the match data says, so FN must not move it
  (replace-regexp-in-string "^\\(?:>.*\\(?:\n\\|\\'\\)\\)+"
                            (lambda (run) (save-match-data (funcall fn run)))
                            text t t))

;;; HTML

(defun mail-quote-html (run)
  "RUN of quoted lines as nested blockquotes, its text escaped.
A line's leading star or hash becomes an entity: inside the export
block Org would read the star as a heading and end the block there."
  (let ((open (format "<blockquote class=\"gmail_quote\" style=\"%s\">"
                      org-mime-beautify-quoted-mail-style))
        (depth 0)
        (html '()))
    (dolist (line (split-string (string-trim-right run "\n") "\n"))
      (let ((level 0))
        (while (string-match "\\`>[ \t]?" line)
          (setq line (substring line (match-end 0))
                level (1+ level)))
        (while (< depth level)
          (push open html)
          (setq depth (1+ depth)))
        (while (< level depth)
          (push "</blockquote>" html)
          (setq depth (1- depth)))
        (push (concat (replace-regexp-in-string
                       "\\`[*#]" (lambda (char) (format "&#%d;" (string-to-char char)))
                       (xml-escape-string line) t t)
                      "<br>")
              html)))
    (dotimes (_ depth)
      (push "</blockquote>" html))
    (string-join (nreverse html) "\n")))

(defun mail-quote-block (run)
  "RUN of quoted lines as an Org block that exports to HTML as is."
  (concat "\n#+begin_export html\n" (mail-quote-html run) "\n#+end_export\n\n"))

(defun mail-signature-to-html (signature)
  "SIGNATURE under its separator line, as HTML."
  (concat "<p>-- <br>\n"
          (mapconcat #'xml-escape-string (split-string signature "\n") "<br>\n")
          "</p>"))

(defun mail-css-color (color)
  "COLOR as a CSS hex value, or nil when it names no color."
  (when-let* ((rgb (and color (color-name-to-rgb color))))
    (apply #'color-rgb-to-hex (append rgb '(2)))))

(defun readable-code-color (color)
  "COLOR, darkened until it reads on `mail-code-background'.
A value that names no color stays as it is."
  ;; below 55, saturated colors end under 4.5:1
  (let ((shr-color-visible-luminance-min 55))
    (cadr (shr-color-visible mail-code-background color t))))

(defun style-mail-code-blocks ()
  "Give code blocks a light background, and colors that read on it.
The text keeps the theme's colors, darkened where they would not read."
  (goto-char (point-min))
  (org-mime-change-element-style
   "pre" (format "background-color: %s;%s padding: 0.6em; border-radius: 4px; overflow-x: auto;"
                 mail-code-background
                 (if-let* ((text (mail-css-color (face-foreground 'default nil t))))
                     (format " color: %s;" text)
                   "")))
  (goto-char (point-min))
  (while (search-forward "<pre" nil t)
    (let ((end (save-excursion (search-forward "</pre>" nil 'move) (point-marker))))
      (while (re-search-forward "[^-]color: *\\([^;\"]+\\)" end t)
        (replace-match (readable-code-color (match-string 1)) t t nil 1)))))

(defun finish-mail-html-h ()
  "Style code and quote blocks inline, and end the HTML with the signature.
Mail clients drop the CSS of Org's classes, so a quote block takes the
reply quotes' border."
  (style-mail-code-blocks)
  (goto-char (point-min))
  (while (search-forward "<blockquote>" nil t)
    (replace-match (format "<blockquote style=\"%s\">" org-mime-beautify-quoted-mail-style) t t))
  (when mail-signature-html
    (goto-char (point-max))
    (insert "\n" mail-signature-html "\n")))

(defun use-mail-markdown-part-h ()
  "Make the text part org-mime builds the Markdown of the message."
  (erase-buffer)
  (insert mail-markdown-part "\n"))

;;; Markdown

(defun mail-plain-links (text)
  "TEXT with each link that has no description reduced to its target.
A file link keeps only the file's name."
  (replace-regexp-in-string
   "\\[\\[\\([^]]+\\)\\]\\]"
   (lambda (link)
     (let ((target (match-string 1 link)))
       (if (string-prefix-p "file:" target)
           (file-name-nondirectory target)
         target)))
   text t t))

(defun mail-markdown (text)
  "TEXT rendered from Org to Markdown, its quoted lines kept as they are."
  (let* ((runs '())
         (org (map-mail-quotes text (lambda (run)
                                      (push (string-trim-right run "\n") runs)
                                      (format "\nmailquote%dmailquote\n\n" (length runs)))))
         (markdown (prisma-render 'markdown (prisma-parse 'org (mail-plain-links org)))))
    (setq runs (nreverse runs))
    (replace-regexp-in-string
     "^mailquote\\([0-9]+\\)mailquote$"
     (lambda (token) (nth (1- (string-to-number (match-string 1 token))) runs))
     markdown t t)))

(defun mail-signature-text (signature)
  "SIGNATURE under its separator line, or nothing without one."
  (if signature (concat "\n\n-- \n" signature) ""))

;;; Conversion at send

(defun mail-body-to-html ()
  "Turn the Org body into HTML with a Markdown text part, through org-mime."
  (pcase-let ((`(,text ,signature ,parts) (split-mail-body (mail-body))))
    (let ((mail-markdown-part (concat (mail-markdown text) (mail-signature-text signature)))
          (mail-signature-html (and signature (mail-signature-to-html signature)))
          (org-mime-export-options mail-export-options)
          (org-mime-beautify-quoted-mail-p nil)
          (org-mime-plain-text-hook (list #'use-mail-markdown-part-h))
          (org-mime-html-hook (list #'finish-mail-html-h))
          (org-export-use-babel nil)
          (org-html-checkbox-type 'unicode)
          ;; Org underlines through a class, whose CSS mail clients drop
          (org-html-text-markup-alist (cons '(underline . "<u>%s</u>") org-html-text-markup-alist))
          (mark-active nil))
      (replace-mail-body (concat (map-mail-quotes text #'mail-quote-block) "\n"))
      (org-mime-htmlize))
    (unless (string-empty-p parts)
      (goto-char (point-max))
      (insert "\n" parts))))

(defun mail-body-to-markdown ()
  "Make the body the Markdown of its Org text."
  (pcase-let ((`(,text ,signature ,parts) (split-mail-body (mail-body))))
    (replace-mail-body (concat (mail-markdown text) (mail-signature-text signature) "\n"
                               (unless (string-empty-p parts) (concat "\n" parts))))))

(defun convert-mail-body ()
  "Turn the body into what `mail-format' asks for, unless that is done."
  (unless (equal (mail-body) mail-sent-body)
    (if (eq mail-format 'plain)
        (mail-body-to-markdown)
      (mail-body-to-html))
    (setq mail-sent-body (mail-body))))

;;;###autoload
(defun send-converted-mail-a (fn &rest args)
  "Call FN with ARGS on the body `mail-format' asks for.
The confirmation comes before the conversion, and a send that stops
short gets the Org body back."
  (if (not mail-format)
      (apply fn args)
    (when message-confirm-send
      (or (y-or-n-p "Send message? ") (keyboard-quit)))
    (let ((source (mail-body))
          (message-confirm-send nil))
      (condition-case err
          (progn
            (convert-mail-body)
            (apply fn args))
        ((quit error)
         (replace-mail-body source)
         (signal (car err) (cdr err)))))))

;;; compose.el ends here
