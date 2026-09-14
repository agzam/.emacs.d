;;; modules/general/autoload/code-snippet.el --- code written inline in prose -*- lexical-binding: t; -*-

;; Code written inline in prose: org's ~code~ and =verbatim=, markdown's
;; `code' and its fenced blocks.  One extractor per syntax yields
;; (TEXT BEG . END) - the code without its delimiters, over the bounds the
;; delimiters included - and `embark-target-code-snippet' turns that into a
;; target.  Org src blocks stay with `embark-target-org-block'.

(require 'org-element)
(require 'subr-x)

(declare-function markdown-inline-code-at-point "markdown-mode" ())

(defconst markdown-code-fence-regexp "^[ \t]*\\(?:```\\|~~~\\)"
  "Match the opening or the closing line of a markdown fenced code block.")

(defun org-inline-code-snippet ()
  "Org inline code or verbatim at point as (TEXT BEG . END).
The element's :end runs past the closing delimiter by its post-blank
whitespace, so take that back off."
  (when-let* ((ctx (org-element-context))
              ((memq (org-element-type ctx) '(code verbatim))))
    (cons (org-element-property :value ctx)
          (cons (org-element-property :begin ctx)
                (- (org-element-property :end ctx)
                   (or (org-element-property :post-blank ctx) 0))))))

(defun markdown-inline-code-snippet ()
  "Markdown inline code at point as (TEXT BEG . END).
`markdown-inline-code-at-point' leaves the code in group 2 and the
backtick runs around it in groups 1 and 3."
  (when (markdown-inline-code-at-point)
    (cons (match-string-no-properties 2)
          (cons (match-beginning 0) (match-end 0)))))

(defun markdown-fenced-code-snippet ()
  "Markdown fenced code block at point as (TEXT BEG . END).
Font-lock owns `markdown-code-block-at-point-p', so an unfontified
buffer reads as blockless there; pairing the fence lines does not care."
  (save-excursion
    (let ((pos (point))
          (case-fold-search nil)
          open found)
      (goto-char (point-min))
      (while (and (not found)
                  (re-search-forward markdown-code-fence-regexp nil t))
        (if (null open)
            (setq open (cons (line-beginning-position)
                             (min (point-max) (1+ (line-end-position)))))
          (when (and (<= (car open) pos) (<= pos (line-end-position)))
            (setq found (cons (string-trim-right
                               (buffer-substring-no-properties
                                (cdr open) (line-beginning-position))
                               "\n+")
                              (cons (car open) (line-end-position)))))
          (setq open nil)))
      found)))

;;;###autoload
(defun code-snippet-at-point ()
  "Code snippet written at point as (TEXT BEG . END), nil when there is none."
  (cond
   ((derived-mode-p 'org-mode)
    (org-inline-code-snippet))
   ((derived-mode-p 'markdown-mode)
    (or (markdown-inline-code-snippet)
        (markdown-fenced-code-snippet)))))

;;;###autoload
(defun embark-target-code-snippet ()
  "Target the code snippet at point."
  (when-let* ((snippet (code-snippet-at-point)))
    `(code-snippet ,(car snippet) . ,(cdr snippet))))
