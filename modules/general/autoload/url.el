;;; modules/general/autoload/url.el -*- lexical-binding: t; -*-

;; Link conversions are parse-once/render-per-target: a parser per source
;; format yields a (:url :label :beg :end) plist, a renderer per target
;; format turns that into text, and every bound command is a thin pairing
;; of the two.

(require 'org)
(require 'org-element)
(require 'bug-reference)
(require 'thingatpt)

;;; link detection (embark target finders)

;;;###autoload
(defun url-get-link-type ()
  "Type and bounds of the link at point, nil when there is none.
Returns (TYPE . (BEG . END)) with TYPE one of org-mode, markdown,
plain or bug-reference.  Branch order is precedence: a url inside an
org or markdown link must not be targeted as plain, and an
org/repo#N-shaped fragment inside a url must not be targeted as a bug
reference."
  (cond
   ((when-let* ((pos (org-in-regexp org-link-bracket-re 1))
                (_ (string-match-p "\\`https?://" (match-string-no-properties 1))))
      (cons 'org-mode pos)))

   ((when-let* ((pos (org-in-regexp "<\\(https?://[^>]+\\)>\\|\\[\\([^]]+\\)\\]\\((https?://[^)]+)\\)" 1)))
      (cons 'markdown pos)))

   ((when-let* ((bounds (bounds-of-thing-at-point 'url)))
      (cons 'plain bounds)))

   ((when-let* ((pos (org-in-regexp bug-reference-bug-regexp 1)))
      (cons 'bug-reference pos)))))

;;;###autoload
(defun embark-target-markdown-link-at-point ()
  "Target markdown link at point."
  (when-let* ((type+pos (url-get-link-type))
              (_ (eq 'markdown (car type+pos)))
              (beg (cadr type+pos))
              (end (cddr type+pos)))
    `(markdown-link ,(buffer-substring-no-properties beg end) . ,(cons beg end))))

;;;###autoload
(defun embark-target-bug-reference-link-at-point ()
  "Target bug-reference link at point."
  (when-let* ((type+pos (url-get-link-type))
              (_ (eq 'bug-reference (car type+pos)))
              (beg (cadr type+pos))
              (end (cddr type+pos)))
    `(bug-reference-link ,(buffer-substring-no-properties beg end) . ,(cons beg end))))

;;;###autoload
(defun embark-target-RFC-number-at-point ()
  "Target RFC number at point.
anything like: RFC 123, rfc-123, RFC123 or rfc123."
  (when-let* ((rfc-pattern "\\b[rR][fF][cC][- ]?[0-9]+\\b")
              (bounds (org-in-regexp rfc-pattern 1))
              (beg (car bounds))
              (end (cdr bounds)))
    `(rfc-number ,(buffer-substring-no-properties beg end)
      . ,(cons beg end))))

;;; link parsers, one per source format

(defun parse-org-link-at-point ()
  "Org link at point as a (:url :label :beg :end) plist, nil when absent.
The element's :end includes post-blank whitespace; trim it so a
conversion does not swallow the space after the link."
  (when-let* ((ctx (org-element-lineage (org-element-context) '(link) t))
              (url (org-element-property :raw-link ctx))
              (beg (org-element-property :begin ctx))
              (end (- (org-element-property :end ctx)
                      (or (org-element-property :post-blank ctx) 0))))
    (let* ((cbeg (org-element-property :contents-begin ctx))
           (cend (org-element-property :contents-end ctx))
           (label (when (and cbeg cend (< cbeg cend))
                    (replace-regexp-in-string
                     "[ \n]+" " "
                     (string-trim
                      (buffer-substring-no-properties cbeg cend))))))
      (list :url url :label label :beg beg :end end))))

(defun parse-markdown-link-at-point ()
  "Markdown link at point as a (:url :label :beg :end) plist, nil when absent.
Autolinks (<url>) carry no label and never reach `markdown-link-at-pos',
which does not understand them."
  (when-let* ((ref (embark-target-markdown-link-at-point))
              (text (nth 1 ref))
              (bounds (nthcdr 2 ref)))
    (if (string-prefix-p "<" text)
        (list :url (substring text 1 -1) :label nil
              :beg (car bounds) :end (cdr bounds))
      (when-let* ((md (markdown-link-at-pos (point)))
                  (url (nth 3 md)))
        (list :url url :label (nth 2 md)
              :beg (car bounds) :end (cdr bounds))))))

(defun parse-plain-link-at-point ()
  "Plain url at point as a (:url :label :beg :end) plist, nil when absent."
  (when-let* ((url (thing-at-point-url-at-point))
              (bounds (bounds-of-thing-at-point 'url)))
    (list :url url :label nil :beg (car bounds) :end (cdr bounds))))

(defun parse-bug-reference-at-point ()
  "Bug reference at point as a (:url :label :beg :end) plist, nil when absent.
The url comes from `bug-reference->github-url', already resolved to the
accurate /pull/ or /issues/ form."
  (when-let* ((ref (embark-target-bug-reference-link-at-point))
              (url (bug-reference->github-url (nth 1 ref)))
              (bounds (nthcdr 2 ref)))
    (list :url url :label nil :beg (car bounds) :end (cdr bounds))))

;;; renderers

(defun link-title (url &optional label)
  "LABEL when present, else a title fetched for URL.
GitHub items get their API title, anything else its page title; nil
when neither can be had."
  (or label
      (get-gh-item-title url)
      (org-cliplink-retrieve-title-synchronously url)))

(defun render-link-as (parts format)
  "Render link PARTS in FORMAT, nil when PARTS cannot express it.
FORMAT is one of: org, markdown, plain, bug-reference, text."
  (let ((url (plist-get parts :url))
        (label (plist-get parts :label)))
    (pcase format
      ('plain url)
      ('text label)
      ('bug-reference (github-url->bug-reference url))
      ('org (if-let* ((title (link-title url label)))
                (format "[[%s][%s]]" url title)
              (format "[[%s]]" url)))
      ('markdown (if-let* ((title (link-title url label)))
                     (format "[%s](%s)" title url)
                   (format "<%s>" url))))))

(defun convert-link-at-point (parse format)
  "Replace the link at point found by PARSE with its FORMAT rendition.
Does nothing when PARSE finds no link or FORMAT cannot be rendered
from it."
  (when-let* ((parts (funcall parse))
              (rendition (render-link-as parts format)))
    (delete-region (plist-get parts :beg) (plist-get parts :end))
    (insert rendition)))

;;; converter commands (the names are bound in the embark keymaps)

;;;###autoload
(defun link-org->link-markdown ()
  "Convert org link at point to markdown."
  (interactive)
  (convert-link-at-point #'parse-org-link-at-point 'markdown))

;;;###autoload
(defun link-org->link-plain ()
  "Convert org link at point to a plain url."
  (interactive)
  (convert-link-at-point #'parse-org-link-at-point 'plain))

;;;###autoload
(defun link-org->just-text ()
  "Strip org link at point down to its description."
  (interactive)
  (convert-link-at-point #'parse-org-link-at-point 'text))

;;;###autoload
(defun link-org->link-bug-reference ()
  "Convert org link at point to an org/repo#N reference."
  (interactive)
  (convert-link-at-point #'parse-org-link-at-point 'bug-reference))

;;;###autoload
(defun link-markdown->link-org-mode ()
  "Convert markdown link at point to org-mode."
  (interactive)
  (convert-link-at-point #'parse-markdown-link-at-point 'org))

;;;###autoload
(defun link-markdown->link-plain ()
  "Convert markdown link at point to a plain url."
  (interactive)
  (convert-link-at-point #'parse-markdown-link-at-point 'plain))

;;;###autoload
(defun link-markdown->just-text ()
  "Strip markdown link at point down to its label."
  (interactive)
  (convert-link-at-point #'parse-markdown-link-at-point 'text))

;;;###autoload
(defun link-markdown->link-bug-reference ()
  "Convert markdown link at point to an org/repo#N reference."
  (interactive)
  (convert-link-at-point #'parse-markdown-link-at-point 'bug-reference))

;;;###autoload
(defun link-plain->link-org-mode ()
  "Convert plain url at point to an org link."
  (interactive)
  (convert-link-at-point #'parse-plain-link-at-point 'org))

;;;###autoload
(defun link-plain->link-markdown ()
  "Convert plain url at point to a markdown link."
  (interactive)
  (convert-link-at-point #'parse-plain-link-at-point 'markdown))

;;;###autoload
(defun link-plain->link-bug-reference ()
  "Convert plain GitHub url at point to an org/repo#N reference."
  (interactive)
  (convert-link-at-point #'parse-plain-link-at-point 'bug-reference))

;;;###autoload
(defun link-bug-reference->link-org-mode ()
  "Convert org/repo#N reference at point to an org link."
  (interactive)
  (convert-link-at-point #'parse-bug-reference-at-point 'org))

;;;###autoload
(defun link-bug-reference->link-markdown ()
  "Convert org/repo#N reference at point to a markdown link."
  (interactive)
  (convert-link-at-point #'parse-bug-reference-at-point 'markdown))

;;;###autoload
(defun link-bug-reference->link-plain ()
  "Convert org/repo#N reference at point to a plain url."
  (interactive)
  (convert-link-at-point #'parse-bug-reference-at-point 'plain))

;;;###autoload
(defun link->link-bug-reference ()
  "Convert link at point to an org/repo#N reference.
Dispatches on the link under point, not the major mode: a bare url in a
markdown-derived buffer (gfm, eca-chat) is a plain link, and the
markdown parser would silently decline it."
  (interactive)
  (pcase (car (url-get-link-type))
    ('org-mode (link-org->link-bug-reference))
    ('markdown (link-markdown->link-bug-reference))
    (_ (link-plain->link-bug-reference))))

;;; the odd ones out

;;;###autoload
(defun link-org->roam-heading ()
  "Convert org link at point to a heading with ROAM_REFS property."
  (interactive)
  (when-let* ((parts (parse-org-link-at-point)))
    (let ((url (plist-get parts :url))
          (title (or (plist-get parts :label) (plist-get parts :url)))
          (id (org-id-new)))
      (delete-region (plist-get parts :beg) (plist-get parts :end))
      (insert "* " title "\n"
              ":PROPERTIES:\n"
              ":id:       " id "\n"
              ":roam_refs: " url "\n"
              ":END:\n"))))

;;;###autoload
(defun open-link-in-vlc ()
  "Open link at point in VLC player."
  (interactive)
  (when-let* ((ctx (org-element-context))
              (path (org-link-unescape
                     (org-element-property :path ctx))))
    ;; TODO: Add Linux version
    (let ((dir? (file-directory-p path)))
      (shell-command
       (concat "open -a VLC \"" path "\""
               (when dir? " --args --playlist-autostart"))))))
