;;; tests/general/url-tests.el --- general/autoload/url.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'org)
(require 'bug-reference)

;; github-url->bug-reference (git module) backs the bug-reference renderer;
;; the git module's installer supplies the production bug-reference regexp.
(load-module-file "modules/git/autoload/misc.el")
(load-module-file "modules/git/autoload/bug-reference.el")
(load-module-file "modules/general/autoload/url.el")

;; markdown-mode is absent from the batch harness; the converters only need
;; a mode to derive from and `markdown-link-at-pos'.
(unless (fboundp 'markdown-mode)
  (define-derived-mode markdown-mode text-mode "Markdown"))
(define-derived-mode chat-like-mode markdown-mode "ChatLike")

(defun stub-markdown-link-at-pos (pos)
  "Bounds, label and url of the inline markdown link covering POS.
Stands in for `markdown-link-at-pos', reading the buffer rather than
returning canned offsets, so specs stay honest about their own text."
  (save-excursion
    (goto-char (line-beginning-position))
    (catch 'found
      (while (re-search-forward "\\[\\([^]]*\\)\\](\\([^)]*\\))" (line-end-position) t)
        (when (and (<= (match-beginning 0) pos) (<= pos (match-end 0)))
          (throw 'found (list (match-beginning 0) (match-end 0)
                              (match-string-no-properties 1)
                              (match-string-no-properties 2)
                              nil nil)))))))

;; NOTE buttercup :var slots are lexical - dynamic lets keep the
;; bug-reference regexp scoped to each spec (git-suite precedent).

(defmacro with-live-buffer (mode text &rest body)
  "Run BODY in a MODE buffer holding TEXT, point at start, bug references live.
Fontification is what makes a buffer live here: only once
`bug-reference-fontify' has run does the mode own thingatpt's url
provider through its overlays, which is the state every real buffer is
in - and the state a bare `with-temp-buffer' never reaches.  The mode is
enabled before the settings so auto-setup cannot outvote the regexp."
  (declare (indent 2))
  `(let ((bug-reference-bug-regexp bug-reference-bug-regexp)
         (bug-reference-url-format bug-reference-url-format))
     (with-temp-buffer
       (funcall (or ,mode #'fundamental-mode))
       (insert ,text)
       (bug-reference-mode 1)
       (init-bug-reference-mode-settings)
       (bug-reference-fontify (point-min) (point-max))
       (goto-char (point-min))
       ,@body)))

(defmacro with-title-stubs (gh-title page-title &rest body)
  "Run BODY with both title fetchers stubbed, never touching the network."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'get-gh-item-title)
              (lambda (&rest _) ,gh-title))
             ((symbol-function 'org-cliplink-retrieve-title-synchronously)
              (lambda (&rest _) ,page-title)))
     ,@body))

(defmacro with-markdown-stub (&rest body)
  "Run BODY with `markdown-link-at-pos' answered by the buffer-reading stub."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'markdown-link-at-pos) #'stub-markdown-link-at-pos))
     ,@body))

;;; detection

(describe "plain-url-at-point"
  (it "spans the whole url, not just the scheme"
    (with-live-buffer nil "see https://example.com/some/path now"
      (search-forward "some")
      (let ((url+bounds (plain-url-at-point)))
        (expect (car url+bounds) :to-equal "https://example.com/some/path")
        (expect (buffer-substring-no-properties
                 (cadr url+bounds) (cddr url+bounds))
                :to-equal "https://example.com/some/path"))))

  ;; `bug-reference-mode' registers itself as thingatpt's url provider, so
  ;; an unguarded `thing-at-point' answers for a reference with a url that
  ;; is nowhere in the buffer - and truncates a real url to the ref-shaped
  ;; fragment inside it.  Both would make a conversion rewrite the wrong
  ;; region with the wrong url.
  (it "sees no url in a bare reference the mode claims as one"
    (with-live-buffer nil "fix agzam/foo#12 soon"
      (search-forward "#12")
      (expect (thing-at-point 'url) :to-equal "https://github.com/agzam/foo/issues/12")
      (expect (plain-url-at-point) :to-be nil)))

  (it "keeps the whole url when a ref-shaped fragment sits inside it"
    (with-live-buffer nil "see https://github.com/agzam/foo#4 here"
      (search-forward "agzam")
      (expect (car (plain-url-at-point))
              :to-equal "https://github.com/agzam/foo#4"))))

;; NOTE assert the type on its own: a `pcase-let' backquote pattern binds
;; the bounds whether or not the literal head matches, so a spec written
;; that way passes on the wrong type as long as the bounds line up.
(describe "url-get-link-type"
  (it "spans the whole url for plain links, not just the scheme"
    (with-live-buffer nil "see https://example.com/some/path now"
      (search-forward "some")
      (let ((type+pos (url-get-link-type)))
        (expect (car type+pos) :to-equal 'plain)
        (expect (buffer-substring-no-properties (cadr type+pos) (cddr type+pos))
                :to-equal "https://example.com/some/path"))))

  (it "types a bare reference as a bug reference"
    (with-live-buffer nil "fix agzam/foo#12 soon"
      (search-forward "#12")
      (let ((type+pos (url-get-link-type)))
        (expect (car type+pos) :to-equal 'bug-reference)
        (expect (buffer-substring-no-properties (cadr type+pos) (cddr type+pos))
                :to-equal "agzam/foo#12"))))

  (it "types a ref-shaped fragment inside a url as plain"
    (with-live-buffer nil "see https://github.com/agzam/foo#4 here"
      (search-forward "agzam")
      (let ((type+pos (url-get-link-type)))
        (expect (car type+pos) :to-equal 'plain)
        (expect (buffer-substring-no-properties (cadr type+pos) (cddr type+pos))
                :to-equal "https://github.com/agzam/foo#4"))))

  (it "types a url inside an org link as org, not plain"
    (with-live-buffer #'org-mode "pre [[https://example.com][x]] post"
      (search-forward "example")
      (expect (car (url-get-link-type)) :to-equal 'org-mode)))

  (it "types a url inside a markdown link as markdown, not plain"
    (with-live-buffer #'markdown-mode "pre [x](https://example.com) post"
      (search-forward "example")
      (expect (car (url-get-link-type)) :to-equal 'markdown))))

(describe "embark-target-bug-reference-link-at-point"
  (it "targets an org/repo#N reference with its bounds"
    (with-live-buffer nil "fix agzam/foo#12 soon"
      (search-forward "#12")
      (let ((ref (embark-target-bug-reference-link-at-point)))
        (expect (car ref) :to-equal 'bug-reference-link)
        (expect (nth 1 ref) :to-equal "agzam/foo#12")
        (expect (buffer-substring-no-properties (nth 2 ref) (cdddr ref))
                :to-equal "agzam/foo#12"))))

  (it "returns nil off-reference"
    (with-live-buffer nil "plain words only"
      (expect (embark-target-bug-reference-link-at-point) :to-be nil)))

  (it "does not mis-target an org/repo#N-shaped fragment inside a url"
    (with-live-buffer nil "https://github.com/agzam/foo#4"
      (search-forward "agzam")
      (expect (embark-target-bug-reference-link-at-point) :to-be nil))))

(describe "embark-target-markdown-link-at-point"
  (it "targets an inline link with its bounds"
    (with-live-buffer #'markdown-mode "pre [x](https://example.com) post"
      (search-forward "example")
      (let ((ref (embark-target-markdown-link-at-point)))
        (expect (car ref) :to-equal 'markdown-link)
        (expect (nth 1 ref) :to-equal "[x](https://example.com)")
        (expect (buffer-substring-no-properties (nth 2 ref) (cdddr ref))
                :to-equal "[x](https://example.com)"))))

  (it "targets an autolink"
    (with-live-buffer #'markdown-mode "pre <https://example.com> post"
      (search-forward "example")
      (let ((ref (embark-target-markdown-link-at-point)))
        (expect (car ref) :to-equal 'markdown-link)
        (expect (nth 1 ref) :to-equal "<https://example.com>")))))

;;; parsers

(describe "markdown-link-url-at-point"
  (it "reads the url out of an inline link"
    (with-markdown-stub
      (with-live-buffer #'markdown-mode "pre [x](https://example.com/a) post"
        (search-forward "example")
        (expect (markdown-link-url-at-point) :to-equal "https://example.com/a"))))

  (it "reads the url out of an autolink"
    (with-live-buffer #'markdown-mode "pre <https://example.com/b> post"
      (search-forward "example")
      (expect (markdown-link-url-at-point) :to-equal "https://example.com/b")))

  (it "returns nil off-link"
    (with-live-buffer #'markdown-mode "no link here"
      (expect (markdown-link-url-at-point) :to-be nil))))

(describe "open-org-link-in-emacs"
  (it "routes a web link through the url handler table"
    (let (handled)
      (cl-letf (((symbol-function 'process-external-url)
                 (lambda (url) (setq handled url)))
                ((symbol-function 'org-open-at-point)
                 (lambda (&rest _) (error "org must not open a web link"))))
        (open-org-link-in-emacs "https://github.com/agzam/foo/pull/12"))
      (expect handled :to-equal "https://github.com/agzam/foo/pull/12")))

  (it "lets org open every other link type"
    (let (opened)
      (cl-letf (((symbol-function 'org-open-at-point)
                 (lambda (&rest _) (setq opened t)))
                ((symbol-function 'process-external-url)
                 (lambda (&rest _) (error "the url table must not see an id link"))))
        (open-org-link-in-emacs "[[id:abc-123][a note]]"))
      (expect opened :to-be t))))

(describe "parse-org-link-at-point"
  (it "extracts url, label and bounds without the trailing blank"
    (with-live-buffer #'org-mode "pre [[https://example.com][Ex ample]] post"
      (search-forward "Ex")
      (let ((parts (parse-org-link-at-point)))
        (expect (plist-get parts :url) :to-equal "https://example.com")
        (expect (plist-get parts :label) :to-equal "Ex ample")
        (expect (buffer-substring-no-properties
                 (plist-get parts :beg) (plist-get parts :end))
                :to-equal "[[https://example.com][Ex ample]]")))))

(describe "parse-plain-link-at-point"
  (it "reads the url written in the buffer, not the one a reference implies"
    (with-live-buffer nil "see https://github.com/agzam/foo#4 here"
      (search-forward "agzam")
      (let ((parts (parse-plain-link-at-point)))
        (expect (plist-get parts :url) :to-equal "https://github.com/agzam/foo#4")
        (expect (buffer-substring-no-properties
                 (plist-get parts :beg) (plist-get parts :end))
                :to-equal "https://github.com/agzam/foo#4")))))

(describe "parse-bug-reference-at-point"
  (it "resolves the reference to its GitHub url"
    (with-live-buffer nil "fix agzam/foo#12 soon"
      (search-forward "#12")
      (let ((parts (parse-bug-reference-at-point)))
        (expect (plist-get parts :url)
                :to-equal "https://github.com/agzam/foo/issues/12")
        (expect (buffer-substring-no-properties
                 (plist-get parts :beg) (plist-get parts :end))
                :to-equal "agzam/foo#12")))))

;;; conversion matrix: every source format to every target it can express

(describe "org link converts to"
  (it "markdown, reusing the existing description as the label"
    (with-live-buffer #'org-mode "pre [[https://example.com][desc]] post"
      (search-forward "desc")
      (with-title-stubs nil nil
        (link-org->link-markdown))
      (expect (buffer-string) :to-equal "pre [desc](https://example.com) post")))

  (it "plain, preserving surrounding spacing"
    (with-live-buffer #'org-mode "pre [[https://example.com][x]] post"
      (search-forward "x]]")
      (link-org->link-plain)
      (expect (buffer-string) :to-equal "pre https://example.com post")))

  (it "just text, keeping only the description"
    (with-live-buffer #'org-mode "pre [[https://example.com][desc]] post"
      (search-forward "desc")
      (link-org->just-text)
      (expect (buffer-string) :to-equal "pre desc post")))

  (it "a bug reference, from a github pull link"
    (with-live-buffer #'org-mode "pre [[https://github.com/agzam/foo/pull/4][x]] post"
      (search-forward "pull")
      (link-org->link-bug-reference)
      (expect (buffer-string) :to-equal "pre agzam/foo#4 post"))))

(describe "markdown link converts to"
  (it "org, reusing the label and asking markdown-link-at-pos only once"
    (let ((calls 0))
      (cl-letf (((symbol-function 'markdown-link-at-pos)
                 (lambda (pos)
                   (cl-incf calls)
                   (stub-markdown-link-at-pos pos))))
        (with-live-buffer #'markdown-mode "pre [label](https://example.com) post"
          (search-forward "label")
          (with-title-stubs nil nil
            (link-markdown->link-org-mode))
          (expect (buffer-string)
                  :to-equal "pre [[https://example.com][label]] post")
          (expect calls :to-equal 1)))))

  (it "org from an autolink, via a fetched title"
    (with-live-buffer #'markdown-mode "pre <https://example.com> post"
      (search-forward "example")
      (with-title-stubs nil "Example Site"
        (link-markdown->link-org-mode))
      (expect (buffer-string)
              :to-equal "pre [[https://example.com][Example Site]] post")))

  (it "plain"
    (with-markdown-stub
      (with-live-buffer #'markdown-mode "pre [label](https://example.com) post"
        (search-forward "label")
        (link-markdown->link-plain)
        (expect (buffer-string) :to-equal "pre https://example.com post"))))

  (it "plain from an autolink"
    (with-live-buffer #'markdown-mode "pre <https://example.com> post"
      (search-forward "example")
      (link-markdown->link-plain)
      (expect (buffer-string) :to-equal "pre https://example.com post")))

  (it "just text, keeping only the label"
    (with-markdown-stub
      (with-live-buffer #'markdown-mode "pre [label](https://example.com) post"
        (search-forward "label")
        (link-markdown->just-text)
        (expect (buffer-string) :to-equal "pre label post"))))

  (it "a bug reference, from a github pull link"
    (with-markdown-stub
      (with-live-buffer #'markdown-mode "pre [x](https://github.com/agzam/foo/pull/4) post"
        (search-forward "pull")
        (link-markdown->link-bug-reference)
        (expect (buffer-string) :to-equal "pre agzam/foo#4 post")))))

(describe "plain url converts to"
  (it "org via the page title"
    (with-live-buffer nil "pre https://example.com post"
      (search-forward "example")
      (with-title-stubs nil "Example Site"
        (link-plain->link-org-mode))
      (expect (buffer-string)
              :to-equal "pre [[https://example.com][Example Site]] post")))

  (it "org, preferring the github item title for github urls"
    (with-live-buffer nil "pre https://github.com/agzam/foo/pull/4 post"
      (search-forward "pull")
      (with-title-stubs "agzam/foo#4 — Title" nil
        (link-plain->link-org-mode))
      (expect (buffer-string)
              :to-equal
              "pre [[https://github.com/agzam/foo/pull/4][agzam/foo#4 — Title]] post")))

  (it "a bare org link when no title can be fetched"
    (with-live-buffer nil "pre https://example.com post"
      (search-forward "example")
      (with-title-stubs nil nil
        (link-plain->link-org-mode))
      (expect (buffer-string) :to-equal "pre [[https://example.com]] post")))

  (it "markdown"
    (with-live-buffer nil "pre https://example.com post"
      (search-forward "example")
      (with-title-stubs nil "Example Site"
        (link-plain->link-markdown))
      (expect (buffer-string)
              :to-equal "pre [Example Site](https://example.com) post")))

  (it "a markdown autolink when no title can be fetched"
    (with-live-buffer nil "pre https://example.com post"
      (search-forward "example")
      (with-title-stubs nil nil
        (link-plain->link-markdown))
      (expect (buffer-string) :to-equal "pre <https://example.com> post")))

  (it "markdown, keeping a url that ends in a ref-shaped fragment whole"
    (with-live-buffer nil "pre https://github.com/agzam/foo#4 post"
      (search-forward "agzam")
      (with-title-stubs nil nil
        (link-plain->link-markdown))
      (expect (buffer-string)
              :to-equal "pre <https://github.com/agzam/foo#4> post")))

  (it "a bug reference, from a github pull url"
    (with-live-buffer nil "pre https://github.com/agzam/foo/pull/4 post"
      (search-forward "pull")
      (link-plain->link-bug-reference)
      (expect (buffer-string) :to-equal "pre agzam/foo#4 post")))

  (it "nothing for non-github urls, erroring instead of silently doing nothing"
    (with-live-buffer nil "pre https://example.com/x post"
      (search-forward "example")
      (expect (link-plain->link-bug-reference) :to-throw 'error))))

;;; bug-reference source
;;
;; Each converter expands the org/repo#N reference to its GitHub URL and
;; uses that URL as the link target - a raw ref target is not clickable and
;; breaks the reverse converters (bisect-github-url errors on non-urls).

(describe "bug reference converts to"
  (it "plain, expanding to its GitHub issue url in place"
    (with-live-buffer nil "fix agzam/foo#12 soon"
      (search-forward "#12")
      (link-bug-reference->link-plain)
      (expect (buffer-string)
              :to-equal "fix https://github.com/agzam/foo/issues/12 soon")))

  (it "org, wrapping the GitHub url with the gh title"
    (with-live-buffer nil "fix agzam/foo#12 soon"
      (search-forward "#12")
      (with-title-stubs "PR title" nil
        (link-bug-reference->link-org-mode))
      (expect (buffer-string)
              :to-equal
              "fix [[https://github.com/agzam/foo/issues/12][PR title]] soon")))

  (it "markdown, wrapping the GitHub url with the gh title"
    (with-live-buffer nil "fix agzam/foo#12 soon"
      (search-forward "#12")
      (with-title-stubs "PR title" nil
        (link-bug-reference->link-markdown))
      (expect (buffer-string)
              :to-equal
              "fix [PR title](https://github.com/agzam/foo/issues/12) soon")))

  (it "org even in an org buffer, where the reference is not an org link"
    (with-live-buffer #'org-mode "fix agzam/foo#12 soon"
      (search-forward "#12")
      (with-title-stubs "PR title" nil
        (link-bug-reference->link-org-mode))
      (expect (buffer-string)
              :to-equal
              "fix [[https://github.com/agzam/foo/issues/12][PR title]] soon")))

  (it "markdown even in a markdown buffer"
    (with-live-buffer #'markdown-mode "fix agzam/foo#12 soon"
      (search-forward "#12")
      (with-title-stubs "PR title" nil
        (link-bug-reference->link-markdown))
      (expect (buffer-string)
              :to-equal
              "fix [PR title](https://github.com/agzam/foo/issues/12) soon"))))

;;; dispatcher
;;
;; Dispatch follows the link under point, so a markdown-derived buffer is
;; stood up here to prove the major mode does not steer it.

(describe "link->link-bug-reference"
  (it "routes through the org converter for an org link"
    (with-live-buffer #'org-mode "pre [[https://github.com/agzam/foo/issues/7][x]] post"
      (search-forward "issues")
      (link->link-bug-reference)
      (expect (buffer-string) :to-equal "pre agzam/foo#7 post")))

  (it "routes through the plain converter for a bare url"
    (with-live-buffer nil "pre https://github.com/agzam/foo/issues/7 post"
      (search-forward "issues")
      (link->link-bug-reference)
      (expect (buffer-string) :to-equal "pre agzam/foo#7 post")))

  (it "converts a bare url in a markdown-derived buffer"
    (with-live-buffer #'chat-like-mode "pre https://github.com/agzam/foo/pull/4 post"
      (search-forward "pull")
      (link->link-bug-reference)
      (expect (buffer-string) :to-equal "pre agzam/foo#4 post")))

  (it "still routes a real markdown link through the markdown converter"
    (with-markdown-stub
      (with-live-buffer #'chat-like-mode "pre [x](https://github.com/agzam/foo/pull/4) post"
        (search-forward "pull")
        (link->link-bug-reference)
        (expect (buffer-string) :to-equal "pre agzam/foo#4 post"))))

  (it "says so on a reference instead of silently doing nothing"
    (with-live-buffer nil "fix agzam/foo#12 soon"
      (search-forward "#12")
      (expect (link->link-bug-reference) :to-throw 'user-error)
      (expect (buffer-string) :to-equal "fix agzam/foo#12 soon"))))
