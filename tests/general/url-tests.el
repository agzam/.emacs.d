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

;; NOTE buttercup :var slots are lexical - dynamic lets keep the
;; bug-reference regexp scoped to each spec (git-suite precedent).

(defmacro with-bug-ref-fixture (&rest body)
  "Run BODY in a buffer holding a bug reference, point on it."
  `(let ((bug-reference-bug-regexp bug-reference-bug-regexp)
         (bug-reference-url-format bug-reference-url-format))
     (init-bug-reference-mode-settings)
     (with-temp-buffer
       (insert "fix agzam/foo#12 soon")
       (search-backward "#12")
       ,@body)))

(defmacro with-title-stubs (gh-title page-title &rest body)
  "Run BODY with both title fetchers stubbed, never touching the network."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'get-gh-item-title)
              (lambda (&rest _) ,gh-title))
             ((symbol-function 'org-cliplink-retrieve-title-synchronously)
              (lambda (&rest _) ,page-title)))
     ,@body))

;;; detection

(describe "url-get-link-type"
  (it "spans the whole url for plain links, not just the scheme"
    (with-temp-buffer
      (insert "see https://example.com/some/path now")
      (search-backward "some")
      (pcase-let ((`(plain ,beg . ,end) (url-get-link-type)))
        (expect (buffer-substring-no-properties beg end)
                :to-equal "https://example.com/some/path")))))

(describe "embark-target-bug-reference-link-at-point"
  (it "targets an org/repo#N reference with its bounds"
    (with-bug-ref-fixture
     (pcase-let ((`(bug-reference-link ,text . ,bounds)
                  (embark-target-bug-reference-link-at-point)))
       (expect text :to-equal "agzam/foo#12")
       (expect (buffer-substring-no-properties (car bounds) (cdr bounds))
               :to-equal "agzam/foo#12"))))

  (it "returns nil off-reference"
    (let ((bug-reference-bug-regexp bug-reference-bug-regexp)
          (bug-reference-url-format bug-reference-url-format))
      (init-bug-reference-mode-settings)
      (with-temp-buffer
        (insert "plain words only")
        (goto-char (point-min))
        (expect (embark-target-bug-reference-link-at-point) :to-be nil))))

  (it "does not mis-target an org/repo#N-shaped fragment inside a url"
    (let ((bug-reference-bug-regexp bug-reference-bug-regexp)
          (bug-reference-url-format bug-reference-url-format))
      (init-bug-reference-mode-settings)
      (with-temp-buffer
        (insert "https://github.com/agzam/foo#4")
        (search-backward "agzam")
        (expect (embark-target-bug-reference-link-at-point) :to-be nil)))))

;;; parsers

(describe "parse-org-link-at-point"
  (it "extracts url, label and bounds without the trailing blank"
    (with-temp-buffer
      (org-mode)
      (insert "pre [[https://example.com][Ex ample]] post")
      (search-backward "Ex")
      (let ((parts (parse-org-link-at-point)))
        (expect (plist-get parts :url) :to-equal "https://example.com")
        (expect (plist-get parts :label) :to-equal "Ex ample")
        (expect (buffer-substring-no-properties
                 (plist-get parts :beg) (plist-get parts :end))
                :to-equal "[[https://example.com][Ex ample]]")))))

;;; org source

(describe "link-org->link-plain"
  (it "replaces the link with its url, preserving surrounding spacing"
    (with-temp-buffer
      (org-mode)
      (insert "pre [[https://example.com][x]] post")
      (search-backward "x")
      (link-org->link-plain)
      (expect (buffer-string) :to-equal "pre https://example.com post"))))

(describe "link-org->link-markdown"
  (it "reuses the existing description as the label"
    (with-temp-buffer
      (org-mode)
      (insert "pre [[https://example.com][desc]] post")
      (search-backward "desc")
      (with-title-stubs nil nil
        (link-org->link-markdown))
      (expect (buffer-string) :to-equal "pre [desc](https://example.com) post"))))

(describe "link-org->just-text"
  (it "strips the link down to its description"
    (with-temp-buffer
      (org-mode)
      (insert "pre [[https://example.com][desc]] post")
      (search-backward "desc")
      (link-org->just-text)
      (expect (buffer-string) :to-equal "pre desc post"))))

(describe "link-org->link-bug-reference"
  (it "converts a github pull link to org/repo#N"
    (with-temp-buffer
      (org-mode)
      (insert "pre [[https://github.com/agzam/foo/pull/4][x]] post")
      (search-backward "x")
      (link-org->link-bug-reference)
      (expect (buffer-string) :to-equal "pre agzam/foo#4 post"))))

;;; markdown source

(describe "link-markdown->link-org-mode"
  (it "reuses the label and asks markdown-link-at-pos only once"
    (let ((calls 0))
      (cl-letf (((symbol-function 'markdown-link-at-pos)
                 (lambda (_)
                   (cl-incf calls)
                   '(5 33 "label" "https://example.com" nil nil))))
        (with-temp-buffer
          (insert "pre [label](https://example.com) post")
          (search-backward "label")
          (with-title-stubs nil nil
            (link-markdown->link-org-mode))
          (expect (buffer-string)
                  :to-equal "pre [[https://example.com][label]] post")
          (expect calls :to-equal 1)))))

  (it "converts autolinks via a fetched title"
    (with-temp-buffer
      (insert "pre <https://example.com> post")
      (search-backward "example")
      (with-title-stubs nil "Example Site"
        (link-markdown->link-org-mode))
      (expect (buffer-string)
              :to-equal "pre [[https://example.com][Example Site]] post"))))

;;; plain source

(describe "link-plain->link-org-mode"
  (it "converts non-github urls via the page title"
    (with-temp-buffer
      (insert "pre https://example.com post")
      (search-backward "example")
      (with-title-stubs nil "Example Site"
        (link-plain->link-org-mode))
      (expect (buffer-string)
              :to-equal "pre [[https://example.com][Example Site]] post")))

  (it "prefers the github item title for github urls"
    (with-temp-buffer
      (insert "pre https://github.com/agzam/foo/pull/4 post")
      (search-backward "pull")
      (with-title-stubs "agzam/foo#4 — Title" nil
        (link-plain->link-org-mode))
      (expect (buffer-string)
              :to-equal
              "pre [[https://github.com/agzam/foo/pull/4][agzam/foo#4 — Title]] post")))

  (it "links bare when no title can be fetched"
    (with-temp-buffer
      (insert "pre https://example.com post")
      (search-backward "example")
      (with-title-stubs nil nil
        (link-plain->link-org-mode))
      (expect (buffer-string) :to-equal "pre [[https://example.com]] post"))))

(describe "link-plain->link-markdown"
  (it "falls back to an autolink when no title can be fetched"
    (with-temp-buffer
      (insert "pre https://example.com post")
      (search-backward "example")
      (with-title-stubs nil nil
        (link-plain->link-markdown))
      (expect (buffer-string) :to-equal "pre <https://example.com> post"))))

(describe "link-plain->link-bug-reference"
  (it "converts a github pull url to org/repo#N"
    (with-temp-buffer
      (insert "pre https://github.com/agzam/foo/pull/4 post")
      (search-backward "pull")
      (link-plain->link-bug-reference)
      (expect (buffer-string) :to-equal "pre agzam/foo#4 post")))

  (it "errors on non-github urls instead of silently doing nothing"
    (with-temp-buffer
      (insert "pre https://example.com/x post")
      (search-backward "example")
      (expect (link-plain->link-bug-reference) :to-throw 'error))))

;;; bug-reference source
;;
;; Each converter expands the org/repo#N reference to its GitHub URL and
;; uses that URL as the link target - a raw ref target is not clickable and
;; breaks the reverse converters (bisect-github-url errors on non-urls).

(describe "link-bug-reference->link-plain"
  (it "expands the reference to its GitHub issue URL in place"
    (with-bug-ref-fixture
     (link-bug-reference->link-plain)
     (expect (buffer-string)
             :to-equal "fix https://github.com/agzam/foo/issues/12 soon"))))

(describe "link-bug-reference->link-org-mode"
  (it "wraps the GitHub URL in an org link with the gh title"
    (with-bug-ref-fixture
     (with-title-stubs "PR title" nil
       (link-bug-reference->link-org-mode))
     (expect (buffer-string)
             :to-equal
             "fix [[https://github.com/agzam/foo/issues/12][PR title]] soon"))))

(describe "link-bug-reference->link-markdown"
  (it "wraps the GitHub URL in a markdown link with the gh title"
    (with-bug-ref-fixture
     (with-title-stubs "PR title" nil
       (link-bug-reference->link-markdown))
     (expect (buffer-string)
             :to-equal
             "fix [PR title](https://github.com/agzam/foo/issues/12) soon"))))

;;; mode-aware dispatcher

(describe "link->link-bug-reference"
  (it "routes through the org converter in org buffers"
    (with-temp-buffer
      (org-mode)
      (insert "pre [[https://github.com/agzam/foo/issues/7][x]] post")
      (search-backward "x")
      (link->link-bug-reference)
      (expect (buffer-string) :to-equal "pre agzam/foo#7 post")))

  (it "routes through the plain converter elsewhere"
    (with-temp-buffer
      (insert "pre https://github.com/agzam/foo/issues/7 post")
      (search-backward "issues")
      (link->link-bug-reference)
      (expect (buffer-string) :to-equal "pre agzam/foo#7 post"))))
