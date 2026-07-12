;;; tests/general/url-tests.el --- general/autoload/url.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'org)
(require 'bug-reference)

;; url.el requires ghub at top level for its gh-title helpers; not installed
;; here and not needed by the functions under test.
(provide 'ghub)
;; let-plist macro (elisp module) backs link-bug-reference->link-markdown;
;; the git module's installer supplies the production bug-reference regexp.
(load-module-file "modules/elisp/autoload/let-plist.el")
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
        (expect (embark-target-bug-reference-link-at-point) :to-be nil)))))

;; The three converters below used to call the void
;; embark-target-bug-reference-at-point (missing -link-; doom.d still has
;; that bug) - each spec pins the fixed call path end to end.

(describe "link-bug-reference->link-plain"
  (it "completes and rewrites the reference in place"
    (with-bug-ref-fixture
     (expect (link-bug-reference->link-plain) :not :to-throw)
     (expect (buffer-string) :to-equal "fix agzam/foo#12 soon"))))

(describe "link-bug-reference->link-org-mode"
  (it "wraps the reference in an org link with the gh title"
    (with-bug-ref-fixture
     (cl-letf (((symbol-function 'get-gh-item-title)
                (lambda (&rest _) "PR title")))
       (link-bug-reference->link-org-mode))
     (expect (buffer-string)
             :to-equal "fix [[agzam/foo#12][PR title]] soon"))))

(describe "link-bug-reference->link-markdown"
  (it "wraps the reference in a markdown link with the gh title"
    (with-bug-ref-fixture
     (cl-letf (((symbol-function 'get-gh-item-title)
                (lambda (&rest _) "PR title"))
               ((symbol-function 'bisect-github-url)
                (lambda (&rest _) '(:org "agzam" :repo "foo" :pull "12"))))
       (link-bug-reference->link-markdown))
     (expect (buffer-string)
             :to-equal "fix [PR title](agzam/foo#12) soon"))))
