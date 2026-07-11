;;; tests/org/custom-tests.el --- org/autoload/custom.el specs -*- lexical-binding: t; -*-
;; get-gh-item-title / org-link-make-description-fn pin the dash/s rewrites
;; (->>/-non-nil/s-blank? ground-truthed against the doom.d originals).
;; Smoke-only: org-roam-toggle-ui-xwidget (xwidget), org-store-link-id-optional
;; (org-store-link session plumbing).

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'org)

;; get-gh-item-title calls ghub-get at runtime; stub for load+call in batch.
(defvar ghub-get-stub-resp nil)
(defvar ghub-get-stub-calls nil)
(defun ghub-get (resource &rest _)
  (push resource ghub-get-stub-calls)
  ghub-get-stub-resp)

(load-module-file "modules/org/autoload/custom.el")

(describe "get-gh-item-title"
  (before-each (setq ghub-get-stub-calls nil))
  (it "resolves PR links through /repos/../pulls/N"
    (setq ghub-get-stub-resp '((title . "Fix the thing")))
    (expect (get-gh-item-title "https://github.com/foo/bar/pull/42")
            :to-equal "foo/bar#42 — Fix the thing")
    (expect ghub-get-stub-calls :to-equal '("/repos/foo/bar/pulls/42")))
  (it "resolves issue links through /repos/../issues/N"
    (setq ghub-get-stub-resp '((title . "It breaks")))
    (expect (get-gh-item-title "https://github.com/foo/bar/issues/7")
            :to-equal "foo/bar#7 — It breaks")
    (expect ghub-get-stub-calls :to-equal '("/repos/foo/bar/issues/7")))
  (it "resolves bare repo links to the repo description"
    (setq ghub-get-stub-resp '((description . "A bar for foos")))
    (expect (get-gh-item-title "https://github.com/foo/bar")
            :to-equal "foo/bar — A bar for foos"))
  (it "compacts file-in-branch links without hitting the API"
    (expect (get-gh-item-title "https://github.com/foo/bar/blob/main/dir/file.el")
            :to-equal "foo/bar/blob/main/dir/file.el")
    (expect ghub-get-stub-calls :to-be nil))
  (it "trims commit shas to 7 chars"
    (expect (get-gh-item-title
             "https://github.com/foo/bar/commit/0123456789abcdef")
            :to-equal "foo/bar/commit/0123456"))
  (it "passes non-github uris through"
    (expect (get-gh-item-title "https://example.com/x") :to-equal
            "https://example.com/x")))

(describe "org-link-make-description-fn"
  (it "prefers a non-empty description"
    (expect (org-link-make-description-fn "https://github.com/a/b" "desc")
            :to-equal "desc"))
  (it "treats empty description as absent (s-blank? parity)"
    (setq ghub-get-stub-resp '((description . "D")))
    (expect (org-link-make-description-fn "https://github.com/a/b" "")
            :to-equal "a/b — D"))
  (it "leaves non-github links alone"
    (expect (org-link-make-description-fn "https://example.com" nil)
            :to-be nil)))

(describe "org-remove-link-at-point"
  (it "deletes the bracket link at point"
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "pre [[https://x][desc]] post")
      (search-backward "desc")
      (org-remove-link-at-point)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "pre  post")))
  (it "complains when not on a link"
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "no link here")
      (expect (org-remove-link-at-point) :to-throw 'user-error))))

(describe "org-goto-bottommost-heading"
  (it "walks to the deepest visible heading without leaking `currlevel'"
    (expect (boundp 'currlevel) :to-be nil)  ; the doom.d global leak, fixed
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "* one\n** two\n*** three\n")
      (goto-char (point-min))
      (org-goto-bottommost-heading)
      (expect (org-at-heading-p) :to-be-truthy))
    (expect (boundp 'currlevel) :to-be nil)))

(describe "person-w-name-based-id"
  (it "builds a reversed-name id and alias block"
    (require 'org-capture)
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (&rest _) "john doe"))
              ((symbol-function 'org-capture-get) #'ignore)
              ((symbol-function 'gui-get-selection) #'ignore))
      (expect (person-w-name-based-id)
              :to-equal
              "John D\n:PROPERTIES:\n:ID: doe-john\n:roam_aliases: \"John Doe\"\n:END:"))))
