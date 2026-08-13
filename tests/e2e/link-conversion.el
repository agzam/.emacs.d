;;; tests/e2e/link-conversion.el --- link conversion flows -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; Every conversion the embark keymaps offer, driven by the keys that
;; reach it.  The batch suite covers the same matrix by calling the
;; converters directly; what only shows up here is the wiring - which
;; finder claims the thing under point, which keymap that target opens,
;; and what the mode did to the buffer before either ran.

(require 'cl-lib)

(defun link-conversion-bug-reference-live-p ()
  "Non-nil when a bug-reference overlay covers part of this buffer.
Proof that the fixture reached the state a real buffer is in: the
overlay and the thingatpt url provider arrive together, and it was the
provider that made every reference look like a plain url."
  (and (cl-find-if (lambda (o) (eq (overlay-get o 'category) 'bug-reference))
                   (overlays-in (point-min) (point-max)))
       t))

;; :what/:ext/:text/:search/:type describe one buffer with point in one
;; place; :acts lists (TARGET KEYS EXPECTED) for every conversion offered
;; from there.  In an org buffer even a bare url is an org link element,
;; so embark types it org-url-link and the org converters answer for it.
(defvar link-conversion-fixtures
  '((:what "bug reference in an org buffer"
     :ext "org" :text "fix agzam/foo#12 soon" :search "#12"
     :type bug-reference-link
     :probe link-conversion-bug-reference-live-p
     :acts (("markdown" "c m" "fix [PR title](https://github.com/agzam/foo/issues/12) soon")
            ("org" "c o" "fix [[https://github.com/agzam/foo/issues/12][PR title]] soon")
            ("plain" "c p" "fix https://github.com/agzam/foo/issues/12 soon")))

    (:what "org link"
     :ext "org" :text "see [[https://github.com/agzam/foo/pull/4][Fix it]] now"
     :search "Fix" :type org-url-link
     :acts (("markdown" "c m" "see [Fix it](https://github.com/agzam/foo/pull/4) now")
            ("plain" "c p" "see https://github.com/agzam/foo/pull/4 now")
            ("text" "c s" "see Fix it now")
            ("bug reference" "c b" "see agzam/foo#4 now")))

    (:what "bare url in an org buffer"
     :ext "org" :text "see https://example.com now" :search "example"
     :type org-url-link
     :acts (("org" "c o" "see [[https://example.com][Example Site]] now")
            ("markdown" "c m" "see [Example Site](https://example.com) now")))

    (:what "bare github url in an org buffer"
     :ext "org" :text "see https://github.com/agzam/foo/pull/4 now" :search "pull"
     :type org-url-link
     :acts (("bug reference" "c b" "see agzam/foo#4 now")))

    ;; the reference-shaped tail is not a reference: converting it must
    ;; keep the url whole instead of rewriting the fragment alone
    (:what "url ending in a ref-shaped fragment"
     :ext "org" :text "see https://github.com/agzam/foo#4 now" :search "agzam"
     :type org-url-link
     :probe link-conversion-bug-reference-live-p
     :acts (("markdown" "c m" "see [Example Site](https://github.com/agzam/foo#4) now")))

    (:what "bug reference in a markdown buffer"
     :ext "md" :text "fix agzam/foo#12 soon" :search "#12"
     :type bug-reference-link
     :probe link-conversion-bug-reference-live-p
     :acts (("org" "c o" "fix [[https://github.com/agzam/foo/issues/12][PR title]] soon")
            ("markdown" "c m" "fix [PR title](https://github.com/agzam/foo/issues/12) soon")
            ("plain" "c p" "fix https://github.com/agzam/foo/issues/12 soon")))

    (:what "markdown link"
     :ext "md" :text "see [Fix it](https://github.com/agzam/foo/pull/4) now"
     :search "Fix" :type markdown-link
     :acts (("org" "c o" "see [[https://github.com/agzam/foo/pull/4][Fix it]] now")
            ("plain" "c p" "see https://github.com/agzam/foo/pull/4 now")
            ("text" "c s" "see Fix it now")
            ("bug reference" "c b" "see agzam/foo#4 now")))

    (:what "markdown autolink"
     :ext "md" :text "see <https://example.com> now" :search "example"
     :type markdown-link
     :acts (("org" "c o" "see [[https://example.com][Example Site]] now")
            ("plain" "c p" "see https://example.com now")))

    (:what "bare url in a markdown buffer"
     :ext "md" :text "see https://example.com now" :search "example"
     :type url
     :acts (("org" "c o" "see [[https://example.com][Example Site]] now")
            ("markdown" "c m" "see [Example Site](https://example.com) now")))

    (:what "bare github url in a markdown buffer"
     :ext "md" :text "see https://github.com/agzam/foo/pull/4 now" :search "pull"
     :type github-pr
     :acts (("bug reference" "c b" "see agzam/foo#4 now")))))

(defun link-conversion-run-fixture (fixture)
  "Run every act FIXTURE lists, returning one result each."
  (mapcar
   (pcase-lambda (`(,target ,keys ,want))
     (e2e-act-case
      (list :label (format "%s -> %s (%s)" (plist-get fixture :what) target keys)
            :ext (plist-get fixture :ext)
            :text (plist-get fixture :text)
            :search (plist-get fixture :search)
            :type (plist-get fixture :type)
            :probe (plist-get fixture :probe)
            :keys keys
            :want want)))
   (plist-get fixture :acts)))

(defun link-conversion-e2e ()
  "Drive every link conversion the embark keymaps offer."
  (require 'embark)
  (require 'which-key)
  ;; only the network is faked; the modes, the fontification, the target
  ;; finders and the keymaps are the ones a session actually runs
  (cl-letf (((symbol-function 'get-gh-item-title)
             (lambda (url &rest _)
               (when (string-match-p
                      "github\\.com/[^/]+/[^/]+/\\(issues\\|pull\\)/[0-9]+" url)
                 "PR title")))
            ((symbol-function 'org-cliplink-retrieve-title-synchronously)
             (lambda (&rest _) "Example Site"))
            ((symbol-function 'bug-reference-github-resolve-url) #'identity))
    (mapcan #'link-conversion-run-fixture link-conversion-fixtures)))

(add-to-list 'e2e-scenarios #'link-conversion-e2e)
