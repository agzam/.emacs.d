;;; tests/e2e/browse-actions.el --- browse keys on link targets -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; "b b" opens a link inside Emacs and "b o" hands it to the external
;; browser, whatever syntax the link is written in.  Only a real keypress
;; shows that: the org and markdown link maps compose with `embark-url-map',
;; where a bare "b" in either shadows the url map's whole browse prefix and
;; every one of these keys stops resolving.

(require 'cl-lib)

(defvar browse-actions-cases
  '(("plain url" "txt" "see https://example.com/x now" "example" url)
    ("org link" "org" "see [[https://example.com/x][docs]] now" "docs" org-url-link)
    ("markdown link" "md" "see [docs](https://example.com/x) now" "docs" markdown-link))
  "(WHAT EXT TEXT SEARCH TYPE) per link syntax the browse keys cover.
Every one wraps the same url, so a case differs from its neighbours in
the syntax alone.")

(defvar browse-actions-destinations
  '(("eww" "b b") ("browser" "b o"))
  "(WHERE KEYS) per half of the browse prefix.")

(defun browse-actions-e2e ()
  "Drive the browse keys over every link syntax."
  (require 'embark)
  (require 'which-key)
  ;; resolve the autoloads first: `process-external-url' and
  ;; `browse-url-externally' share one file, so the first case would load it
  ;; on top of the stubs below and hand the rest to the real browser
  (dolist (fn '(process-external-url
                browse-url-externally
                eww-open-in-other-window
                markdown-link-url-at-point
                open-org-link-in-emacs))
    (when (autoloadp (symbol-function fn))
      (autoload-do-load (symbol-function fn) fn)))
  ;; only the destinations are faked - each writes the url it received into
  ;; the buffer, which is what the case compares against
  (let ((record (lambda (url &rest _)
                  (interactive "sURL: ")
                  (goto-char (point-max))
                  (insert " -> " (format "%s" url)))))
    (cl-letf (((symbol-function 'eww-open-in-other-window) record)
              ((symbol-function 'browse-url-externally) record)
              ((symbol-function 'forge-visit-topic-via-url) record))
      (append
       (mapcan
        (pcase-lambda (`(,what ,ext ,text ,search ,type))
          (mapcar
           (pcase-lambda (`(,where ,keys))
             (e2e-act-case
              (list :label (format "%s -> %s (%s)" what where keys)
                    :ext ext
                    :text text
                    :search search
                    :type type
                    :keys keys
                    :want (concat text " -> https://example.com/x"))))
           browse-actions-destinations))
        browse-actions-cases)
       ;; "b b" is the handler `embark-url-config' prescribes, not eww flat
       (let ((text "see [[https://github.com/agzam/foo/pull/12][the PR]] now"))
         (list
          (e2e-act-case
           (list :label "org link to a pull request -> forge (b b)"
                 :ext "org"
                 :text text
                 :search "the PR"
                 :type 'org-url-link
                 :keys "b b"
                 :want (concat text " -> https://github.com/agzam/foo/pull/12")))))))))

(add-to-list 'e2e-scenarios #'browse-actions-e2e)
