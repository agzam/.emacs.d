;;; tests/e2e/bug-reference-actions.el --- bug reference embark actions -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; Visiting and browsing a reference must work where nothing fontified it:
;; embark collect and export buffers hold the reference as plain text, with
;; no bug-reference overlay and no url at point.  A text-mode buffer is in
;; the same state, so these cases act on one and assert the url that reached
;; the action - proof that embark's target string, not point, feeds it.

(require 'cl-lib)

(defun bug-reference-actions-unfontified-p ()
  "Non-nil when no bug-reference overlay covers this buffer.
The precondition of every case here: with an overlay around, actions
that read point would pass too and prove nothing."
  (not (cl-find-if (lambda (o) (eq (overlay-get o 'category) 'bug-reference))
                   (overlays-in (point-min) (point-max)))))

(defvar bug-reference-actions-cases
  '(("forge topic" "b b")
    ("forge topic" "v")
    ("browser" "b o"))
  "(WHAT KEYS) per action offered on a `bug-reference-link' target.")

(defun bug-reference-actions-e2e ()
  "Drive the bug-reference embark actions over an unfontified reference."
  (require 'embark)
  (require 'which-key)
  (require 'bug-reference)
  ;; only the two destinations are faked - each writes the url it received
  ;; into the buffer, which is what the case compares against
  (let ((record (lambda (url &rest _)
                  (goto-char (point-max))
                  (insert " -> " (format "%s" url))))
        (bug-reference-bug-regexp bug-reference-bug-regexp)
        (bug-reference-url-format bug-reference-url-format))
    ;; scenarios run in load order, so the org/markdown buffers that install
    ;; the org/repo#N regexp through the mode hook may not have opened yet
    (init-bug-reference-mode-settings)
    (cl-letf (((symbol-function 'browse-url-externally) record)
              ((symbol-function 'forge-visit-topic-via-url) record)
              ((symbol-function 'bug-reference-github-resolve-url) #'identity))
      (mapcar
       (pcase-lambda (`(,what ,keys))
         (e2e-act-case
          (list :label (format "unfontified bug reference -> %s (%s)" what keys)
                :ext "txt"
                :text "fix agzam/foo#12 soon"
                :search "#12"
                :type 'bug-reference-link
                :probe #'bug-reference-actions-unfontified-p
                :keys keys
                :want (concat "fix agzam/foo#12 soon"
                              " -> https://github.com/agzam/foo/issues/12"))))
       bug-reference-actions-cases))))

(add-to-list 'e2e-scenarios #'bug-reference-actions-e2e)
