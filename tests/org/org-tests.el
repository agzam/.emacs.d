;;; tests/org/org-tests.el --- org/autoload/org.el specs -*- lexical-binding: t; -*-
;; Runs against BUILT-IN org (test-elpa carries only buttercup); the module
;; targets the elpaca-menu org build, but these helpers stick to stable API.
;; Smoke-only siblings (see MIGRATION coverage map): org-dwim-at-point,
;; org-cycle-only-current-subtree-h and the fold-level pair (window-bound).

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'org)

(load-module-file "modules/org/autoload/org.el")

(defmacro with-org-buffer (content &rest body)
  "Run BODY in a fundamentally quiet org-mode temp buffer over CONTENT.
Point starts at the position of the ^ marker (removed), else point-min."
  (declare (indent 1))
  `(with-temp-buffer
     (delay-mode-hooks (org-mode))
     (insert ,content)
     (goto-char (point-min))
     (if (search-forward "^" nil t)
         (delete-char -1)
       (goto-char (point-min)))
     ,@body))

;; NOTE buttercup's :var is no use here: for special variables the binding
;; is live while the suite BUILDS, not when specs run - bind inside `it'.
(describe "org-get-todo-keywords-for"
  (it "returns the whole sequence a keyword belongs to, markers stripped"
    (let ((org-todo-keywords '((sequence "TODO(t)" "ONGOING(o)" "|" "DONE(d)"))))
      ;; the "|" separator survives - doom.d parity
      (expect (org-get-todo-keywords-for "ONGOING")
              :to-equal '("TODO" "ONGOING" "|" "DONE"))))
  (it "returns nil for a foreign keyword"
    (let ((org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)"))))
      (expect (org-get-todo-keywords-for "NOPE") :to-be nil)))
  (it "returns nil without a keyword"
    (expect (org-get-todo-keywords-for) :to-be nil)))

(describe "org--insert-item"
  (it "inserts a heading below, after the current subtree"
    (with-org-buffer "* one^\nbody\n* two\n"
      (org--insert-item 'below)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "* one\nbody\n* \n* two\n")
      (expect (org-at-heading-p) :to-be-truthy)))
  (it "inserts a heading above the current one"
    (with-org-buffer "* one^\nbody\n"
      (org--insert-item 'above)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "* \n* one\nbody\n")))
  (it "carries the todo keyword over"
    (with-org-buffer "* TODO task^\n"
      (org--insert-item 'below)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-match "\\* TODO \n")))
  (it "inserts a list item below"
    (with-org-buffer "- one^\n- two\n"
      (org--insert-item 'below)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "- one\n- \n- two\n"))))

(describe "org-insert-item-below/above"
  (it "repeat COUNT times"
    (with-org-buffer "* one^\n"
      (org-insert-item-below 2)
      (expect (count-lines (point-min) (point-max)) :to-equal 3))))

;; NOTE no buttercup spies on org edit commands: every `org-mode' call runs
;; `org-fold--advice-edit-commands', whose advice-add rewrites the advised
;; symbol's function cell and orphans the spy bookkeeping.  cl-letf instead.
(describe "org-shift-return"
  (it "falls through to org-return outside tables"
    (let (called)
      (cl-letf (((symbol-function 'org-return)
                 (lambda (&rest args) (setq called args))))
        (with-org-buffer "text^"
          (org-shift-return 1)))
      (expect called :to-equal '(nil 1))))
  (it "copies down inside a table"
    (let (called)
      (cl-letf (((symbol-function 'org-table-copy-down)
                 (lambda (&rest args) (setq called args))))
        (with-org-buffer "| a |\n|^ b |\n"
          (org-shift-return 1)))
      (expect called :to-equal '(1)))))

(describe "org-table-previous-row"
  (it "moves to the same column one row up"
    (with-org-buffer "| a | b |\n| 1 | 2 |\n| 3 | ^4 |\n"
      (expect (org-table-current-column) :to-equal 2)
      (org-table-previous-row)
      (expect (org-table-current-column) :to-equal 2)
      (expect (line-number-at-pos) :to-equal 2))))

(describe "fold commands"
  (it "org-close-all-folds hides body text, org-open-all-folds reveals it"
    (with-org-buffer "* one\nbody one\n** sub\nbody sub\n"
      (org-close-all-folds)
      (expect (org-fold-folded-p (save-excursion
                                   (goto-char (point-min))
                                   (search-forward "body one")
                                   (match-beginning 0)))
              :to-be-truthy)
      (org-open-all-folds)
      (expect (org-fold-folded-p (save-excursion
                                   (goto-char (point-min))
                                   (search-forward "body one")
                                   (match-beginning 0)))
              :to-be nil)))
  (it "aliases resolve to real functions"
    (expect (fboundp 'org-toggle-fold) :to-be-truthy)
    (expect (indirect-function 'org-close-fold)
            :to-equal (indirect-function 'outline-hide-subtree))))

(describe "org-clear-babel-results-h"
  (it "removes the RESULTS block under the src block at point"
    (with-org-buffer
        "#+begin_src emacs-lisp\n^(+ 1 2)\n#+end_src\n\n#+RESULTS:\n: 3\n"
      (expect (org-clear-babel-results-h) :to-be t)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :not :to-match "RESULTS")))
  (it "returns nil when there is nothing to clear"
    (with-org-buffer "#+begin_src emacs-lisp\n^(+ 1 2)\n#+end_src\n"
      (expect (org-clear-babel-results-h) :to-be nil))
    (with-org-buffer "plain ^text\n"
      (expect (org-clear-babel-results-h) :to-be nil))))
