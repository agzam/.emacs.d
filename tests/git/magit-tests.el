;;; tests/git/magit-tests.el --- git/autoload/magit.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/git/autoload/magit.el")

;; Expected values below were captured from doom.d's dash/s-based original
;; running in the live session - the rewrite must not drift from it.
(describe "magit-create-branch-friendly-string"
  (it "flattens punctuation, drops the ticket number"
    (expect (magit-create-branch-friendly-string
             "Fix parser: crash {on} empty [input] #123")
            :to-equal "fix_parser_crash_on_empty_input"))
  (it "converts apostrophes like the original"
    (expect (magit-create-branch-friendly-string "Don't break CamelCase")
            :to-equal "don_t_break_camelcase"))
  (it "turns path separators into word joints"
    (expect (magit-create-branch-friendly-string "Add support for foo/bar paths")
            :to-equal "add_support_for_foo_bar_paths"))
  (it "drops punctuation-only words"
    (expect (magit-create-branch-friendly-string "[Bug] name -- weird -- #99")
            :to-equal "bug_name_weird"))
  (it "keeps at most eight words"
    (expect (magit-create-branch-friendly-string
             "one two three four five six seven eight nine ten")
            :to-equal "one_two_three_four_five_six_seven_eight"))
  (it "replaces special chars with separators"
    (expect (magit-create-branch-friendly-string
             "Weird ~chars~ ^here^ ?maybe? @sure@")
            :to-equal "weird_chars_here_maybe_sure"))
  (it "keeps ticket-prefix style titles usable"
    (expect (magit-create-branch-friendly-string
             "[QCB-1234]: some { special } thing #42")
            :to-equal "qcb_1234_some_special_thing")))

;; batch stub: magit isn't installed in the test sandbox
(defvar magit-tests--which-function nil)
(defun magit-which-function () magit-tests--which-function)

(describe "magit-python-which-function"
  (it "strips the class prefix from dotted names"
    (let ((magit-tests--which-function "MyClass.method"))
      (expect (magit-python-which-function) :to-equal "method")))
  (it "leaves plain function names alone"
    (let ((magit-tests--which-function "top_level_fn"))
      (expect (magit-python-which-function) :to-equal "top_level_fn")))
  (it "returns nil when there is no function at point"
    (let ((magit-tests--which-function nil))
      (expect (magit-python-which-function) :to-be nil))))
