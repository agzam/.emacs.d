;;; tests/lookup/commands-tests.el --- lookup/autoload/commands.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; dash-docs-common-docsets must be special so the command's `let' binds it
;; dynamically (as it does in real boots, where dash-docs declares it).
(defvar dash-docs-common-docsets nil)

(load-module-file "modules/lookup/autoload/commands.el")

(describe "lookup-in-all-docsets"
  (it "widens consult-dash to every installed docset and prefills the symbol"
    (provide 'dash-docs)
    (let ((captured nil))
      (cl-letf (((symbol-function 'dash-docs-installed-docsets)
                 (lambda () '("Python" "Rust" "CSS")))
                ((symbol-function 'consult-dash)
                 (lambda (&optional term)
                   (setq captured (list :term term
                                        :common dash-docs-common-docsets)))))
        (with-temp-buffer
          (insert "foobar")
          (goto-char (point-min))
          (lookup-in-all-docsets)
          (expect (plist-get captured :common)
                  :to-equal '("Python" "Rust" "CSS"))
          (expect (plist-get captured :term) :to-equal "foobar"))))))

(describe "lookup-dictionary-definition"
  (it "delegates to the offline sdcv word lookup"
    (let ((called nil))
      (cl-letf (((symbol-function 'sdcv-search-at-point)
                 (lambda (&rest _) (setq called t))))
        (lookup-dictionary-definition)
        (expect called :to-be-truthy)))))

(describe "lookup-synonyms"
  (it "delegates to the in-Emacs thesaurus"
    (let ((called nil))
      (cl-letf (((symbol-function 'mw-thesaurus-lookup-dwim)
                 (lambda (&rest _) (setq called t))))
        (lookup-synonyms)
        (expect called :to-be-truthy)))))
