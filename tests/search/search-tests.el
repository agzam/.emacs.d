;;; tests/search/search-tests.el --- search/autoload/search.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'dired)

;; consult-line-collect-urls requires consult at call time; batch tier has
;; no packages
(provide 'consult)

(load-module-file "modules/search/autoload/search.el")

(describe "search-github-with-lang"
  :var (opened)

  (before-each (setq opened nil))

  (it "seeds the query with the mode's GitHub language term"
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt initial) initial))
              ((symbol-function 'browse-url)
               (lambda (url) (setq opened url))))
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "mapconcat")
        (goto-char (point-min))
        (search-github-with-lang)))
    (expect opened :to-equal
            (format "https://github.com/search?q=%s&type=code"
                    (url-hexify-string "language:\"Emacs Lisp\" mapconcat"))))

  (it "omits the language term for unmapped modes"
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt initial) initial))
              ((symbol-function 'browse-url)
               (lambda (url) (setq opened url))))
      (with-temp-buffer
        (fundamental-mode)
        (insert "needle")
        (goto-char (point-min))
        (search-github-with-lang)))
    (expect opened :to-equal
            (format "https://github.com/search?q=%s&type=code"
                    (url-hexify-string "needle")))))

;; zoxide-find and add-to-zoxide-cache left for the consult-zoxide package;
;; their specs live in that repo's test/ now

(describe "search-in-project"
  :var (got)

  (before-each
    (setq got nil))

  (it "seeds the project search with the active region"
    (cl-letf (((symbol-function 'consult-ripgrep)
               (lambda (dir initial) (setq got (list dir initial))))
              ((symbol-function 'project-current) (lambda (&rest _) 'proj))
              ((symbol-function 'project-root) (lambda (_) "/proj/")))
      (with-temp-buffer
        (transient-mark-mode 1)
        (insert "region payload")
        (push-mark (point-min) t t)
        (goto-char (+ (point-min) 6))
        (search-in-project)))
    (expect got :to-equal '("/proj/" "region")))

  (it "seeds with the symbol at point"
    (cl-letf (((symbol-function 'consult-ripgrep)
               (lambda (dir initial) (setq got (list dir initial))))
              ((symbol-function 'project-current) (lambda (&rest _) 'proj))
              ((symbol-function 'project-root) (lambda (_) "/proj/")))
      (with-temp-buffer
        (insert "needle")
        (goto-char (point-min))
        (search-in-project)))
    (expect got :to-equal '("/proj/" "needle")))

  (it "seeds nothing when point is between symbols"
    ;; doom.d rot pin: (symbol-name (symbol-at-point)) seeded literal "nil"
    (cl-letf (((symbol-function 'consult-ripgrep)
               (lambda (dir initial) (setq got (list dir initial))))
              ((symbol-function 'project-current) (lambda (&rest _) 'proj))
              ((symbol-function 'project-root) (lambda (_) "/proj/")))
      (with-temp-buffer
        (search-in-project)))
    (expect got :to-equal '("/proj/" nil))))

(describe "consult-line-collect-urls--candidates"
  (it "collects only url-bearing lines, first url per line"
    (with-temp-buffer
      (insert "no url here\n"
              "see https://one.example/x now\n"
              "plain line\n"
              "and https://two.example/y plus https://three.example/z\n")
      (expect (consult-line-collect-urls--candidates)
              :to-equal '("2: https://one.example/x"
                          "4: https://two.example/y"))))

  (it "carries the bare url as an embark url target plus its position"
    (with-temp-buffer
      (insert "see https://one.example/x now\n")
      (let ((cand (car (consult-line-collect-urls--candidates))))
        (expect (get-text-property 0 'multi-category cand)
                :to-equal '(url . "https://one.example/x"))
        (expect (get-text-property 0 'consult--candidate cand)
                :to-equal 5))))

  (it "terminates org-link urls at the closing bracket"
    (with-temp-buffer
      (insert "[[https://news.ycombinator.com/item?id=42][View story in eww]]\n")
      (expect (consult-line-collect-urls--candidates)
              :to-equal '("1: https://news.ycombinator.com/item?id=42"))))

  (it "trims trailing prose punctuation off the url"
    (with-temp-buffer
      (insert "read https://one.example/x, before bed\n")
      (expect (consult-line-collect-urls--candidates)
              :to-equal '("1: https://one.example/x"))))

  (it "skips lines matching ignore-regexp anywhere on the line"
    (with-temp-buffer
      (insert "keep https://one.example/x\n"
              "https://news.ycombinator.com/item?id=1\n"
              "[[https://two.example/y][view story in eww]]\n")
      (expect (consult-line-collect-urls--candidates
               "ycombinator\\.com\\|view story in eww")
              :to-equal '("1: https://one.example/x")))))

(describe "consult-line-collect-urls"
  (it "signals user-error when the buffer has no urls"
    (with-temp-buffer
      (insert "nothing\nto collect\n")
      (expect (consult-line-collect-urls) :to-throw 'user-error)))

  (it "wires url candidates into consult's jump-state protocol"
    (with-temp-buffer
      (insert "see https://one.example/x now\n"
              "and https://two.example/y\n")
      (let (captured-opts)
        (cl-letf* (((symbol-function 'consult--jump-state)
                    (lambda ()
                      (lambda (action cand)
                        (when (and (eq action 'return) cand)
                          (goto-char cand)))))
                   ((symbol-function 'consult--lookup-candidate)
                    (lambda (selected cands &rest _)
                      (get-text-property 0 'consult--candidate
                                         (car (member selected cands)))))
                   ((symbol-function 'consult--read)
                    (lambda (cands &rest opts)
                      (setq captured-opts opts)
                      ;; emulate consult: pick the 2nd candidate, resolve
                      ;; it through :lookup, hand it to :state on 'return
                      (let ((pos (funcall (plist-get opts :lookup)
                                          (nth 1 cands) cands "")))
                        (funcall (plist-get opts :state) 'return pos)
                        pos))))
          (consult-line-collect-urls))
        (expect (plist-get captured-opts :category) :to-be 'multi-category)
        (expect (plist-get captured-opts :require-match) :to-be t)
        (expect (plist-member captured-opts :sort) :to-be-truthy)
        (expect (plist-get captured-opts :sort) :to-be nil)
        ;; point landed on the second url, not at bol
        (expect (line-number-at-pos) :to-equal 2)
        (expect (looking-at "https://two\\.example/y") :to-be-truthy)))))
