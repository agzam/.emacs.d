;;; tests/writing/sdcv-tests.el --- writing/autoload/sdcv.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/writing/autoload/sdcv.el")

(defun sdcv-tests--use-package-form ()
  "Read the (use-package sdcv ...) form out of the writing module's config."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "modules/writing/config.el" test-config-root))
    (goto-char (point-min))
    (let (form)
      (condition-case nil
          (while (not form)
            (let ((f (read (current-buffer))))
              (when (and (eq (car-safe f) 'use-package) (eq (cadr f) 'sdcv))
                (setq form f))))
        (end-of-file nil))
      form)))

(describe "sdcv package recipe"
  ;; The MELPA menu inherits repo.or.cz (flaky, times out); the pin must stay
  ;; on emacsmirror, which mirrors the same history.  And it must never drift
  ;; to manateelazycat/sdcv - same name, incompatible implementation.
  (it "pins the emacsmirror mirror explicitly"
    (let* ((form (sdcv-tests--use-package-form))
           (recipe (cadr (memq :ensure form))))
      (expect (car-safe recipe) :to-be 'sdcv)
      (expect (plist-get (cdr recipe) :host) :to-be 'github)
      (expect (plist-get (cdr recipe) :repo) :to-equal "emacsmirror/sdcv"))))

(describe "region-or-word-at-point-str"
  (it "returns the word at point"
    (with-temp-buffer
      (insert "hello world")
      (goto-char 3)
      (expect (region-or-word-at-point-str) :to-equal "hello")))

  (it "prefers the active region"
    (with-temp-buffer
      (insert "hello world")
      (push-mark 1 t t)
      (goto-char 8)
      (expect (region-or-word-at-point-str) :to-equal "hello w"))))

(describe "sdcv-search-at-point"
  (it "searches the word at point with focus"
    (let (args)
      (cl-letf (((symbol-function 'sdcv-search)
                 (lambda (&rest a) (setq args a))))
        (with-temp-buffer
          (insert "lexeme")
          (goto-char 3)
          (sdcv-search-at-point))
        (expect args :to-equal '("lexeme" nil nil t))))))