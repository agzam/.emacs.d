;;; tests/web-browsing/eww-tests.el --- web-browsing/autoload/eww.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'eww)

(load-module-file "modules/web-browsing/autoload/eww.el")

(defun eww-tests--insert-links ()
  "Two shr links with surrounding prose; returns (start-a . start-b)."
  (insert "intro ")
  (let ((a (point)))
    (insert (propertize "Link A" 'shr-url "https://a.example/"))
    (insert "\nmiddle text\n")
    (let ((b (point)))
      (insert (propertize "Link B" 'shr-url "https://b.example/"))
      (insert " outro\n")
      (cons a b))))

(describe "eww--capture-url-on-page"
  (it "collects LABEL @ URL strings in buffer order"
    (with-temp-buffer
      (eww-tests--insert-links)
      (let ((links (eww--capture-url-on-page)))
        (expect (length links) :to-equal 2)
        (expect (nth 0 links) :to-match "Link A  @ https://a\\.example/")
        (expect (nth 1 links) :to-match "Link B  @ https://b\\.example/"))))

  (it "prepends line,column (point) coordinates when POSITION is non-nil"
    (with-temp-buffer
      (let ((starts (eww-tests--insert-links)))
        (let ((links (eww--capture-url-on-page t)))
          (expect (nth 0 links)
                  :to-match (format "(%d)[ \t]+~ Link A  @ https://a\\.example/"
                                    (car starts))))))))

(describe "eww-jump-to-url-on-page"
  (it "jumps to the selected link's position"
    (with-temp-buffer
      ;; fill first - eww-mode buffers are read-only
      (let ((starts (eww-tests--insert-links)))
        (delay-mode-hooks (eww-mode))
        (goto-char (point-min))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt coll &rest _)
                     (seq-find (lambda (s) (string-match-p "Link B" s)) coll)))
                  ((symbol-function 'recenter) #'ignore))
          ;; ARG t = whole buffer, no window narrowing (batch has no window)
          (eww-jump-to-url-on-page t))
        (expect (point) :to-equal (cdr starts))))))

(describe "eww--rename-buffer"
  (it "uses the page title when present"
    (with-temp-buffer
      (setq-local eww-data '(:title "Some Page" :url "https://x.example/"))
      (eww--rename-buffer)
      (expect (buffer-name) :to-match "\\*Some Page # eww\\*")))

  (it "falls back to the url for untitled pages"
    (with-temp-buffer
      (setq-local eww-data (list :title "" :url "https://y.example/"))
      (eww--rename-buffer)
      (expect (buffer-name) :to-match "\\*https://y\\.example/ # eww\\*"))))

(describe "eww-hn-discussion"
  (it "searches HN for the page's url, on the url attribute alone"
    (with-temp-buffer
      (setq-local eww-data '(:url "https://example.com/post"))
      (let (call)
        (cl-letf (((symbol-function 'consult-hn)
                   (lambda (&rest args) (setq call args))))
          (eww-hn-discussion))
        (expect call :to-equal '("https://example.com/post" :url-match t)))))

  (it "refuses to search when the buffer shows no page"
    (with-temp-buffer
      (cl-letf (((symbol-function 'consult-hn) #'ignore))
        (expect (eww-hn-discussion) :to-throw 'user-error)))))

(defun eww-tests--eww-config-forms ()
  "Read the (use-package eww ...) :config forms out of web-browsing/config.el."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "modules/web-browsing/config.el" test-config-root))
    (let (form)
      (while (and (setq form (ignore-errors (read (current-buffer))))
                  (not (and (eq (car-safe form) 'use-package)
                            (eq (cadr form) 'eww)))))
      (expect form :to-be-truthy)
      (use-package-body-forms (cddr form) :config))))

(describe "eww :config"
  (it "gives every eww buffer variable-pitch prose and visual wrapping"
    (let ((eww-mode-hook nil)
          (eww-after-render-hook nil))
      (eval `(progn ,@(eww-tests--eww-config-forms)) t)
      (expect (memq 'variable-pitch-mode eww-mode-hook) :to-be-truthy)
      (expect (memq 'visual-line-mode eww-mode-hook) :to-be-truthy)))

  (it "binds the HN search under the localleader"
    (expect (cl-search '("h" function eww-hn-discussion)
                       (flatten-tree (eww-tests--eww-config-forms))
                       :test #'equal)
            :to-be-truthy)))

(describe "eww readable-by-default"
  ;; run the real :config against the real built-in eww, so a future
  ;; eww-readable-urls format change breaks here and not at browse time
  (before-all
    (eval `(progn ,@(eww-tests--eww-config-forms)) t))

  (it "makes eww render any url readable"
    (expect (eww-default-readable-p "https://anything.example/some/page")
            :to-be-truthy))

  (it "keeps the (REGEXP . nil) opt-out lane working"
    (let ((eww-readable-urls (cons '("\\`https://opt\\.out/" . nil)
                                   eww-readable-urls)))
      (expect (eww-default-readable-p "https://opt.out/page") :to-be nil)
      (expect (eww-default-readable-p "https://still.example/page")
              :to-be-truthy))))
