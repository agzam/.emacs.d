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
