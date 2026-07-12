;;; tests/web-browsing/browser-tests.el --- web-browsing/autoload/browser.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/web-browsing/autoload/browser.el")

(describe "browser--goto-tab-completing-fn"
  :var* ((coll '(("https://a.example\tTab A\t#window:1 #tab:2"
                  . (:windowIndex 1 :tabIndex 2 :url "https://a.example"
                     :title "Tab A" :active t))
                 ("https://b.example\tTab B\t#window:1 #tab:3"
                  . (:windowIndex 1 :tabIndex 3 :url "https://b.example"
                     :title "Tab B" :active nil))))
         (table (browser--goto-tab-completing-fn coll)))

  (it "completes over the row strings"
    (expect (funcall table "" nil t)
            :to-have-same-items-as (mapcar #'car coll)))

  (it "returns url-category metadata with a tab/url annotation"
    (let* ((md (funcall table "" nil 'metadata))
           ;; entries are (KEY . CLOSURE) - the closure is spliced in
           (ann (cdr (assq 'annotation-function (cdr md)))))
      (expect (alist-get 'category (cdr md)) :to-equal 'url)
      (expect (funcall ann (caar coll)) :to-match "2 https://a\\.example")))

  (it "keeps rows in given order via display-sort-function"
    (let* ((md (funcall table "" nil 'metadata))
           (sort-fn (cdr (assq 'display-sort-function (cdr md)))))
      (expect (funcall sort-fn '("z" "a")) :to-equal '("z" "a")))))
