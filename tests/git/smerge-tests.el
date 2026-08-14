;;; tests/git/smerge-tests.el --- git/autoload/smerge.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/git/autoload/smerge.el")

(describe "smerge-transient"
  (it "binds the resolution commands to their keys"
    (dolist (pair '(("n" . smerge-next)
                    ("p" . smerge-prev)
                    ("u" . smerge-keep-upper)
                    ("l" . smerge-keep-lower)
                    ("RET" . smerge-keep-current)
                    ("a" . smerge-keep-all)
                    ("r" . smerge-resolve)
                    ("E" . smerge-ediff)
                    ("q" . transient-quit-one)))
      ;; transient-get-suffix returns the suffix spec list, not an object
      (let ((suffix (transient-get-suffix 'smerge-transient (car pair))))
        (expect (plist-get (cdr suffix) :command) :to-be (cdr pair)))))

  (it "stays up across resolutions and passes foreign keys through"
    (let ((prefix (get 'smerge-transient 'transient--prefix)))
      (expect (oref prefix transient-suffix) :to-be 'transient--do-stay)
      (expect (oref prefix transient-non-suffix) :to-be 'transient--do-stay)))

  (it "exits for ediff"
    ;; transient-get-suffix returns the suffix spec list, not an object
    (let ((suffix (transient-get-suffix 'smerge-transient "E")))
      (expect (plist-member (cdr suffix) :transient) :to-be-truthy)
      (expect (plist-get (cdr suffix) :transient) :to-be nil)))

  (it "renders as title-less columns of at most two rows"
    (let* ((group (car (aref (get 'smerge-transient 'transient--layout) 2)))
           (columns (aref group 2)))
      (expect (aref group 0) :to-be 'transient-columns)
      (expect (length columns) :to-equal 5)
      (dolist (col columns)
        (expect (aref col 0) :to-be 'transient-column)
        ;; no Navigate/Keep/Other headings - the keys speak for themselves
        (expect (plist-get (aref col 1) :description) :to-be nil)
        (expect (length (aref col 2)) :to-be-less-than 3))))

  (it "keeps the count line tight, no trailing blank"
    (let* ((group (car (aref (get 'smerge-transient 'transient--layout) 2)))
           (desc (plist-get (aref group 1) :description)))
      (with-temp-buffer
        (insert "<<<<<<< a\nx\n=======\ny\n>>>>>>> b\n")
        (let ((transient--original-buffer (current-buffer)))
          (expect (funcall desc) :to-equal "smerge: 1 conflict(s) left"))))))
