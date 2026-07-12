;;; tests/writing/misc-tests.el --- writing/autoload/misc.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/writing/autoload/misc.el")

(describe "find-latest-png"
  :var* ((dir (make-temp-file "find-latest-png" t)))

  (after-all (delete-directory dir t))

  (it "returns a recently created png and ignores stale ones"
    (let ((recent (expand-file-name "aa.png" dir))
          (stale (expand-file-name "zz.png" dir))
          (noise (expand-file-name "bb.txt" dir))
          (now (float-time)))
      (dolist (f (list recent stale noise)) (write-region "" nil f nil 'silent))
      (set-file-times recent (seconds-to-time (- now 30)))
      (set-file-times stale (seconds-to-time (- now 3000)))
      (expect (find-latest-png dir) :to-equal recent)))

  (it "returns nil when every png is old"
    (let ((dir (make-temp-file "find-latest-png-old" t))
          (old (expand-file-name "aa.png" dir)))
      (unwind-protect
          (progn
            (write-region "" nil old nil 'silent)
            (set-file-times old (seconds-to-time (- (float-time) 3000)))
            (expect (find-latest-png dir) :to-be nil))
        (delete-directory dir t)))))

(describe "insert-bracket-pair"
  (it "self-inserts an opening bracket (smartparens closes it in live sessions)"
    (with-temp-buffer
      (insert-bracket-pair)
      (expect (buffer-string) :to-equal "[")
      (expect (point) :to-equal 2))))