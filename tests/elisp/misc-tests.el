;;; tests/elisp/misc-tests.el --- elisp/autoload/misc.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/elisp/autoload/misc.el")

(describe "datetime<->timestamp"
  ;; round trip cancels the local-TZ dependence of date-to-time
  (it "round-trips a datetime through milliseconds"
    (let* ((input "2023-05-21 12:09:31")
           (ms (datetime->timestamp input)))
      (expect ms :to-match "\\`[0-9]+\\'")
      (expect (timestamp->datetime (string-to-number ms))
              :to-equal input)))

  (it "reads 13-digit millisecond stamps"
    (let ((out (timestamp->datetime 1684672171000)))
      (expect out :to-match "\\`[0-9]\\{4\\}-"))))

(describe "elisp-fully-qualified-name"
  (it "derives ns/name from the defining library"
    (with-temp-buffer
      (insert "datetime->timestamp")
      (goto-char (point-min))
      (expect (elisp-fully-qualified-name)
              :to-equal "misc/datetime->timestamp")))

  (it "falls back to the visited file's stem for unknown symbols"
    (let ((file (make-temp-file "myns" nil ".el")))
      (unwind-protect
          (with-current-buffer (find-file-noselect file)
            (insert "totally-undefined-thing-xyz")
            (goto-char (point-min))
            (expect (elisp-fully-qualified-name)
                    :to-match "/totally-undefined-thing-xyz\\'")
            (set-buffer-modified-p nil)
            (kill-buffer))
        (delete-file file))))

  (it "returns nil with no symbol at point"
    (with-temp-buffer
      (expect (elisp-fully-qualified-name) :to-be nil))))
