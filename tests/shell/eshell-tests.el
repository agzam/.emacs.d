;;; tests/shell/eshell-tests.el --- shell/autoload/eshell.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'esh-mode)

(load-module-file "modules/shell/autoload/eshell.el")

(describe "eshell/b"
  :var (buf)
  (before-each
    (setq buf (generate-new-buffer "eshell-b-target"))
    (with-current-buffer buf (insert "buffer payload")))
  (after-each
    (kill-buffer buf))

  (it "returns contents of the buffer matching a regexp"
    (expect (eshell/b "eshell-b-target") :to-equal "buffer payload"))

  (it "accepts a buffer object"
    (expect (eshell/b buf) :to-equal "buffer payload"))

  (it "returns nil when nothing matches"
    (expect (eshell/b "no-such-buffer-anywhere") :to-be nil)))

(describe "fontify helpers"
  (it "eshell-buffer-contents returns the buffer text"
    (with-temp-buffer
      (insert "text here")
      (expect (substring-no-properties
               (eshell-buffer-contents (current-buffer)))
              :to-equal "text here")))

  (it "eshell-file-contents reads a file and cleans up its buffer"
    ;; file-truename: the vendored Doom defaults set find-file-visit-truename,
    ;; and macOS $TMPDIR sits behind the /var -> /private/var symlink.
    (let ((f (file-truename
              (make-temp-file "eshell-contents" nil ".txt" "file payload"))))
      (unwind-protect
          (progn
            (expect (substring-no-properties (eshell-file-contents f))
                    :to-equal "file payload")
            (expect (get-file-buffer f) :to-be nil))
        (delete-file f))))

  (it "eshell-file-contents leaves pre-existing buffers alive"
    (let* ((f (file-truename
               (make-temp-file "eshell-contents" nil ".txt" "file payload")))
           (buf (find-file-noselect f)))
      (unwind-protect
          (progn
            (expect (substring-no-properties (eshell-file-contents f))
                    :to-equal "file payload")
            (expect (buffer-live-p buf) :to-be-truthy))
        (kill-buffer buf)
        (delete-file f)))))

(describe "eshell-clear-buffer"
  (it "clears scrollback then resends input"
    (spy-on 'eshell/clear-scrollback)
    (spy-on 'eshell-send-input)
    (eshell-clear-buffer)
    (expect 'eshell/clear-scrollback :to-have-been-called)
    (expect 'eshell-send-input :to-have-been-called)))

(describe "plus-free renames"
  (it "defines the renamed commands"
    (expect (fboundp 'eshell-clear-buffer) :to-be-truthy)
    (expect (fboundp 'eshell-export-output) :to-be-truthy)
    (expect (fboundp 'eshell-here) :to-be-truthy))

  (it "leaves no plus-affixed residue"
    (expect (fboundp 'eshell-clear+) :to-be nil)
    (expect (fboundp 'eshell-export-output+) :to-be nil)))
