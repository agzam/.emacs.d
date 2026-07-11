;;; tests/elisp/messages-tests.el --- elisp/autoload/messages.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/elisp/autoload/messages.el")

(describe "erase-messages-buffer"
  (it "empties *Messages* despite read-only"
    (with-current-buffer (messages-buffer)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "leftover line\n")))
    (erase-messages-buffer)
    (expect (with-current-buffer (messages-buffer) (buffer-string))
            :to-equal "")))

(describe "switch-to-last-elisp-buffer"
  (it "is a no-op when the remembered buffer has no window"
    (let ((last-known-elisp-buffer (generate-new-buffer " *orphan*")))
      (unwind-protect
          (expect (switch-to-last-elisp-buffer) :not :to-throw)
        (kill-buffer last-known-elisp-buffer))))

  (it "is a no-op when nothing was remembered"
    (let ((last-known-elisp-buffer nil))
      (expect (switch-to-last-elisp-buffer) :not :to-throw))))

(describe "hide-messages-window"
  (it "is a no-op without a *Messages* window"
    (expect (hide-messages-window) :not :to-throw)))
