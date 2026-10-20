;;; tests/bindings/config-tests.el --- bindings/config.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defvar bindings-tests--cancel-advice
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "modules/bindings/config.el" test-config-root))
    (search-forward "(defadvice! cancel-prefix-quietly-a")
    (goto-char (match-beginning 0))
    (read (current-buffer)))
  "The advice form, read from source: config.el needs general to load.")

(eval bindings-tests--cancel-advice t)

(describe "cancel-prefix-quietly-a"
  (it "wraps undefined"
    (expect (cadr (memq :around bindings-tests--cancel-advice))
            :to-equal '(function undefined)))

  (it "stays quiet when ESC or C-g ends the key sequence"
    (dolist (key '(?\e ?\C-g))
      (let ((last-command-event key)
            called)
        (cancel-prefix-quietly-a (lambda () (setq called t)))
        (expect called :to-be nil))))

  (it "reports any other undefined key"
    (let ((last-command-event ?a)
          called)
      (cancel-prefix-quietly-a (lambda () (setq called t)))
      (expect called :to-be t))))

;;; config-tests.el ends here
