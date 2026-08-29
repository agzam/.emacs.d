;;; tests/evil/config-tests.el --- evil module specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(defun evil-tests--read-form (file marker)
  "Read the form that starts at MARKER in FILE, relative to the config root.
The specs read source: config.el cannot be loaded in the batch tier, where
neither evil nor general (behind `map!') is available."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file test-config-root))
    (emacs-lisp-mode)
    (goto-char (point-min))
    (search-forward marker)
    (goto-char (match-beginning 0))
    (read (current-buffer))))

;; Reductions of the evil state the advice sits between: the paste commands
;; it hangs on, the change markers evil moves while pasting, and the three
;; variables `evil-visual-restore' reads back.
(defvar evil-tests--markers nil)
(defvar evil-visual-mark nil)
(defvar evil-visual-point nil)
(defvar evil-visual-selection nil)

(defun evil-get-marker (char &optional _raw)
  "Answer the position recorded for CHAR in `evil-tests--markers'."
  (alist-get char evil-tests--markers))

(defun evil-paste-before (&rest _) nil)
(defun evil-paste-after (&rest _) nil)

(defvar evil-tests--paste-advice
  (evil-tests--read-form "modules/evil/config.el"
                         "(defadvice! paste-sets-visual-selection-a"))

(defvar evil-tests--yank-advice
  (evil-tests--read-form "modules/evil/config.el"
                         "(defadvice! yank-sets-visual-selection-a"))

(eval (evil-tests--read-form "modules/evil/config.el"
                             "(defun remember-visual-selection")
      t)
(eval evil-tests--paste-advice t)
(eval evil-tests--yank-advice t)

(describe "paste-sets-visual-selection-a"
  (it "hangs on both paste commands"
    (expect (cadr (memq :after evil-tests--paste-advice))
            :to-equal '(quote (evil-paste-before evil-paste-after))))

  (it "points evil-visual-restore at the text just pasted"
    ;; The problem it solves: `gv' after a paste restored a selection made
    ;; long before it, or errored with nothing stored at all.
    (with-temp-buffer
      (insert "0123456789abcdef")
      (let ((evil-tests--markers '((?\[ . 5) (?\] . 10)))
            (evil-visual-mark nil)
            (evil-visual-point nil)
            (evil-visual-selection nil))
        (evil-paste-after)
        (expect (marker-position evil-visual-mark) :to-equal 5)
        (expect (marker-position evil-visual-point) :to-equal 10)
        (expect evil-visual-selection :to-be 'char))))

  (it "records the bounds as markers, so later edits carry them"
    (with-temp-buffer
      (insert "0123456789abcdef")
      (let ((evil-tests--markers '((?\[ . 5) (?\] . 10)))
            (evil-visual-mark nil)
            (evil-visual-point nil)
            (evil-visual-selection nil))
        (evil-paste-after)
        (goto-char 1)
        (insert "XXX")
        (expect (marker-position evil-visual-mark) :to-equal 8)
        (expect (marker-position evil-visual-point) :to-equal 13))))

  (it "keeps the stored selection when the paste moved no markers"
    (with-temp-buffer
      (insert "0123456789")
      (let ((evil-tests--markers nil)
            (evil-visual-mark (copy-marker 1))
            (evil-visual-point (copy-marker 3))
            (evil-visual-selection 'line))
        (evil-paste-before)
        (expect (marker-position evil-visual-mark) :to-equal 1)
        (expect (marker-position evil-visual-point) :to-equal 3)
        (expect evil-visual-selection :to-be 'line)))))

(describe "yank-sets-visual-selection-a"
  (it "covers every paste command through insert-for-yank"
    ;; The gap it closes: the system paste key runs plain `yank', which
    ;; leaves Evil's change markers where the last Evil paste left them.
    (expect (cadr (memq :around evil-tests--yank-advice))
            :to-equal '(function insert-for-yank)))

  (it "points evil-visual-restore at the text a plain yank inserted"
    (with-temp-buffer
      (insert "before after")
      (goto-char 8)
      (let ((evil-visual-mark nil)
            (evil-visual-point nil)
            (evil-visual-selection nil))
        (insert-for-yank "YANKED")
        (expect (buffer-substring-no-properties
                 (marker-position evil-visual-mark)
                 (1+ (marker-position evil-visual-point)))
                :to-equal "YANKED")
        (expect evil-visual-selection :to-be 'char))))

  (it "leaves the stored selection alone when nothing was inserted"
    (with-temp-buffer
      (insert "text")
      (let ((evil-visual-mark (copy-marker 1))
            (evil-visual-point (copy-marker 3))
            (evil-visual-selection 'line))
        (insert-for-yank "")
        (expect (marker-position evil-visual-mark) :to-equal 1)
        (expect evil-visual-selection :to-be 'line)))))

;;; config-tests.el ends here
