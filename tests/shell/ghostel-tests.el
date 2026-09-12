;;; tests/shell/ghostel-tests.el --- shell/autoload/ghostel.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

(load-module-file "modules/shell/autoload/ghostel.el")

;; Neither ghostel nor evil-ghostel is installed in the batch tier; the
;; functions under test only need these entry points to exist.  The
;; state variables are declared here as well so the specs' `let' forms
;; bind them dynamically (a valueless `defvar' is file-local).
(defvar ghostel--term)
(defvar ghostel--cursor-pos)
(defvar ghostel--cursor-char-pos)
(defun ghostel--viewport-row-at (pos) (1- (line-number-at-pos pos)))
(defun ghostel--send-encoded (_key-name _mods &optional _utf8))
(defun evil-ghostel--sync-render ())
(defun evil-ghostel--input-end ())

(defconst ghostel-tests--prompt "at 15:48:26 zsh ❯ "
  "A starship prompt row as the renderer materializes it, 18 cells wide.")

(defun ghostel-tests--insert-row (input)
  "Insert the prompt row followed by INPUT, with the renderer's glyph claim.
The ❯ comes from a fallback font wider than the cell, so the renderer
gives it `min-width' 2 and hides the blank cell after it."
  (let ((start (point)))
    (insert ghostel-tests--prompt input)
    (let ((glyph (+ start (length "at 15:48:26 zsh "))))
      (put-text-property glyph (1+ glyph) 'display '((min-width (2)) (height 1.0)))
      (put-text-property (1+ glyph) (+ 2 glyph) 'display '(space :width 0)))))

(describe "ghostel-cell-distance"
  (it "counts one cell per character between two positions"
    (with-temp-buffer
      (insert "abcd")
      (expect (ghostel-cell-distance 1 5) :to-equal 4)))

  (it "is negative when TO precedes FROM"
    (with-temp-buffer
      (insert "abcd")
      (expect (ghostel-cell-distance 5 2) :to-equal -3)))

  (it "counts a wide character as two cells"
    (with-temp-buffer
      (insert "a漢b")
      (expect (ghostel-cell-distance 1 4) :to-equal 4)))

  (it "ignores the renderer's claimed cell, unlike current-column"
    (with-temp-buffer
      (ghostel-tests--insert-row "ls")
      (let ((l (1+ (length ghostel-tests--prompt))))
        ;; the premise: Emacs undercounts the row by the hidden cell
        (expect (save-excursion (goto-char l) (current-column))
                :to-equal (1- (length ghostel-tests--prompt)))
        (expect (ghostel-cell-distance (point-min) l)
                :to-equal (length ghostel-tests--prompt))))))

(describe "ghostel-cell-column"
  (it "counts cells from the start of the row"
    (with-temp-buffer
      (insert "xy\n")
      (ghostel-tests--insert-row "ls")
      (expect (ghostel-cell-column (point-max))
              :to-equal (+ 2 (length ghostel-tests--prompt))))))

(defmacro ghostel-tests--with-row (input &rest body)
  "Run BODY in a buffer holding the prompt row plus INPUT.
The terminal cursor sits at the end of INPUT; INPUT-START and INPUT-END
name the buffer positions around INPUT."
  (declare (indent 1))
  `(with-temp-buffer
     (ghostel-tests--insert-row ,input)
     (setq input-start (1+ (length ghostel-tests--prompt))
           input-end (point-max))
     (let ((ghostel--term t)
           (ghostel--cursor-pos (cons (+ (length ghostel-tests--prompt)
                                         (length ,input))
                                      0))
           (ghostel--cursor-char-pos (point-max)))
       ,@body)))

(describe "evil-ghostel-goto-input-position-a"
  :var (sent input-start input-end)
  (before-each
    (setq sent nil)
    (spy-on 'ghostel--send-encoded :and-call-fake
            (lambda (key _mods &optional _utf8) (push key sent)))
    (spy-on 'evil-ghostel--sync-render)
    (spy-on 'evil-ghostel--input-end :and-call-fake (lambda () input-end)))

  (it "moves one cell left to insert between l and s"
    (ghostel-tests--with-row "ls"
      (expect (evil-ghostel-goto-input-position-a (1+ input-start)) :to-be t)
      (expect sent :to-equal '("left"))
      (expect (point) :to-equal (1+ input-start))))

  (it "moves two cells left to the start of the input"
    (ghostel-tests--with-row "ls"
      (evil-ghostel-goto-input-position-a input-start)
      (expect sent :to-equal '("left" "left"))))

  (it "sends nothing and skips the render sync when already there"
    (ghostel-tests--with-row "ls"
      (evil-ghostel-goto-input-position-a input-end)
      (expect sent :to-equal nil)
      (expect 'evil-ghostel--sync-render :not :to-have-been-called)))

  (it "moves right by cells and syncs the render"
    (ghostel-tests--with-row "abcd"
      (let ((ghostel--cursor-pos (cons (length ghostel-tests--prompt) 0))
            (ghostel--cursor-char-pos input-start))
        (evil-ghostel-goto-input-position-a (+ 3 input-start))
        (expect sent :to-equal '("right" "right" "right"))
        (expect 'evil-ghostel--sync-render :to-have-been-called))))

  (it "clamps a rightward target to the end of typed input"
    (ghostel-tests--with-row "abcd"
      (let ((ghostel--cursor-pos (cons (length ghostel-tests--prompt) 0))
            (ghostel--cursor-char-pos input-start))
        ;; a trailing autosuggestion ends the typed input two cells early
        (setq input-end (+ 2 input-start))
        (evil-ghostel-goto-input-position-a (+ 4 input-start))
        (expect sent :to-equal '("right" "right"))
        (expect (point) :to-equal (+ 2 input-start)))))

  (it "crosses rows with up/down and a row-relative column"
    (with-temp-buffer
      (insert "xy\n")
      (ghostel-tests--insert-row "abcd")
      (let ((ghostel--term t)
            (ghostel--cursor-pos (cons (+ 4 (length ghostel-tests--prompt)) 1))
            (ghostel--cursor-char-pos (point-max)))
        (evil-ghostel-goto-input-position-a 2)
        (expect (cl-count "up" sent :test #'equal) :to-equal 1)
        (expect (cl-count "left" sent :test #'equal)
                :to-equal (+ 3 (length ghostel-tests--prompt)))
        (expect (point) :to-equal 2))))

  (it "does nothing without a live terminal"
    (with-temp-buffer
      (ghostel-tests--insert-row "ls")
      (let ((ghostel--term nil)
            (ghostel--cursor-pos '(20 . 0))
            (ghostel--cursor-char-pos (point-max)))
        (expect (evil-ghostel-goto-input-position-a 1) :to-be nil)
        (expect sent :to-equal nil)))))

(describe "evil-ghostel-reset-cursor-point-a"
  (it "jumps to the renderer's cursor position"
    (with-temp-buffer
      (ghostel-tests--insert-row "ls")
      (let ((ghostel--cursor-char-pos 3)
            (called nil))
        (evil-ghostel-reset-cursor-point-a (lambda () (setq called t)))
        (expect (point) :to-equal 3)
        (expect called :to-be nil))))

  (it "falls back to the original when the position is unknown"
    (with-temp-buffer
      (let ((ghostel--cursor-char-pos nil)
            (called nil))
        (evil-ghostel-reset-cursor-point-a (lambda () (setq called t)))
        (expect called :to-be t)))))

;;; ghostel-tests.el ends here
