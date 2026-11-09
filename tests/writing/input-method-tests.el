;;; tests/writing/input-method-tests.el --- writing/autoload/input-method.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/writing/autoload/input-method.el")

;; evil is absent from the batch tier; these stand in for its locals.
(defvar evil-local-mode nil)
(defvar-local evil-state nil)
(defvar-local evil-input-method nil)

(defun input-method-tests--presses (n)
  "Press `cycle-input-method' N times in a row; return the method after each."
  (let (seen (last-command nil))
    (dotimes (_ n)
      (cycle-input-method)
      (push (input-method-in-effect) seen)
      (setq last-command 'cycle-input-method))
    (nreverse seen)))

(defmacro input-method-tests--with-state (recent &rest body)
  "Run BODY in a fresh buffer with RECENT as `input-method-recent'."
  (declare (indent 1))
  `(let ((input-method-cycle '("russian-computer" "spanish-postfix"))
         (input-method-recent ,recent)
         (cycle-input-method--walk nil)
         (input-method-activate-hook (list #'input-method-note-h))
         (default-input-method "russian-computer"))
     (with-temp-buffer
       ,@body)))

(describe "cycle-input-method--ring"
  (it "puts no input method right after a method, the rest by recency"
    (let ((input-method-cycle '("russian-computer" "spanish-postfix"))
          (input-method-recent '("spanish-postfix")))
      (expect (cycle-input-method--ring "russian-computer")
              :to-equal '("russian-computer" nil "spanish-postfix"))
      (expect (cycle-input-method--ring nil)
              :to-equal '(nil "spanish-postfix" "russian-computer"))))

  (it "ignores remembered methods that left `input-method-cycle'"
    (let ((input-method-cycle '("russian-computer" "spanish-postfix"))
          (input-method-recent '("latin-postfix" "spanish-postfix")))
      (expect (cycle-input-method--ring nil)
              :to-equal '(nil "spanish-postfix" "russian-computer")))))

(describe "cycle-input-method"
  (it "turns on the most recently used method from none"
    (input-method-tests--with-state '("spanish-postfix" "russian-computer")
      (expect (input-method-tests--presses 1) :to-equal '("spanish-postfix"))
      (expect current-input-method :to-equal "spanish-postfix")))

  (it "turns a method off with one press"
    (input-method-tests--with-state nil
      (activate-input-method "russian-computer")
      (expect (input-method-tests--presses 1) :to-equal '(nil))))

  (it "walks on through the methods while presses repeat"
    (input-method-tests--with-state nil
      (activate-input-method "russian-computer")
      (expect (input-method-tests--presses 3)
              :to-equal '(nil "spanish-postfix" "russian-computer"))))

  (it "starts over from the buffer's method after another command"
    (input-method-tests--with-state '("russian-computer" "spanish-postfix")
      (input-method-tests--presses 2)
      (expect current-input-method :to-equal "spanish-postfix")
      (let ((last-command 'self-insert-command))
        (cycle-input-method))
      (expect current-input-method :to-be nil)))

  (it "remembers the method a walk ends on as the most recent"
    (input-method-tests--with-state '("russian-computer" "spanish-postfix")
      (input-method-tests--presses 2)
      (expect input-method-recent
              :to-equal '("spanish-postfix" "russian-computer"))
      (let ((last-command 'self-insert-command))
        (cycle-input-method)
        (cycle-input-method))
      (expect current-input-method :to-equal "spanish-postfix")))

  (it "counts activations by other commands as use"
    (input-method-tests--with-state '("russian-computer" "spanish-postfix")
      (activate-input-method "spanish-postfix")
      (expect (car input-method-recent) :to-equal "spanish-postfix")))

  (it "leaves `default-input-method' alone"
    (input-method-tests--with-state '("spanish-postfix")
      (input-method-tests--presses 1)
      (expect default-input-method :to-equal "russian-computer")))

  (it "switches the method evil holds back in normal state"
    (input-method-tests--with-state '("spanish-postfix" "russian-computer")
      (cl-letf (((symbol-function 'evil-state-property)
                 (lambda (state prop &optional _)
                   (and (eq prop :input-method) (memq state '(insert emacs))))))
        (setq-local evil-local-mode t
                    evil-state 'normal)
        (expect (input-method-tests--presses 1) :to-equal '("spanish-postfix"))
        (expect evil-input-method :to-equal "spanish-postfix")
        (expect current-input-method :to-be nil)
        (expect (car input-method-recent) :to-equal "spanish-postfix")
        (let ((last-command 'evil-insert))
          (cycle-input-method))
        (expect evil-input-method :to-be nil))))

  (it "reads any method under a prefix argument, like `toggle-input-method'"
    (let (got)
      (cl-letf (((symbol-function 'toggle-input-method)
                 (lambda (&rest args) (setq got args))))
        (cycle-input-method '(4))
        (expect got :to-equal '((4) t))))))
