;;; tests/e2e/embark-cycle.el --- backward target cycling -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; The backward step lives inside a running `embark-act': the prompter
;; answers `embark-cycle' with a negative prefix argument and the act loop
;; rotates a list local to it.  Only real keypresses reach that loop, and
;; `embark-insert' writes whichever target is current into the buffer, so
;; the resulting text names the target the keys landed on.

;; `embark-insert' inserting a whole line takes its multiline path, which
;; calls `looking-back' with 40 as the limit - an absolute position, so a
;; shorter paragraph than that errors after the text lands.
(defvar embark-cycle-e2e-text "alpha beta. gamma delta. epsilon zeta eta theta."
  "One paragraph of sentences: every target level inserts differently.")

(defvar embark-cycle-e2e-cases
  '(("cycle forward once: sentence"
     1 nil "alpha beta.alpha beta. gamma delta. epsilon zeta eta theta.")
    ("cycle forward twice: paragraph"
     2 nil "alpha beta. gamma delta. epsilon zeta eta theta.\nalpha beta. gamma delta. epsilon zeta eta theta.\n")
    ;; two forward and one back must land where one forward lands
    ("DEL steps back from the paragraph to the sentence"
     2 t "alpha beta.alpha beta. gamma delta. epsilon zeta eta theta."))
  "(LABEL FORWARD-STEPS BACK-STEP WANT) per case.")

(defun embark-cycle-e2e ()
  "Drive `embark-act' over the target cycle and step back out of it."
  (require 'embark)
  (require 'which-key)
  (mapcar
   (pcase-lambda (`(,label ,steps ,back ,want))
     (e2e-act-case
      (list :label label
            :ext "txt"
            :text embark-cycle-e2e-text
            :search "beta"
            :type 'identifier
            :keys (string-join
                   (append (make-list steps embark-cycle-key)
                           (and back '("DEL"))
                           '("i"))
                   " ")
            :want want)))
   embark-cycle-e2e-cases))

(add-to-list 'e2e-scenarios #'embark-cycle-e2e)
