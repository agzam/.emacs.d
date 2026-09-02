;;; tests/general/avy-tests.el --- general/autoload/avy.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/general/autoload/avy.el")

;; avy never loads in the batch env, and the value-less `defvar's in the
;; module file only mark these special inside that file - without a global
;; declaration here the let bindings below would be lexical and the code
;; under test would read the void globals.
(defvar avy-action nil)
(defvar avy-dispatch-alist nil)
(defvar avy-single-candidate-jump t)

(defvar avy-dispatch-alist-fixture
  '((?x . avy-action-kill-move)
    (?y . avy-action-yank)
    (?. . avy-action-embark)))

(describe "avy-dispatch-action-name"
  (it "drops the avy-action- prefix"
    (expect (avy-dispatch-action-name 'avy-action-yank) :to-equal "yank"))
  (it "keeps the name of a function outside avy's namespace"
    (expect (avy-dispatch-action-name 'embark-act) :to-equal "embark-act"))
  (it "labels an anonymous action"
    (expect (avy-dispatch-action-name (lambda (_pt) nil)) :to-equal "custom")))

(describe "avy-dispatch-guide"
  (before-each (spy-on 'message))
  (it "lists every dispatch key with its action"
    (let ((avy-action nil)
          (avy-dispatch-alist avy-dispatch-alist-fixture))
      (avy-dispatch-guide)
      (expect (substring-no-properties
               (nth 1 (spy-calls-args-for 'message 0)))
              :to-equal "x kill-move  y yank  . embark")))
  (it "reports the armed action instead of the key list"
    (let ((avy-action 'avy-action-teleport)
          (avy-dispatch-alist avy-dispatch-alist-fixture))
      (avy-dispatch-guide)
      (expect (substring-no-properties
               (nth 1 (spy-calls-args-for 'message 0)))
              :to-equal "teleport: pick a candidate"))))

(describe "avy-dispatch-guide-a"
  ;; The advice cannot assert on redisplay in batch, so each spec inspects
  ;; the timer from inside the wrapped call - the point where avy would be
  ;; blocked in `read-key'.
  (let (armed)
    (before-each (setq armed 'unset
                       avy-dispatch-guide--timer nil))

    (defun avy-tests--run (candidates &optional body)
      "Call the advice over CANDIDATES, recording whether a timer was armed.
BODY runs inside the wrapped call."
      (avy-dispatch-guide-a
       (lambda (_candidates &rest _args)
         (setq armed (timerp avy-dispatch-guide--timer))
         (when body (funcall body))
         'selected)
       candidates))

    (it "arms the guide while several candidates are on screen"
      (let ((avy-single-candidate-jump t))
        (expect (avy-tests--run '(1 2 3)) :to-equal 'selected)
        (expect armed :to-be t)))

    (it "stays quiet when avy jumps straight to a lone candidate"
      (let ((avy-single-candidate-jump t))
        (avy-tests--run '(1))
        (expect armed :to-be nil)))

    (it "arms for a lone candidate that still has to be picked"
      (let ((avy-single-candidate-jump nil))
        (avy-tests--run '(1))
        (expect armed :to-be t)))

    (it "stays quiet when nothing matched"
      (let ((avy-single-candidate-jump t))
        (avy-tests--run nil)
        (expect armed :to-be nil)))

    (it "cancels the timer once the selection ends"
      (let ((avy-single-candidate-jump t))
        (avy-tests--run '(1 2 3))
        (expect avy-dispatch-guide--timer :to-be nil)))

    (it "cancels the timer when the selection is aborted"
      (let ((avy-single-candidate-jump t))
        (expect (catch 'done
                  (avy-tests--run '(1 2 3) (lambda () (throw 'done 'abort))))
                :to-equal 'abort)
        (expect avy-dispatch-guide--timer :to-be nil)))

    (it "clears the echo area only after the guide has fired"
      (let ((avy-single-candidate-jump t)
            (avy-action nil)
            (avy-dispatch-alist avy-dispatch-alist-fixture)
            (avy-dispatch-guide-delay 0.01))
        (spy-on 'message)
        ;; sleeping inside the wrapped call runs the pending timer, the way
        ;; a hesitation inside avy's `read-key' would
        (avy-tests--run '(1 2 3) (lambda () (sleep-for 0.1)))
        (expect (spy-calls-count 'message) :to-equal 2)
        (expect (spy-calls-args-for 'message 1) :to-equal '(nil))))

    (it "leaves the echo area alone when the guide never fired"
      (let ((avy-single-candidate-jump t)
            (avy-dispatch-guide-delay 30))
        (spy-on 'message)
        (avy-tests--run '(1 2 3))
        (expect (spy-calls-count 'message) :to-equal 0)))))

(defvar avy-tests--config-form
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "modules/general/config.el" test-config-root))
    (emacs-lisp-mode)
    (goto-char (point-min))
    (search-forward "(setopt avy-all-windows t")
    (while (ignore-errors (backward-up-list) t))
    (read (current-buffer)))
  "The real (after! avy ...) form from modules/general/config.el.")

(describe "the after! avy block"
  (it "gates on avy"
    (expect (car avy-tests--config-form) :to-be 'after!)
    (expect (cadr avy-tests--config-form) :to-be 'avy))

  (it "installs the dispatch guide advice"
    (expect (member '(advice-add (function avy--process-1)
                                 :around (function avy-dispatch-guide-a))
                    avy-tests--config-form)
            :to-be-truthy))

  ;; the regression: with `avy-single-candidate-jump' back at its default t,
  ;; a lone candidate is jumped to without a read loop, so no dispatch action
  ;; and no guide are ever reachable
  (it "keeps the read loop reachable for a lone candidate"
    (let ((setopt (seq-find (lambda (form) (eq (car-safe form) 'setopt))
                            avy-tests--config-form)))
      (expect (plist-member (cdr setopt) 'avy-single-candidate-jump)
              :to-equal '(avy-single-candidate-jump nil)))))
