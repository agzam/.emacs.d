;;; tests/writing/google-translate-tests.el --- writing/autoload/google-translate.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; The autoload file requires the (absent) package; fake the feature and
;; the defcustoms/fns it reaches for.  Valued defvars - module fns let-bind
;; and setq them, so they must be special (the embark-indicators lesson).
(provide 'google-translate)
;; translate-at-point-smart requires this (deferred) fork file at call time.
(provide 'google-translate-posframe)
(defvar google-translate-default-source-language "auto")
(defvar google-translate-default-target-language "en")

(load-module-file "modules/writing/autoload/google-translate.el")

(describe "number-to-words"
  (it "runs node from the quarantined cache workdir"
    (let (install-attempted seen-dir)
      (cl-letf (((symbol-function 'call-process)
                 (lambda (prog &rest _)
                   (when (equal prog "npm") (setq install-attempted t))
                   0))
                ((symbol-function 'shell-command-to-string)
                 (lambda (_cmd)
                   (setq seen-dir default-directory)
                   "twenty\n")))
        (expect (number-to-words 20) :to-equal "twenty")
        (expect install-attempted :to-be nil)
        (expect seen-dir :to-equal number-to-words-workdir)
        (expect (file-in-directory-p number-to-words-workdir doom-cache-dir)
                :to-be-truthy)
        (expect (file-directory-p number-to-words-workdir) :to-be-truthy))))

  (it "installs the npm package on first miss"
    (let (installed)
      (cl-letf (((symbol-function 'call-process)
                 (lambda (prog &rest args)
                   (pcase prog
                     ("node" (if installed 0 1))
                     ("npm" (setq installed t) 0))))
                ((symbol-function 'shell-command-to-string)
                 (lambda (_cmd) "five\n")))
        (expect (number-to-words 5) :to-equal "five")
        (expect installed :to-be t))))

  (it "errors out when the install fails"
    (cl-letf (((symbol-function 'call-process) (lambda (&rest _) 1)))
      (expect (number-to-words 5) :to-throw 'user-error))))

(describe "google-translate-years-to-words-a"
  :var (received recorder real-ntw)

  (before-each
    (setq received nil
          recorder (lambda (src tgt text &optional dest)
                     (setq received (list src tgt text dest)))
          real-ntw (symbol-function 'number-to-words))
    (setf (symbol-function 'number-to-words)
          (lambda (n)
            (pcase n
              (2023 "two thousand, twenty-three")
              (2021 "two thousand, twenty-one")
              (_ (number-to-string n))))))

  (after-each
    (setf (symbol-function 'number-to-words) real-ntw))

  (it "spells out year-looking numbers for English sources"
    (google-translate-years-to-words-a
     recorder "en" "es" "2023 was better than 2021")
    (expect (nth 2 received)
            :to-equal
            "two thousand and twenty-three was better than two thousand and twenty-one"))

  (it "leaves non-English sources alone"
    (google-translate-years-to-words-a
     recorder "ru" "en" "2023 rocks")
    (expect (nth 2 received) :to-equal "2023 rocks")))

(describe "translate--set-lang"
  (it "pairs a source language with its habitual target"
    (dolist (case '(("en" . "es") ("ru" . "en") ("es" . "en")))
      (let ((google-translate-default-source-language "auto")
            (google-translate-default-target-language "xx"))
        (translate--set-lang :source (car case))
        (expect google-translate-default-source-language
                :to-equal (car case))
        (expect google-translate-default-target-language
                :to-equal (cdr case)))))

  (it "sets the target directly"
    (let ((google-translate-default-source-language "auto")
          (google-translate-default-target-language "en"))
      (translate--set-lang :target "ru")
      (expect google-translate-default-target-language :to-equal "ru"))))

(describe "translate-transient layout"
  (it "wires the documented suffixes; module-owned ones are defined"
    (let ((cmds (transient-layout-commands
                 (get 'translate-transient 'transient--layout))))
      (dolist (sym '(translate--set-source translate--set-target
                     translate--minibuffer translate--translate
                     ;; loads with the posframe file in live sessions
                     google-translate-posframe-mode))
        (expect (memq sym cmds) :to-be-truthy))
      (dolist (sym '(translate--set-source translate--set-target
                     translate--minibuffer translate--translate))
        (expect (fboundp sym) :to-be-truthy)))))

(describe "translate-at-point-smart"
  (it "translates the grabbed text via the posframe helper"
    (let (received)
      (cl-letf (((symbol-function 'google-translate-posframe--get-text-to-translate)
                 (lambda () "hola"))
                ((symbol-function 'google-translate-translate)
                 (lambda (src tgt text &optional _)
                   (setq received (list src tgt text)))))
        (translate-at-point-smart)
        (expect received
                :to-equal (list google-translate-default-source-language
                                google-translate-default-target-language
                                "hola")))))

  (it "does nothing when there is no text to translate"
    (let (called)
      (cl-letf (((symbol-function 'google-translate-posframe--get-text-to-translate)
                 (lambda () nil))
                ((symbol-function 'google-translate-translate)
                 (lambda (&rest _) (setq called t))))
        (translate-at-point-smart)
        (expect called :to-be nil)))))