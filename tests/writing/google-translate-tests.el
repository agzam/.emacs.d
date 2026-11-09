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
;; the field reads the input method in effect through this module file
(load-module-file "modules/writing/autoload/input-method.el")

(defun google-translate-tests--config-value (var)
  "Return the value the writing module's google-translate block sets VAR to."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "modules/writing/config.el" test-config-root))
    (goto-char (point-min))
    (let (form)
      (condition-case nil
          (while (not form)
            (let ((f (read (current-buffer))))
              (when (and (eq (car-safe f) 'use-package)
                         (eq (cadr f) 'google-translate))
                (setq form f))))
        (end-of-file nil))
      (when-let* ((setq-form
                   (cl-find-if (lambda (f)
                                 (and (eq (car-safe f) 'setq) (memq var f)))
                               form)))
        (eval (plist-get (cdr setq-form) var) t)))))

(defun google-translate-tests--listen-args ()
  "Return the listen program args the writing module's config sets."
  (google-translate-tests--config-value 'google-translate-listen-program-args))

(defvar google-translate-preferable-input-methods-alist
  (google-translate-tests--config-value
   'google-translate-preferable-input-methods-alist))

(describe "google-translate listen program args"
  (it "keeps ffplay's http connection open so -autoexit plays the whole clip"
    (let ((args (google-translate-tests--listen-args)))
      (expect (member "-autoexit" args) :to-be-truthy)
      (expect (cadr (member "-multiple_requests" args)) :to-equal "1")))

  (it "speeds speech up 30% without changing pitch"
    (expect (cadr (member "-af" (google-translate-tests--listen-args)))
            :to-equal "atempo=1.3")))

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
                     translate--field translate--translate
                     ;; loads with the posframe file in live sessions
                     google-translate-posframe-mode))
        (expect (memq sym cmds) :to-be-truthy))
      (dolist (sym '(translate--set-source translate--set-target
                     translate--field translate--translate))
        (expect (fboundp sym) :to-be-truthy)))))

(describe "input methods and translation languages"
  (it "maps each input method to the language it types, and back"
    (expect (translate--input-method-language nil) :to-equal "en")
    (expect (translate--input-method-language "russian-computer") :to-equal "ru")
    (expect (translate--input-method-language "spanish-postfix") :to-equal "es")
    (expect (translate--input-method-language "Emoji") :to-be nil)
    (expect (translate--language-input-method "ru") :to-be 'russian-computer)
    (expect (translate--language-input-method "es") :to-be 'spanish-postfix)
    (expect (translate--language-input-method "en") :to-be nil)
    (expect (translate--language-input-method "auto") :to-be nil)))

(describe "translate--select-source"
  (it "pairs the target and drops the source from the target choices"
    (let ((google-translate-default-source-language "auto")
          (google-translate-default-target-language "en")
          (translate--langs-target translate--langs-target))
      (translate--select-source "ru")
      (expect google-translate-default-source-language :to-equal "ru")
      (expect google-translate-default-target-language :to-equal "en")
      (expect (ring-member translate--langs-target "ru") :to-be nil))))

;; evil is absent from the batch tier; these stand in for its locals.
(defvar evil-local-mode nil)
(defvar-local evil-state nil)
(defvar-local evil-input-method nil)

(defmacro google-translate-tests--in-field (source target &rest body)
  "Run BODY in a buffer prepared as the field, translating SOURCE to TARGET."
  (declare (indent 2))
  `(let ((google-translate-default-source-language ,source)
         (google-translate-default-target-language ,target)
         (translate--langs-target translate--langs-target)
         (input-method-activate-hook nil)
         (input-method-deactivate-hook nil)
         (redrawn 0))
     (ignore redrawn)
     (cl-letf (((symbol-function 'translate--refresh-menu)
                (lambda () (setq redrawn (1+ redrawn)))))
       (with-temp-buffer
         (translate-field--prepare)
         ,@body))))

(describe "translate field buffer"
  (it "completes the whole text from earlier translations"
    (google-translate-tests--in-field "auto" "en"
      (insert "hol")
      (let ((translate-history '("hola mundo")))
        (expect (translate-field-history-capf)
                :to-equal '(1 4 ("hola mundo") :exclusive no)))
      (let ((translate-history nil))
        (expect (translate-field-history-capf) :to-be nil))))

  (it "turns on the source's method at open without re-pairing the target"
    (google-translate-tests--in-field "ru" "es"
      (expect current-input-method :to-equal "russian-computer")
      (expect google-translate-default-target-language :to-equal "es")
      (translate-field-follow-h)
      (expect redrawn :to-equal 0)))

  (it "makes the language of a newly chosen method the source, pairing the target"
    (google-translate-tests--in-field "auto" "en"
      (activate-input-method "russian-computer")
      (translate-field-follow-h)
      (expect google-translate-default-source-language :to-equal "ru")
      (expect google-translate-default-target-language :to-equal "en")
      (expect (substring-no-properties line-prefix) :to-equal "ru → en: ")
      (expect redrawn :to-equal 1)
      (deactivate-input-method)
      (translate-field-follow-h)
      (expect google-translate-default-source-language :to-equal "en")
      (expect google-translate-default-target-language :to-equal "es")))

  (it "leaves an auto source alone until the method changes"
    (google-translate-tests--in-field "auto" "en"
      (translate-field-follow-h)
      (expect google-translate-default-source-language :to-equal "auto")))

  (it "leaves the languages alone for a method without one"
    (google-translate-tests--in-field "auto" "en"
      (activate-input-method "latin-postfix")
      (translate-field-follow-h)
      (expect google-translate-default-source-language :to-equal "auto")))

  (it "follows the method evil holds back in normal state"
    (google-translate-tests--in-field "auto" "en"
      (cl-letf (((symbol-function 'evil-state-property)
                 (lambda (state prop &optional _)
                   (and (eq prop :input-method) (memq state '(insert emacs))))))
        (setq-local evil-local-mode t
                    evil-state 'normal
                    evil-input-method "spanish-postfix")
        (translate-field-follow-h)
        (expect google-translate-default-source-language :to-equal "es")))))

(describe "translate-field-edit"
  :var (seen)
  (before-each (setq seen nil))

  (cl-flet ((edit-with (typing)
              ;; TYPING runs where the user would, inside the recursive edit
              (save-window-excursion
                (delete-other-windows)
                (let ((origin (selected-window))
                      (google-translate-default-source-language "auto")
                      (google-translate-default-target-language "en"))
                  (cl-letf (((symbol-function 'exit-recursive-edit) #'ignore)
                            ((symbol-function 'recursive-edit)
                             (lambda ()
                               (setq seen (list :buffer (current-buffer)
                                                :mode major-mode
                                                :above (window-in-direction 'above)
                                                :height (window-body-height)))
                               (funcall typing))))
                    (list :result (translate-field-edit)
                          :origin-selected (eq (selected-window) origin)
                          :windows (length (window-list))))))))

    (it "edits in a one-line window below the origin, then cleans up"
      (let ((got (edit-with (lambda () (insert " hola \n") (translate-field-submit)))))
        (expect (plist-get got :result) :to-equal "hola")
        (expect (plist-get seen :mode) :to-be 'translate-field-mode)
        (expect (plist-get seen :above) :to-be-truthy)
        (expect (plist-get seen :height) :to-equal 1)
        (expect (buffer-live-p (plist-get seen :buffer)) :to-be nil)
        (expect (plist-get got :origin-selected) :to-be t)
        (expect (plist-get got :windows) :to-equal 1)))

    (it "hands back nothing when cancelled or left empty"
      (expect (plist-get (edit-with (lambda () (insert "x") (translate-field-cancel)))
                         :result)
              :to-be nil)
      (expect (plist-get (edit-with (lambda () (translate-field-submit))) :result)
              :to-be nil))))

(describe "translate-field-leave-h"
  (it "cancels once a window other than the field or the minibuffer is selected"
    (let (scheduled)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat fn &rest args) (push (cons fn args) scheduled))))
        (with-temp-buffer
          (let ((translate-field--buffer (current-buffer))
                (translate-field--depth 1))
            (cl-letf (((symbol-function 'window-buffer) (lambda (&rest _) (current-buffer))))
              (translate-field-leave-h nil))
            (expect scheduled :to-be nil)
            (translate-field-leave-h nil)
            (expect scheduled :to-equal '((translate-field--abandon 1)))))))))

(describe "translate--translate"
  :var (sent)
  (before-each (setq sent nil))

  (it "prefers typed text over the region"
    (cl-letf (((symbol-function 'google-translate-buffer)
               (lambda (&rest _) (setq sent (buffer-string)))))
      (with-temp-buffer
        (insert "region text")
        (let ((transient-mark-mode t)
              (translate--field-text "typed"))
          (set-mark (point-min))
          (goto-char (point-max))
          (activate-mark)
          (translate--translate)
          (expect sent :to-equal "typed")))))

  (it "falls back to the region when nothing was typed"
    (cl-letf (((symbol-function 'google-translate-buffer)
               (lambda (&rest _) (setq sent (buffer-string)))))
      (with-temp-buffer
        (insert "region text")
        (let ((transient-mark-mode t)
              (translate--field-text nil))
          (set-mark (point-min))
          (goto-char (point-max))
          (activate-mark)
          (translate--translate)
          (expect sent :to-equal "region text"))))))

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