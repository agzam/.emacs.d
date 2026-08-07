;;; tests/completion/vertico-tests.el --- orderless dispatcher and vertico command specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

(load-module-file "modules/completion/autoload/vertico.el")

;; orderless isn't loaded here; declare its var special so the let-bindings
;; below are dynamic, not lexical.
(defvar orderless-affix-dispatch-alist)

;; Mirrors the alist set in modules/completion/config.el; the dispatchers
;; read it at call time, so orderless itself isn't needed.
(defvar test-affix-alist
  '((?! . orderless-without-literal)
    (?& . orderless-annotation)
    (?% . char-fold-to-regexp)
    (?` . orderless-initialism)
    (?= . orderless-literal)
    (?^ . orderless-literal-prefix)
    (?~ . orderless-flex)))

(describe "vertico-orderless-dispatch"
  (it "dispatches on a prefix affix"
    (let ((orderless-affix-dispatch-alist test-affix-alist))
      (expect (vertico-orderless-dispatch "=exact" 0 1)
              :to-equal '(orderless-literal . "exact"))))
  (it "dispatches on a suffix affix"
    (let ((orderless-affix-dispatch-alist test-affix-alist))
      (expect (vertico-orderless-dispatch "fuzzy~" 0 1)
              :to-equal '(orderless-flex . "fuzzy"))))
  (it "ignores a lone dispatcher character"
    (let ((orderless-affix-dispatch-alist test-affix-alist))
      (expect (vertico-orderless-dispatch "=" 0 1) :to-be #'ignore)))
  (it "leaves escaped affixes alone"
    (let ((orderless-affix-dispatch-alist test-affix-alist))
      (expect (vertico-orderless-dispatch "literal\\~" 0 1) :to-be nil)))
  (it "does not dispatch plain patterns"
    (let ((orderless-affix-dispatch-alist test-affix-alist))
      (expect (vertico-orderless-dispatch "plain" 0 1) :to-be nil))))

(describe "vertico-orderless-disambiguation-dispatch"
  (it "rewrites $ to skip consult disambiguation suffixes"
    (let ((result (vertico-orderless-disambiguation-dispatch "end$" 0 1)))
      (expect (car result) :to-be 'orderless-regexp)
      (expect (cdr result) :to-match "^end")
      (expect (cdr result) :to-match "\\$$")))
  (it "stays out of the way without a $ suffix"
    (expect (vertico-orderless-disambiguation-dispatch "end" 0 1) :to-be nil)))

;; vertico/posframe aren't loaded here; declare their vars special so the
;; let-bindings below are dynamic.  vertico--input gets a nil default like
;; vertico's own defvar-local, so `buffer-local-value' works on any buffer.
(defvar vertico--input nil)
(defvar vertico-posframe-mode)
(defvar vertico-posframe-height)
(defvar vertico-count)

(describe "vertico-detour"
  :var (log fake-mb)

  ;; The frame's real (inactive) minibuffer buffer satisfies `minibufferp',
  ;; which checks the internal minibuffer list, not the buffer name; window
  ;; functions are mocked since batch Emacs has no live minibuffer sessions.
  (before-each
    (setq log nil
          fake-mb (window-buffer (minibuffer-window)))
    (with-current-buffer fake-mb (setq-local vertico--input t)))

  (after-each
    (with-current-buffer fake-mb (kill-local-variable 'vertico--input)))

  (it "errors when no minibuffer is active"
    (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () nil)))
      (expect (vertico-detour) :to-throw 'user-error)))

  (it "errors when the active minibuffer is not a vertico session"
    (with-current-buffer fake-mb (kill-local-variable 'vertico--input))
    (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () 'mb-win))
              ((symbol-function 'window-buffer) (lambda (&optional _) fake-mb)))
      (expect (vertico-detour) :to-throw 'user-error)))

  (it "jumps to the calling window from a plain minibuffer session"
    (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () 'mb-win))
              ((symbol-function 'window-buffer) (lambda (&optional _) fake-mb))
              ((symbol-function 'minibuffer-selected-window) (lambda () 'main-win))
              ((symbol-function 'select-window)
               (lambda (w &optional _) (push (cons 'select w) log))))
      (with-current-buffer fake-mb (vertico-detour)))
    (expect (reverse log) :to-equal '((select . main-win))))

  (it "switches a posframe session to the minibuffer display before leaving"
    (let ((vertico-posframe-mode t))
      (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () 'mb-win))
                ((symbol-function 'window-buffer) (lambda (&optional _) fake-mb))
                ((symbol-function 'posframe-workable-p) (lambda () t))
                ((symbol-function 'vertico-multiform-posframe)
                 (lambda () (push 'multiform-toggle log)))
                ((symbol-function 'vertico--exhibit)
                 (lambda () (push 'exhibit log)))
                ((symbol-function 'minibuffer-selected-window) (lambda () 'main-win))
                ((symbol-function 'select-window)
                 (lambda (w &optional _) (push (cons 'select w) log))))
        (with-current-buffer fake-mb (vertico-detour))))
    (expect (reverse log) :to-equal '(multiform-toggle exhibit (select . main-win))))

  (it "leaves the posframe display alone when it is not workable"
    (let ((vertico-posframe-mode t))
      (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () 'mb-win))
                ((symbol-function 'window-buffer) (lambda (&optional _) fake-mb))
                ((symbol-function 'posframe-workable-p) (lambda () nil))
                ((symbol-function 'vertico-multiform-posframe)
                 (lambda () (push 'multiform-toggle log)))
                ((symbol-function 'minibuffer-selected-window) (lambda () 'main-win))
                ((symbol-function 'select-window)
                 (lambda (w &optional _) (push (cons 'select w) log))))
        (with-current-buffer fake-mb (vertico-detour))))
    (expect (reverse log) :to-equal '((select . main-win))))

  (it "drops briefly-tall before leaving a posframe session"
    (let ((vertico-posframe-mode t)
          (vertico-posframe-tall-mode t)
          (vertico-posframe-height 40)
          (vertico-count 75))
      (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () 'mb-win))
                ((symbol-function 'window-buffer) (lambda (&optional _) fake-mb))
                ((symbol-function 'posframe-workable-p) (lambda () t))
                ((symbol-function 'vertico-multiform-posframe)
                 (lambda () (push 'multiform-toggle log)))
                ((symbol-function 'vertico--exhibit)
                 (lambda () (push 'exhibit log)))
                ((symbol-function 'minibuffer-selected-window) (lambda () 'main-win))
                ((symbol-function 'select-window)
                 (lambda (w &optional _) (push (cons 'select w) log))))
        (with-current-buffer fake-mb (vertico-detour))
        (expect vertico-posframe-tall-mode :to-be nil)
        (expect vertico-posframe-height :to-be nil)
        (expect vertico-count :to-equal 15))))

  (it "jumps back into the pending session from outside the minibuffer"
    (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () 'mb-win))
              ((symbol-function 'window-buffer) (lambda (&optional _) fake-mb))
              ((symbol-function 'select-window)
               (lambda (w &optional _) (push (cons 'select w) log))))
      (with-temp-buffer (vertico-detour)))
    (expect (reverse log) :to-equal '((select . mb-win)))))
