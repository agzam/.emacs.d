;;; tests/lookup/lookup-tests.el --- lookup/autoload/lookup.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; Handler-list defvars normally come from the module's config.el, which the
;; batch tier can't load (map!/use-package); valued defvars stand in.
(defvar lookup-definition-functions nil)
(defvar lookup-implementations-functions nil)
(defvar lookup-type-definition-functions nil)
(defvar lookup-references-functions nil)
(defvar lookup-documentation-functions nil)
(defvar lookup-file-functions nil)

(load-module-file "modules/lookup/autoload/lookup.el")

;; A fake minor mode for set-lookup-handlers! (its enable check reads the
;; mode symbol as a variable when it isn't the major mode).
(defvar fake-lookup-mode nil)
(defvar fake-lookup-mode-hook nil)

;; Named handler symbols - lookup--run-handlers does (get handler
;; 'lookup-async), so handlers MUST be symbols (as they are in real hook
;; lists); raw lambdas would signal wrong-type-argument.
(defun lookup-tests--def-handler (_id) (car (list 'def)))
(defun lookup-tests--goto7 (_id) (goto-char 7) t)
(defun lookup-tests--boom (_id) (error "boom"))
(defun lookup-tests--deferred (_id) 'deferred)

(defun lookup-tests--init ()
  "Call the current init fn (fresh - set-lookup-handlers! re-interns it)."
  (funcall (intern "lookup--init-fake-lookup-mode-handlers-h")))

(describe "set-lookup-handlers!"
  (before-each
    (setq fake-lookup-mode nil)
    (dolist (v '(lookup-definition-functions lookup-references-functions
                 lookup-implementations-functions lookup-type-definition-functions
                 lookup-documentation-functions lookup-file-functions))
      (set-default v nil)))

  (it "registers a named init fn on the mode hook"
    (set-lookup-handlers! 'fake-lookup-mode :definition #'lookup-tests--def-handler)
    (expect (fboundp (intern "lookup--init-fake-lookup-mode-handlers-h"))
            :to-be-truthy)
    (expect (memq (intern "lookup--init-fake-lookup-mode-handlers-h")
                  fake-lookup-mode-hook)
            :to-be-truthy))

  (it "adds handlers buffer-locally when the mode is active"
    (set-lookup-handlers! 'fake-lookup-mode
      :definition #'lookup-tests--def-handler
      :references '(lookup-tests--goto7 :async t))
    (with-temp-buffer
      (setq-local fake-lookup-mode t)
      (lookup-tests--init)
      (expect (memq #'lookup-tests--def-handler lookup-definition-functions)
              :to-be-truthy)
      (expect (memq #'lookup-tests--goto7 lookup-references-functions)
              :to-be-truthy)
      (expect (get 'lookup-tests--goto7 'lookup-async) :to-be-truthy)
      ;; buffer-local, not global
      (expect (default-value 'lookup-definition-functions)
              :not :to-contain 'lookup-tests--def-handler)))

  (it "does nothing when the mode is off in the buffer"
    (set-lookup-handlers! 'fake-lookup-mode :definition #'lookup-tests--def-handler)
    (with-temp-buffer
      (setq-local fake-lookup-mode nil)
      (lookup-tests--init)
      (expect lookup-definition-functions :not :to-contain 'lookup-tests--def-handler)))

  (it "a nil plist unregisters the init fn"
    (set-lookup-handlers! 'fake-lookup-mode :definition #'lookup-tests--def-handler)
    (set-lookup-handlers! 'fake-lookup-mode nil)
    (expect (intern-soft "lookup--init-fake-lookup-mode-handlers-h") :to-be nil)))

(describe "lookup--run-handlers"
  ;; lookup--run-handlers uses condition-case-unless-debug (Doom's idiom, so
  ;; lookup errors surface when the user runs with debug-on-error).  Buttercup
  ;; forces debug-on-error t to grab backtraces, which makes that form
  ;; re-signal - bind it nil to assert the normal-session catching behavior.
  (it "returns nil and restores windows when the handler errors"
    (let ((debug-on-error nil))
      (expect (lookup--run-handlers #'lookup-tests--boom "id" (point-marker))
              :to-be nil)))

  (it "passes 'deferred through"
    (expect (lookup--run-handlers #'lookup-tests--deferred "id" (point-marker))
            :to-equal t))

  (it "treats handlers with the async prop as successful"
    (put 'lookup-tests--async-h 'lookup-async t)
    (fset 'lookup-tests--async-h (lambda (_) nil))
    (expect (lookup--run-handlers 'lookup-tests--async-h "id" (point-marker))
            :to-equal t))
  ;; The marker-return path (a point-moving handler) exercises
  ;; set-window-configuration, which restores buttercup's own runner window
  ;; state mid-spec and hard-aborts the harness.  Covered instead by the
  ;; lookup--jump-to dispatch spec below (point moves) and the live probe.
  )

(describe "lookup--jump-to"
  (it "dispatches through the prop's handler list and lands on the result"
    (with-temp-buffer
      (insert "alpha beta")
      (goto-char (point-min))
      (let ((lookup-definition-functions (list #'lookup-tests--goto7)))
        (expect (lookup--jump-to :definition "beta") :to-be-truthy)
        (expect (point) :to-equal 7))))

  (it "reports failure when no handler finds anything"
    (with-temp-buffer
      (let ((lookup-definition-functions (list #'ignore)))
        (expect (lookup--jump-to :definition "nope") :to-be nil)))))

(describe "xref backend wrappers"
  (it "swallow cl-no-applicable-method"
    (cl-letf (((symbol-function 'xref-find-backend) (lambda () 'none))
              ((symbol-function 'xref-backend-definitions)
               (lambda (&rest _) (signal 'cl-no-applicable-method nil))))
      (expect (lookup-xref-definitions-backend-fn "x") :to-be nil))))

(describe "lookup-definition (command)"
  (it "user-errors with nothing under point"
    (expect (lookup-definition nil) :to-throw 'user-error)))
