;;; tests/helper.el --- shared buttercup bootstrap -*- lexical-binding: t; -*-

;; Sandbox the XDG dirs BEFORE doom-compat derives its paths from them:
;; tests must never touch the real ~/.cache/emacs-lab or ~/.local/share.
(defvar test-sandbox-dir
  (file-name-as-directory (make-temp-file "emacs-lab-tests" t)))

(setenv "XDG_DATA_HOME" (concat test-sandbox-dir "data"))
(setenv "XDG_STATE_HOME" (concat test-sandbox-dir "state"))
(setenv "XDG_CACHE_HOME" (concat test-sandbox-dir "cache"))

(defvar test-config-root
  (expand-file-name "../" (file-name-directory (or load-file-name buffer-file-name)))
  "Root of the config being tested, derived from this file's location.")

(add-to-list 'load-path (expand-file-name "lisp/" test-config-root))
(require 'doom-compat)

(defun load-module-file (relpath)
  "Load RELPATH relative to the config root, without load-path pollution."
  (load (expand-file-name relpath test-config-root) nil 'nomessage))

;; On Emacs 31 `featurep' no longer reads a let-bindable `features', so
;; faking a loaded feature takes a function stub.
(defmacro with-fake-feature (feature &rest body)
  "Run BODY with `featurep' (and thus `require') treating FEATURE as loaded."
  (declare (indent 1))
  `(cl-letf* ((real-featurep (symbol-function 'featurep))
              (real-require (symbol-function 'require))
              ((symbol-function 'featurep)
               (lambda (f &optional subfeature)
                 (or (eq f ,feature) (funcall real-featurep f subfeature))))
              ((symbol-function 'require)
               (lambda (f &optional filename noerror)
                 (unless (eq f ,feature)
                   (funcall real-require f filename noerror)))))
     ,@body))

(defun transient-layout-commands (node)
  "Collect suffix command symbols from a parsed transient layout NODE.
Walks both layout dialects: suffixes as (CLASS :command CMD ...) with a
flat plist in the cdr (transient >= 0.8, Emacs 31) and as (LEVEL CLASS
(PLIST)) with a nested plist (0.7.x, Emacs 30's bundled copy - what CI
runs)."
  (cond
   ((vectorp node)
    (mapcan #'transient-layout-commands (append node nil)))
   ((proper-list-p node)
    (if-let* ((cmd (if (keywordp (car node))
                       (plist-get node :command)
                     (plist-get (cdr node) :command))))
        (list cmd)
      ;; keyword car = a plist without :command; don't descend into values
      (unless (keywordp (car node))
        (mapcan #'transient-layout-commands node))))))

(provide 'test-helper)
