;;; tests/completion/config-tests.el --- modules/completion/config.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)
(require 'thingatpt)

(defun completion-config-tests--top-level-forms ()
  "Read every top-level form out of the completion module's config, in file order."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "modules/completion/config.el" test-config-root))
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(defun completion-config-tests--settings (package)
  "Plist of everything PACKAGE's `use-package' form sets under `:config'."
  (let ((block (cl-find-if (lambda (f)
                             (and (eq (car-safe f) 'use-package)
                                  (eq (cadr f) package)))
                           (completion-config-tests--top-level-forms)))
        settings)
    (dolist (form (use-package-body-forms (cddr block) :config) settings)
      (when (eq (car-safe form) 'setopt)
        (setq settings (append settings (cdr form)))))))

(defun completion-config-tests--consult-settings ()
  "Plist of everything the consult `use-package' form sets under `:config'."
  (completion-config-tests--settings 'consult))

(describe "consult async delays"
  ;; The delays gate how soon rg/fd spawn and how often results reach the UI.
  ;; Consult ships 0.2/0.5/0.2, which idles far longer than the search costs.
  :var* ((settings (completion-config-tests--consult-settings))
         (delay (lambda (var) (plist-get settings var))))

  (dolist (var '(consult-async-input-debounce
                 consult-async-input-throttle
                 consult-async-refresh-delay))
    (it (format "sets %s under the shipped default, and above zero" var)
      (let ((value (funcall delay var)))
        (expect value :to-be-truthy)
        (expect (numberp value) :to-be t)
        ;; zero refresh delay redisplays on every arriving chunk
        (expect value :to-be-greater-than 0)
        (expect value :to-be-less-than 0.2))))

  (it "debounces no longer than it throttles, so the throttle still caps spawns"
    (expect (funcall delay 'consult-async-input-debounce)
            :to-be-less-than (funcall delay 'consult-async-input-throttle))))

(describe "vertico-buffer display"
  ;; the configured action runs against real windows, so a wrong direction
  ;; or a reused window fails here, not only in a booted config
  (it "opens a new full-height window at the frame's left edge"
    (save-window-excursion
      (delete-other-windows)
      (let* ((right (split-window-right))
             (action (eval (plist-get (completion-config-tests--settings 'vertico-buffer)
                                      'vertico-buffer-display-action)
                           t))
             (buffer (get-buffer-create " *vertico-buffer-probe*"))
             (window (with-selected-window right (display-buffer buffer action))))
        (unwind-protect
            (progn
              (expect (window-left-column window) :to-equal 0)
              (expect (window-full-height-p window) :to-be t)
              (expect (length (window-list nil 'no)) :to-equal 3))
          (kill-buffer buffer))))))

(describe "yasnippet-capf search distance"
  ;; completion-preview calls the capf on every keystroke; unbounded, its
  ;; symbol lookup scans from point-min, so deep in a big file typing lags.
  :var* ((distance (plist-get (completion-config-tests--settings 'yasnippet-capf)
                              'yasnippet-capf-max-search-distance)))

  (it "is bounded"
    (expect (natnump distance) :to-be t)
    (expect distance :to-be-greater-than 0))

  (it "still finds the whole symbol at point deep in a big buffer"
    (with-temp-buffer
      (emacs-lisp-mode)
      (dotimes (_ 20000) (insert "filler-word "))
      (insert "(some-snippet-key")
      (expect (thing-at-point-looking-at "\\(?:\\sw\\|\\s_\\)+" distance)
              :to-be-truthy)
      (expect (match-string 0) :to-equal "some-snippet-key"))))

;;; config-tests.el ends here
