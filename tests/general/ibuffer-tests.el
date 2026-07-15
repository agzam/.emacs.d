;;; tests/general/ibuffer-tests.el --- general/config.el ibuffer specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
;; define-ibuffer-filter and ibuffer-filtering-alist live here, not in ibuffer.
(require 'ibuf-ext)

(defun ibuffer-tests--config-form (marker)
  "Read the top-level modules/general/config.el form whose body holds MARKER."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "modules/general/config.el" test-config-root))
    (emacs-lisp-mode)
    (goto-char (point-min))
    (search-forward marker)
    (while (ignore-errors (backward-up-list) t))
    (read (current-buffer))))

(defvar ibuffer-tests--form
  (ibuffer-tests--config-form "(define-ibuffer-filter unsaved-file-buffers")
  "The real (after! ...) form defining the custom ibuffer filters.")

(describe "custom ibuffer filters"
  (it "gate on ibuffer and pull in ibuf-ext themselves"
    ;; The regression: gating on ibuf-ext (which nothing loads on the main
    ;; ibuffer path) left the filters undefined.  They must gate on ibuffer
    ;; and require ibuf-ext, or they silently vanish again.
    (expect (car ibuffer-tests--form) :to-be 'after!)
    (expect (cadr ibuffer-tests--form) :to-be 'ibuffer)
    (expect (member '(require 'ibuf-ext) ibuffer-tests--form) :to-be-truthy))

  (describe "once the definitions run"
    (before-all
      (dolist (form (cddr ibuffer-tests--form))
        (when (eq (car-safe form) 'define-ibuffer-filter)
          (eval form t))))

    (it "defines the three filter commands"
      (expect (fboundp 'ibuffer-filter-by-unsaved-file-buffers) :to-be-truthy)
      (expect (fboundp 'ibuffer-filter-by-file-buffers) :to-be-truthy)
      (expect (fboundp 'ibuffer-filter-by-non-special-buffers) :to-be-truthy))

    (it "registers them in ibuffer-filtering-alist"
      (dolist (name '(unsaved-file-buffers file-buffers non-special-buffers))
        (expect (assq name ibuffer-filtering-alist) :to-be-truthy)))

    (it "unsaved filter matches only modified file-visiting buffers"
      (let ((pred (nth 2 (assq 'unsaved-file-buffers ibuffer-filtering-alist))))
        (with-temp-buffer
          (setq buffer-file-name "/tmp/ibuffer-unsaved-probe")
          (set-buffer-modified-p t)
          (expect (funcall pred (current-buffer) nil) :to-be-truthy)
          ;; saved (unmodified) file buffer no longer qualifies
          (set-buffer-modified-p nil)
          (expect (funcall pred (current-buffer) nil) :to-be nil)
          (setq buffer-file-name nil))
        ;; a modified buffer with no file never qualifies
        (with-temp-buffer
          (insert "not a file")
          (expect (funcall pred (current-buffer) nil) :to-be nil))))))
