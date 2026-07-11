;;; modules/elisp/autoload/profiler.el --- profiler toggle + report helpers -*- lexical-binding: t; -*-
;;; Commentary:
;; toggle-profiler is a renamed doom/toggle-profiler (lisp/lib/debug.el);
;; the report helpers moved here from misc.el.
;;; Code:

(defvar toggle-profiler--running nil)

;;;###autoload
(defun toggle-profiler ()
  "Toggle the Emacs profiler.  Run it again to see the profiling report."
  (interactive)
  (if (not toggle-profiler--running)
      (profiler-start 'cpu+mem)
    (profiler-report)
    (profiler-stop))
  (setq toggle-profiler--running (not toggle-profiler--running)))

;;;###autoload
(defun profiler-report-expand-all ()
  "Expand all entries in every profiler report buffer."
  (interactive)
  (thread-last
    (buffer-list)
    (seq-filter
     (lambda (b)
       (string-match-p
        "\\*\\(CPU\\|Memory\\)-Profiler-Report.*\\*"
        (buffer-name b))))
    (seq-do
     (lambda (b)
       (with-current-buffer b
         (goto-char (point-min))
         (while (not (eobp))
           (profiler-report-expand-entry)
           (profiler-report-next-entry))
         (goto-char (point-min)))))))

;;;###autoload
(defun profiler-report-helpful-symbol-at-point ()
  "Open the profiler entry at point in helpful."
  (interactive)
  (helpful-symbol (get-text-property (point) 'profiler-entry)))

;;; profiler.el ends here
