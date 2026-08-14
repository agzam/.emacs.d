;;; tests/e2e/improve-text-variants.el --- variants pick flow -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; The batch suite calls the picker commands directly; what only shows
;; up here is the wiring: the picker's digits and RET travel through
;; the `keymap' text property, which must outrank evil's normal-state
;; map (where digits are prefix arguments), and the review handoff
;; opens a real smerge-transient whose suffixes drive resolution.

(require 'cl-lib)

(defun improve-text-variants-e2e ()
  "Drive the variants prompt: request, pick by digit, resolve the review."
  (require 'gptel)
  ;; the improve-text machinery lives in a lazily-loaded autoload file;
  ;; force it so the prompt list is available before the command runs
  (unless (boundp 'gptel-improve-text-prompts-history)
    (load (expand-file-name "modules/ai/autoload/gptel.el" e2e-root)
          nil 'nomessage))
  (let* ((file (expand-file-name "variants-origin.txt" e2e-work-dir))
         (origin (find-file-noselect file))
         (canned (concat "Aa good start. Bb stays fine.\n\n---\n\n"
                         "Aa better start. Bb stays fine.\n\n---\n\n"
                         "Aa best start. Bb stays fine."))
         (results '())
         sent-context expected-end picker)
    (cl-flet ((record (label ok got want)
                (push (list :label (format "improve-text variants: %s" label)
                            :ok ok :got got :want want)
                      results)
                ok))
      (unwind-protect
          (catch 'stop
            (with-current-buffer origin
              (switch-to-buffer origin)
              (delete-other-windows)
              (erase-buffer)
              (insert "Aa bad start. Bb stays fine.")
              (font-lock-ensure)
              (setq expected-end (point-max))
              (cl-letf (((symbol-function 'gptel-request)
                         (cl-function
                          (lambda (_prompt &key context callback
                                           &allow-other-keys)
                            (setq sent-context context)
                            (funcall callback canned
                                     (list :context context))))))
                (let ((gptel-improve-text-prompt
                       (nth 2 gptel-improve-text-prompts-history)))
                  (push-mark (point-min) t t)
                  (goto-char (point-max))
                  (gptel-improve-text))))
            (unless (record
                     "request captures the origin region as markers"
                     (pcase sent-context
                       (`(:improve-region (,b . ,e))
                        (and (markerp b) (markerp e)
                             (eq (marker-buffer b) origin)
                             (= (marker-position b) 1)
                             (= (marker-position e) expected-end)))
                       (_ nil))
                     (format "%S" sent-context)
                     (format "(:improve-region (#<marker 1> . #<marker %d>)) into origin"
                             expected-end))
              (throw 'stop nil))
            (setq picker (window-buffer (selected-window)))
            (unless (record
                     "picker window selected with legend and keymap property"
                     (and (buffer-live-p picker)
                          (string-match-p "improve-text variants"
                                          (buffer-name picker))
                          (with-current-buffer picker
                            (and (= 3 (length gptel-improve-text-variants))
                                 (stringp header-line-format)
                                 (eq (get-text-property (point-min) 'keymap)
                                     gptel-improve-text-variants-map))))
                     (format "buffer=%s" (buffer-name picker))
                     "selected *improve-text variants* with 3 variants, legend, keymap")
              (throw 'stop nil))
            ;; the keypress that must beat evil's digit-argument
            (execute-kbd-macro (kbd "2"))
            (let ((got (with-current-buffer origin
                         (buffer-substring-no-properties (point-min) (point-max))))
                  (want (concat "<<<<<<< original\nAa bad start. \n=======\n"
                                "Aa better start. \n>>>>>>> variant 2\n"
                                "Bb stays fine.")))
              (unless (record
                       "digit 2 lands sentence conflicts and opens the panel"
                       (and (equal got want)
                            (not (buffer-live-p picker))
                            (buffer-local-value 'smerge-mode origin)
                            (eq (and (bound-and-true-p transient--prefix)
                                     (oref transient--prefix command))
                                'smerge-transient))
                       (format "text=%S smerge=%s transient=%s picker-live=%s"
                               got (buffer-local-value 'smerge-mode origin)
                               (and (bound-and-true-p transient--prefix)
                                    (oref transient--prefix command))
                               (buffer-live-p picker))
                       (format "text=%S smerge=t transient=smerge-transient picker-live=nil"
                               want))
                (throw 'stop nil)))
            (execute-kbd-macro (kbd "l"))
            (let ((got (with-current-buffer origin
                         (buffer-substring-no-properties (point-min) (point-max))))
                  (want "Aa better start. Bb stays fine.")
                  (last-region
                   (with-current-buffer origin
                     (when (and gptel-improve-text-last-region
                                (marker-position
                                 (car gptel-improve-text-last-region)))
                       (buffer-substring-no-properties
                        (car gptel-improve-text-last-region)
                        (cdr gptel-improve-text-last-region))))))
              (unless (record
                       "panel's keep-lower completes the review and rejoins"
                       (and (equal got want)
                            (equal last-region want)
                            (not (buffer-local-value 'smerge-mode origin))
                            (null (buffer-local-value 'header-line-format origin)))
                       (format "text=%S last-region=%S smerge=%s"
                               got last-region
                               (buffer-local-value 'smerge-mode origin))
                       (format "text=%S last-region=%S smerge=nil" want want))
                (throw 'stop nil)))
            (execute-kbd-macro (kbd "q"))
            (record "q dismisses the panel"
                    (null (bound-and-true-p transient--prefix))
                    (format "transient--prefix=%S"
                            (bound-and-true-p transient--prefix))
                    "transient--prefix=nil"))
        ;; a failed step may strand the transient; never leak it into
        ;; the scenarios that run after this one
        (when (bound-and-true-p transient--prefix)
          (ignore-errors
            (if (fboundp 'transient--emergency-exit)
                (transient--emergency-exit)
              (transient-quit-all))))
        (dolist (b (buffer-list))
          (when (string-match-p "improve-text variants" (buffer-name b))
            (kill-buffer b)))
        (when (buffer-live-p origin)
          (with-current-buffer origin (set-buffer-modified-p nil))
          (kill-buffer origin))))
    (nreverse results)))

(add-to-list 'e2e-scenarios #'improve-text-variants-e2e)
