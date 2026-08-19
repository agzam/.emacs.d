;;; tests/colors/config-tests.el --- colors module specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; circadian isn't installed in the batch tier.  These reductions keep the two
;; shapes the module's advice corrects: `circadian-encode-time' builds an
;; absolute moment through the obsolescent `encode-time' call, and
;; `circadian-enable-theme' releases `circadian-next-timer' only on the branch
;; that changes the theme.
(provide 'circadian)
(defvar circadian-themes nil)
(defvar circadian-next-timer nil)

(defun circadian-encode-time (hour min)
  "Encode today's HOUR and MIN the way upstream circadian does."
  (let ((now (decode-time)))
    (encode-time 0 min hour (nth 3 now) (nth 4 now) (nth 5 now) nil -1 nil)))

(defun circadian-schedule ()
  "Arm the next switch, which upstream skips while a timer is on record."
  (when (null circadian-next-timer)
    (setq circadian-next-timer 'armed)))

(defun circadian-enable-theme (theme)
  "Enable THEME, releasing the spent timer on the changing branch alone."
  (unless (member theme custom-enabled-themes)
    (setq custom-enabled-themes (list theme))
    (when circadian-next-timer
      (cancel-timer circadian-next-timer))
    (setq circadian-next-timer nil))
  (circadian-schedule))

(defvar colors-tests--packages nil)

(defun colors-tests--expand-use-package (name &rest args)
  "Stub `use-package' expander: record NAME, run its :init/:config forms."
  `(progn (push ',name colors-tests--packages)
          ,@(apply #'use-package-body-forms args '(:init :config))))

(cl-letf (((symbol-function 'use-package)
           (cons 'macro #'colors-tests--expand-use-package)))
  (load-module-file "modules/colors/config.el"))

(describe "colors module config"
  (it "installs circadian and the theme packages it schedules"
    (dolist (pkg '(circadian ag-themes spacemacs-theme base16-theme doom-themes))
      (expect (memq pkg colors-tests--packages) :to-be-truthy)))

  (it "schedules a theme for every entry in the ring"
    (expect (length circadian-themes) :to-be-greater-than 1)
    (expect (seq-every-p #'symbolp (mapcar #'cdr circadian-themes)) :to-be-truthy)))

(describe "circadian-encode-time-as-time-value-a"
  (it "never answers a bare number, whichever form encode-time uses"
    (dolist (form '(nil t))
      (let ((current-time-list form))
        (expect (numberp (circadian-encode-time 19 0)) :to-be nil))))

  (it "gives run-at-time an absolute moment, not an offset from now"
    (let* ((current-time-list nil)
           (target (circadian-encode-time 19 0))
           (timer (run-at-time target nil #'ignore)))
      (unwind-protect
          (expect (format-time-string "%F %H:%M" (timer--time timer))
                  :to-equal (format-time-string "%F %H:%M" target))
        (cancel-timer timer))))

  (it "encodes the hour and minute it was asked for"
    (let ((current-time-list nil))
      (expect (format-time-string "%H:%M" (circadian-encode-time 21 30))
              :to-equal "21:30"))))

(describe "circadian-clear-fired-timer-a"
  (it "arms the following switch when the due theme is already enabled"
    (let ((custom-enabled-themes '(theme-a))
          (circadian-next-timer 'spent))
      (cl-letf (((symbol-function 'cancel-timer) #'ignore))
        (circadian-enable-theme 'theme-a))
      (expect circadian-next-timer :to-be 'armed)))

  (it "still arms the following switch when the theme changes"
    (let ((custom-enabled-themes '(theme-a))
          (circadian-next-timer 'spent))
      (cl-letf (((symbol-function 'cancel-timer) #'ignore))
        (circadian-enable-theme 'theme-b))
      (expect circadian-next-timer :to-be 'armed)
      (expect custom-enabled-themes :to-equal '(theme-b))))

  (it "cancels the spent timer rather than leaking it"
    (let ((custom-enabled-themes '(theme-a))
          (circadian-next-timer 'spent)
          cancelled)
      (cl-letf (((symbol-function 'cancel-timer)
                 (lambda (timer) (push timer cancelled))))
        (circadian-enable-theme 'theme-a))
      (expect cancelled :to-equal '(spent)))))
