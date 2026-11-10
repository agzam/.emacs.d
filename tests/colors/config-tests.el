;;; tests/colors/config-tests.el --- colors module specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; circadian isn't installed in the batch tier.  These reductions keep the
;; upstream shapes the module's advice corrects: the obsolescent
;; `encode-time' call, the timer released on one branch only, and sun times
;; that take no offset.
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

(defvar colors-tests--sunset '(17 31))

(defun circadian-match-sun (input)
  "Answer a fixed sunrise and sunset for INPUT, else parse it as HH:MM."
  (pcase input
    (:sunrise '(7 5))
    (:sunset colors-tests--sunset)
    (_ (mapcar #'string-to-number (split-string input ":")))))

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

(describe "circadian-sun-offset-a"
  (it "shifts sunrise and sunset by the given minutes"
    (expect (circadian-match-sun '(:sunset -110)) :to-equal '(15 41))
    (expect (circadian-match-sun '(:sunrise 30)) :to-equal '(7 35)))

  (it "wraps the shifted time around midnight"
    (expect (circadian-match-sun '(:sunset 420)) :to-equal '(0 31))
    (expect (circadian-match-sun '(:sunrise -480)) :to-equal '(23 5)))

  (it "leaves fixed times and bare sun keys to circadian"
    (expect (circadian-match-sun "6:00") :to-equal '(6 0))
    (expect (circadian-match-sun :sunset) :to-equal '(17 31)))

  (it "reads every time in the schedule as an hour and minute of the day"
    (dolist (entry circadian-themes)
      (pcase-let ((`(,hour ,minute) (circadian-match-sun (car entry))))
        (expect (and (<= 0 hour 23) (<= 0 minute 59)) :to-be-truthy))))

  (it "goes around the clock once in written order at the earliest and latest sunset"
    (dolist (colors-tests--sunset '((17 18) (20 40)))
      (let* ((minutes (mapcar (lambda (entry)
                                (pcase-let ((`(,hour ,minute)
                                             (circadian-match-sun (car entry))))
                                  (+ (* 60 hour) minute)))
                              circadian-themes))
             (gaps (seq-mapn (lambda (from to) (mod (- to from) 1440))
                             minutes
                             (append (cdr minutes) (list (car minutes))))))
        (expect (seq-every-p (lambda (gap) (< 0 gap)) gaps) :to-be-truthy)
        (expect (apply #'+ gaps) :to-equal 1440)))))

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

(describe "enable-theme-parents-first-a"
  (it "moves a face after the face it inherits from"
    (put 'colors-tests-order 'theme-settings
         '((theme-face colors-tests-b colors-tests-order ((t :inherit colors-tests-a)))
           (theme-face colors-tests-a colors-tests-order ((t :inherit bold)))))
    (enable-theme-parents-first-a 'colors-tests-order)
    (expect (mapcar #'cadr (get 'colors-tests-order 'theme-settings))
            :to-equal '(colors-tests-a colors-tests-b))))

(describe "enable-theme-all-or-nothing-a"
  (it "drops the specs a theme applied before failing partway"
    (custom-declare-theme 'colors-tests-broken 'colors-tests-broken-theme)
    (put 'colors-tests-broken 'theme-settings
         '((theme-face colors-tests-b colors-tests-broken ((t :foreground "red")))
           (theme-face colors-tests-a colors-tests-broken ((t :foreground "blue")))))
    (cl-letf (((symbol-function 'custom-theme-recalc-face)
               (lambda (face) (when (eq face 'colors-tests-a) (error "Face fails")))))
      (expect (enable-theme 'colors-tests-broken) :to-throw 'error))
    (expect (get 'colors-tests-b 'theme-face) :to-be nil)
    (expect (custom-theme-enabled-p 'colors-tests-broken) :to-be nil))

  (it "never undoes the user theme"
    (custom-theme-set-faces 'user '(colors-tests-c ((t :foreground "green"))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'custom-theme-recalc-face)
                     (lambda (_) (error "Face fails"))))
            (expect (enable-theme 'user) :to-throw 'error))
          (expect (assq 'user (get 'colors-tests-c 'theme-face)) :to-be-truthy))
      (custom-push-theme 'theme-face 'colors-tests-c 'user 'reset)
      (put 'colors-tests-c 'saved-face nil))))
