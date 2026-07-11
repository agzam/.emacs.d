;;; tests/colors/cycle-themes-tests.el --- colors/autoload/cycle-themes.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; circadian isn't installed in the batch tier; the file only needs its
;; variable.  Theme activation itself is smoke/probe territory.
(provide 'circadian)
(defvar circadian-themes nil)

(load-module-file "modules/colors/autoload/cycle-themes.el")

(describe "cycle-themes-ring-init"
  (it "builds the ring from circadian-themes cdrs, in schedule order"
    (let ((cycle-themes-ring nil)
          (circadian-themes '(("6:00" . theme-a)
                              ("19:00" . theme-b)
                              ("20:00" . theme-c))))
      (expect (ring-elements (cycle-themes-ring-init))
              :to-equal '(theme-a theme-b theme-c))))

  (it "reuses the ring once built"
    (let ((cycle-themes-ring nil)
          (circadian-themes '(("6:00" . theme-a))))
      (cycle-themes-ring-init)
      (let ((circadian-themes '(("6:00" . theme-z))))
        (expect (ring-elements (cycle-themes-ring-init))
                :to-equal '(theme-a))))))

(describe "load-cycled-theme"
  (it "loads the ring successor and disables the current theme"
    (let ((cycle-themes-ring nil)
          (circadian-themes '(("6" . theme-a) ("19" . theme-b) ("20" . theme-c)))
          (custom-enabled-themes '(theme-a))
          loaded disabled)
      (cl-letf (((symbol-function 'load-theme)
                 (lambda (theme &rest _) (push theme loaded)))
                ((symbol-function 'disable-theme)
                 (lambda (theme) (push theme disabled))))
        (expect (load-cycled-theme 'next) :to-be 'theme-b)
        (expect loaded :to-equal '(theme-b))
        (expect disabled :to-equal '(theme-a)))))

  (it "wraps backwards from the ring head"
    (let ((cycle-themes-ring nil)
          (circadian-themes '(("6" . theme-a) ("19" . theme-b) ("20" . theme-c)))
          (custom-enabled-themes '(theme-a)))
      (cl-letf (((symbol-function 'load-theme) #'ignore)
                ((symbol-function 'disable-theme) #'ignore))
        (expect (load-cycled-theme 'prev) :to-be 'theme-c))))

  (it "round-trips: next then prev restores the starting theme"
    (let ((cycle-themes-ring nil)
          (circadian-themes '(("6" . theme-a) ("19" . theme-b) ("20" . theme-c)))
          (custom-enabled-themes '(theme-b)))
      (cl-letf (((symbol-function 'load-theme)
                 (lambda (theme &rest _)
                   (setq custom-enabled-themes (cons theme custom-enabled-themes))))
                ((symbol-function 'disable-theme)
                 (lambda (theme)
                   (setq custom-enabled-themes (delq theme custom-enabled-themes)))))
        (load-cycled-theme 'next)
        (load-cycled-theme 'prev)
        (expect custom-enabled-themes :to-equal '(theme-b)))))

  (it "inserts an off-ring current theme and cycles from it"
    (let ((cycle-themes-ring nil)
          (circadian-themes '(("6" . theme-a) ("19" . theme-b)))
          (custom-enabled-themes '(alien-theme)))
      (cl-letf (((symbol-function 'load-theme) #'ignore)
                ((symbol-function 'disable-theme) #'ignore))
        (expect (load-cycled-theme 'next) :to-be 'theme-a)
        (expect (ring-member cycle-themes-ring 'alien-theme) :to-be-truthy))))

  (it "cycles into the ring when no theme is enabled"
    (let ((cycle-themes-ring nil)
          (circadian-themes '(("6" . theme-a) ("19" . theme-b)))
          (custom-enabled-themes nil)
          disabled)
      (cl-letf (((symbol-function 'load-theme) #'ignore)
                ((symbol-function 'disable-theme)
                 (lambda (theme) (push theme disabled))))
        (expect (load-cycled-theme 'next) :to-be 'theme-a)
        (expect disabled :to-be nil)))))

(describe "cycle-themes transient wrappers"
  (it "cycle-themes-down opens the transient, then advances"
    (let (calls)
      (cl-letf (((symbol-function 'cycle-themes)
                 (lambda () (push 'transient calls)))
                ((symbol-function 'load-next-theme)
                 (lambda () (push 'next calls))))
        (cycle-themes-down)
        (expect (reverse calls) :to-equal '(transient next)))))

  (it "cycle-themes-up opens the transient, then steps back"
    (let (calls)
      (cl-letf (((symbol-function 'cycle-themes)
                 (lambda () (push 'transient calls)))
                ((symbol-function 'load-prev-theme)
                 (lambda () (push 'prev calls))))
        (cycle-themes-up)
        (expect (reverse calls) :to-equal '(transient prev))))))
