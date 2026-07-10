;;; tests/general/frames-tests.el --- frames lib specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/general/autoload/frames.el")

;; the dual-dialect layout walker lives in tests/helper.el (shared with
;; expreg-tests); its dialect specs stay here where it was born
(describe "transient-layout-commands"
  (it "walks the flat dialect (transient >= 0.8)"
    (expect (transient-layout-commands
             '([transient-column (:description "d")
                ((transient-suffix :key "j" :command cmd-a)
                 (transient-suffix :key "k" :command cmd-b))]))
            :to-equal '(cmd-a cmd-b)))
  (it "walks the nested-plist dialect (transient 0.7, Emacs 30)"
    (expect (transient-layout-commands
             '([1 transient-column nil
                ((1 transient-suffix (:key "j" :command cmd-a))
                 (1 transient-suffix (:key "k" :command cmd-b)))]))
            :to-equal '(cmd-a cmd-b))))

(describe "font-size-increment"
  (it "stays a whole-point multiple - sub-point steps render as no-ops on mac"
    (expect (mod font-size-increment 10) :to-equal 0)))

(describe "font-size-increase"
  (before-each (setq font-size--initial-height nil))
  (after-each (setq font-size--initial-height nil))

  (it "bumps the default face height by the increment"
    (let ((height 160) (sets nil))
      (cl-letf (((symbol-function 'face-attribute) (lambda (&rest _) height))
                ((symbol-function 'set-face-attribute)
                 (lambda (_f _fr _p val) (push val sets) (setq height val))))
        (font-size-increase)
        (expect sets :to-equal (list (+ 160 font-size-increment))))))

  (it "captures the pre-adjustment height only once"
    (let ((height 160))
      (cl-letf (((symbol-function 'face-attribute) (lambda (&rest _) height))
                ((symbol-function 'set-face-attribute)
                 (lambda (_f _fr _p val) (setq height val))))
        (font-size-increase)
        (font-size-increase)
        (expect font-size--initial-height :to-equal 160))))

  (it "never drops the height below 10"
    (let ((height 12) (sets nil))
      (cl-letf (((symbol-function 'face-attribute) (lambda (&rest _) height))
                ((symbol-function 'set-face-attribute)
                 (lambda (_f _fr _p val) (push val sets) (setq height val))))
        (font-size-increase -5)
        (expect sets :to-equal '(10))))))

(describe "font-size-decrease"
  (before-each (setq font-size--initial-height nil))
  (after-each (setq font-size--initial-height nil))

  (it "steps the height down by the increment"
    (let ((height 160) (sets nil))
      (cl-letf (((symbol-function 'face-attribute) (lambda (&rest _) height))
                ((symbol-function 'set-face-attribute)
                 (lambda (_f _fr _p val) (push val sets) (setq height val))))
        (font-size-decrease)
        (expect sets :to-equal (list (- 160 font-size-increment)))))))

(describe "font-size-reset"
  (after-each (setq font-size--initial-height nil))

  (it "restores the captured height"
    (let ((height 180) (sets nil))
      (setq font-size--initial-height 160)
      (cl-letf (((symbol-function 'face-attribute) (lambda (&rest _) height))
                ((symbol-function 'set-face-attribute)
                 (lambda (_f _fr _p val) (push val sets) (setq height val))))
        (font-size-reset)
        (expect sets :to-equal '(160)))))

  (it "no-ops before any adjustment"
    (let ((sets nil))
      (setq font-size--initial-height nil)
      (cl-letf (((symbol-function 'set-face-attribute)
                 (lambda (_f _fr _p val) (push val sets))))
        (font-size-reset)
        (expect sets :to-be nil)))))

(describe "text-scale-reset"
  (it "zeroes the buffer's text scale"
    (with-temp-buffer
      (text-scale-set 2)
      (expect (bound-and-true-p text-scale-mode) :to-be-truthy)
      (text-scale-reset)
      (expect (bound-and-true-p text-scale-mode) :to-be nil)
      (expect text-scale-mode-amount :to-equal 0))))

(describe "toggle-frame-full-height"
  (it "undecorates and stretches to the workarea height"
    (let ((params nil) (positions nil) (heights nil))
      (cl-letf (((symbol-function 'frame-parameter)
                 (lambda (_fr key) (alist-get key params)))
                ((symbol-function 'set-frame-parameter)
                 (lambda (_fr key val) (setf (alist-get key params) val)))
                ((symbol-function 'frame-position) (lambda (&optional _) '(100 . 50)))
                ((symbol-function 'set-frame-position)
                 (lambda (_fr x y) (push (list x y) positions)))
                ((symbol-function 'set-frame-height)
                 (lambda (_fr h &rest _) (push h heights)))
                ((symbol-function 'display-current-workarea)
                 (lambda () '(0 25 1920 1055)))
                ((symbol-function 'tab-bar-height) (lambda (&rest _) 30))
                ((symbol-function 'reset-ns-autohide-menu-bar) #'ignore)
                ((symbol-function 'redraw-display) #'ignore))
        (toggle-frame-full-height)
        (expect (alist-get 'undecorated params) :to-be t)
        (expect (alist-get 'undecorated-fullheight params) :to-be t)
        ;; x kept, y snapped to the workarea top (not a hardcoded offset)
        (expect positions :to-equal '((100 25)))
        ;; workarea height minus the tab bar
        (expect heights :to-equal '(1025)))))

  (it "restores decorations on the second toggle"
    (let ((params '((undecorated-fullheight . t) (undecorated . t)))
          (positions nil) (heights nil))
      (cl-letf (((symbol-function 'frame-parameter)
                 (lambda (_fr key) (alist-get key params)))
                ((symbol-function 'set-frame-parameter)
                 (lambda (_fr key val) (setf (alist-get key params) val)))
                ((symbol-function 'frame-position) (lambda (&optional _) '(100 . 50)))
                ((symbol-function 'set-frame-position)
                 (lambda (_fr x y) (push (list x y) positions)))
                ((symbol-function 'set-frame-height)
                 (lambda (_fr h &rest _) (push h heights)))
                ((symbol-function 'reset-ns-autohide-menu-bar) #'ignore)
                ((symbol-function 'redraw-display) #'ignore))
        (toggle-frame-full-height)
        (expect (alist-get 'undecorated params) :to-be nil)
        (expect (alist-get 'undecorated-fullheight params) :to-be nil)
        (expect (alist-get 'fullscreen params) :to-be nil)
        (expect positions :to-be nil)
        (expect heights :to-be nil)))))

(describe "frame-zoom-transient"
  (it "references only defined commands in its layout"
    (let ((cmds (transient-layout-commands
                 (get 'frame-zoom-transient 'transient--layout))))
      (expect cmds :not :to-be nil)
      (expect (seq-remove #'fboundp cmds) :to-equal nil)))

  (it "covers exactly the font, text-scale and full-height commands"
    (expect (transient-layout-commands
             (get 'frame-zoom-transient 'transient--layout))
            :to-have-same-items-as
            '(font-size-decrease font-size-increase font-size-reset
              text-scale-decrease text-scale-increase text-scale-reset
              toggle-frame-full-height))))
