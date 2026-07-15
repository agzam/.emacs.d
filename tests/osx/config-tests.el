;;; tests/osx/config-tests.el --- osx module specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; The mac/ns C vars only exist in mac builds, locate-command lives in
;; unloaded locate.el - declare them special so the dynamic lets below
;; scope the module's setqs on linux CI too.
(defvar mac-right-option-modifier)
(defvar ns-pop-up-frames)
(defvar locate-command)

(load-module-file "modules/osx/autoload/open.el")

(defun osx-tests--load ()
  "Load the module config with `use-package' stubbed inert for batch."
  (let ((stub (not (fboundp 'use-package))))
    (unwind-protect
        (progn
          (when stub
            (defalias 'use-package (cons 'macro (lambda (&rest _) nil))))
          (load-module-file "modules/osx/config.el"))
      (when stub (fmakunbound 'use-package)))))

(describe "osx module config"
  (it "restores right-option Meta (doom-defaults downgrades it to 'none)"
    (let ((mac-right-option-modifier 'none))
      (osx-tests--load)
      (expect mac-right-option-modifier :to-be 'meta)))

  (it "carries the doom os/macos defaults"
    (let ((locate-command "locate")
          (ns-pop-up-frames t)
          (delete-by-moving-to-trash nil))
      (osx-tests--load)
      (expect locate-command :to-equal "mdfind")
      (expect ns-pop-up-frames :to-be nil)
      ;; batch is noninteractive - trash engages in real sessions only
      (expect delete-by-moving-to-trash :to-be nil)))

  (it "appends keychain backends once auth-source loads"
    (osx-tests--load)
    (require 'auth-source)
    (expect (last auth-sources 2)
            :to-equal '(macos-keychain-generic macos-keychain-internet))))

(describe "macos-open-with"
  (it "opens the current buffer's file with the default program"
    (let (captured)
      (cl-letf (((symbol-function 'doom-call-process)
                 (lambda (&rest args) (setq captured args) '(0 . "")))
                ((symbol-function 'buffer-file-name)
                 (lambda (&optional _) "/tmp/it's notes.txt")))
        (macos-open-in-default-program)
        (expect (car captured) :to-equal "open")
        (expect (cadr captured) :to-match "notes\\.txt\\'"))))

  (it "passes -a APP before the path when an app is named"
    (let (captured)
      (cl-letf (((symbol-function 'doom-call-process)
                 (lambda (&rest args) (setq captured args) '(0 . ""))))
        (macos-open-with "Finder" "/tmp/x")
        (expect captured :to-equal '("open" "-a" "Finder" "/tmp/x"))))))

(describe "macos-reveal-in-finder"
  (it "opens the current directory in Finder"
    (let (captured)
      (cl-letf (((symbol-function 'doom-call-process)
                 (lambda (&rest args) (setq captured args) '(0 . ""))))
        (let ((default-directory "/tmp/reveal-me/"))
          (macos-reveal-in-finder))
        (expect (seq-take captured 3) :to-equal '("open" "-a" "Finder"))
        (expect (nth 3 captured) :to-match "reveal-me")))))

(describe "macos-reveal-project-in-finder"
  (it "reveals the project root inside a project"
    (let (captured)
      (cl-letf (((symbol-function 'doom-call-process)
                 (lambda (&rest args) (setq captured args) '(0 . "")))
                ((symbol-function 'project-current) (lambda (&rest _) 'proj))
                ((symbol-function 'project-root) (lambda (_) "/tmp/projroot/")))
        (macos-reveal-project-in-finder)
        (expect (nth 2 captured) :to-equal "Finder")
        (expect (nth 3 captured) :to-match "projroot"))))
  (it "falls back to default-directory outside a project"
    (let (captured)
      (cl-letf (((symbol-function 'doom-call-process)
                 (lambda (&rest args) (setq captured args) '(0 . "")))
                ((symbol-function 'project-current) (lambda (&rest _) nil)))
        (let ((default-directory "/tmp/noproj/"))
          (macos-reveal-project-in-finder))
        (expect (nth 3 captured) :to-match "noproj")))))
