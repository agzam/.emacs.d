;;; tests/dired/project-tests.el --- dired/autoload/project.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; treemacs isn't installed in the batch tier; the toggle requires it at
;; call time - stub the feature and the entry points it dispatches on.
;; Real treemacs window/workspace behavior is probe territory.
(provide 'treemacs)

(load-module-file "modules/dired/autoload/project.el")

(describe "treemacs-project-toggle+"
  (it "deletes the local treemacs window when one is visible"
    (let (deleted)
      (cl-letf (((symbol-function 'treemacs-current-visibility)
                 (lambda () 'visible))
                ((symbol-function 'treemacs-get-local-window)
                 (lambda () 'window-sentinel))
                ((symbol-function 'delete-window)
                 (lambda (win) (setq deleted win))))
        (treemacs-project-toggle+)
        (expect deleted :to-be 'window-sentinel))))

  (it "adds and displays the current project exclusively otherwise"
    (let (displayed)
      (cl-letf (((symbol-function 'treemacs-current-visibility)
                 (lambda () 'none))
                ((symbol-function 'treemacs-add-and-display-current-project-exclusively)
                 (lambda () (setq displayed t))))
        (treemacs-project-toggle+)
        (expect displayed :to-be-truthy)))))

(describe "dired-jump-find-in-project"
  ;; subtree descent to the visited file needs real dired-subtree overlays -
  ;; probe territory; the root resolution branches are covered here.
  (it "opens dired at the project.el root"
    (let (opened)
      (with-temp-buffer
        (cl-letf (((symbol-function 'project-current)
                   (lambda (&rest _) 'project-sentinel))
                  ((symbol-function 'project-root)
                   (lambda (project)
                     (expect project :to-be 'project-sentinel)
                     "/fake/root/"))
                  ((symbol-function 'dired)
                   (lambda (dir) (setq opened dir)))
                  ((symbol-function 'recenter)
                   (lambda (&rest _))))
          (dired-jump-find-in-project)
          (expect opened :to-equal "/fake/root/")))))

  (it "falls back to default-directory outside projects"
    (let (opened)
      (with-temp-buffer
        (setq default-directory "/tmp/nowhere/")
        (cl-letf (((symbol-function 'project-current)
                   (lambda (&rest _) nil))
                  ((symbol-function 'dired)
                   (lambda (dir) (setq opened dir)))
                  ((symbol-function 'recenter)
                   (lambda (&rest _))))
          (dired-jump-find-in-project)
          (expect opened :to-equal "/tmp/nowhere/"))))))
