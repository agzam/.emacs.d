;;; tests/scripts/check-root-tests.el --- config-root tripwire specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "scripts/check-root.el")

(defun check-root-tests--repo (&rest tracked)
  "Make a temp git repo whose committed files are TRACKED, and return its path."
  (let* ((dir (file-name-as-directory (make-temp-file "check-root" t)))
         (default-directory dir))
    (dolist (relpath tracked)
      (make-directory (file-name-directory (expand-file-name relpath dir)) t)
      (write-region "" nil (expand-file-name relpath dir) nil 'silent))
    (call-process "git" nil nil nil "init" "-q")
    (call-process "git" nil nil nil "add" "-A")
    dir))

(describe "check-root-strays"
  (it "sees nothing in a repo where every entry is tracked"
    (let ((dir (check-root-tests--repo "init.el" "modules/git/config.el")))
      (unwind-protect
          (expect (check-root-strays dir) :to-equal nil)
        (delete-directory dir t))))

  (it "flags an untracked directory that is not sanctioned"
    (let ((dir (check-root-tests--repo "init.el")))
      (unwind-protect
          (progn
            (make-directory (expand-file-name "eln-cache" dir))
            (expect (check-root-strays dir) :to-equal '("eln-cache")))
        (delete-directory dir t))))

  (it "flags an untracked file at the root"
    (let ((dir (check-root-tests--repo "init.el")))
      (unwind-protect
          (progn
            (write-region "" nil (expand-file-name "recentf" dir) nil 'silent)
            (expect (check-root-strays dir) :to-equal '("recentf")))
        (delete-directory dir t))))

  (it "tolerates the sanctioned untracked entries"
    (let ((dir (check-root-tests--repo "init.el")))
      (unwind-protect
          (progn
            (make-directory (expand-file-name ".local" dir))
            (make-directory (expand-file-name ".clj-kondo" dir))
            (write-region "" nil (expand-file-name "custom.el" dir) nil 'silent)
            (expect (check-root-strays dir) :to-equal nil))
        (delete-directory dir t))))

  (it "reports strays sorted, and only the root level"
    (let ((dir (check-root-tests--repo "init.el" "modules/git/config.el")))
      (unwind-protect
          (progn
            (make-directory (expand-file-name "transient" dir))
            (make-directory (expand-file-name "elpa" dir))
            ;; nested droppings are somebody else's problem: only the root counts
            (write-region "" nil (expand-file-name "modules/git/stray.el" dir) nil 'silent)
            (expect (check-root-strays dir) :to-equal '("elpa" "transient")))
        (delete-directory dir t))))

  (it "counts a tracked entry as allowed even before it is committed"
    (let ((dir (check-root-tests--repo "MIGRATION.org")))
      (unwind-protect
          (expect (check-root-strays dir) :to-equal nil)
        (delete-directory dir t)))))
