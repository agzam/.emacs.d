;;; tests/lisp/local-dev-tests.el --- own-package checkout cloning specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

(load-module-file "lisp/local-dev.el")

;; The suites run without init.el, which is where elpaca's builds dir is set;
;; declaring it special keeps the `let' below dynamic, as the code reads it.
(defvar elpaca-builds-directory)

(describe "local-dev--derive-repo"
  (it "takes the last two path components as OWNER/REPO"
    (expect (local-dev--derive-repo "/home/x/GitHub/agzam/remoto.el")
            :to-equal "agzam/remoto.el"))
  (it "ignores a trailing slash"
    (expect (local-dev--derive-repo "/home/x/GitHub/agzam/consult-hn/")
            :to-equal "agzam/consult-hn")))

(describe "local-dev-clone-spec"
  (it "derives an ssh url and a nil branch by default"
    (expect (local-dev-clone-spec 'remoto "/h/GitHub/agzam/remoto.el" nil)
            :to-equal '("git@github.com:agzam/remoto.el.git")))
  (it "honors a :repo override when the dir name isn't the repo (spacehammer)"
    (expect (local-dev-clone-spec 'spacehammer "/h/.hammerspoon"
                                  '((spacehammer :repo "agzam/spacehammer")))
            :to-equal '("git@github.com:agzam/spacehammer.git")))
  (it "honors a pinned :branch override"
    (expect (local-dev-clone-spec 'reddigg "/h/GitHub/agzam/emacs-reddigg"
                                  '((reddigg :branch "next")))
            :to-equal '("git@github.com:agzam/emacs-reddigg.git" . "next")))
  (it "follows the url format CI binds for its keyless https clones"
    (let ((local-dev-clone-url-format "https://github.com/%s.git"))
      (expect (local-dev-clone-spec 'prisma "/h/GitHub/agzam/prisma.el" nil)
              :to-equal '("https://github.com/agzam/prisma.el.git")))))

(describe "ensure-local-dev-checkouts"
  (let ((base nil))
    (before-each (setq base (make-temp-file "local-dev-test" t)))
    (after-each (when (and base (file-directory-p base)) (delete-directory base t)))

    (it "skips a checkout that already exists"
      (let ((existing (expand-file-name "already/there.el" base))
            (called nil))
        (make-directory existing t)
        (cl-letf (((symbol-function 'call-process)
                   (lambda (&rest _) (setq called t) 0)))
          (expect (ensure-local-dev-checkouts (list (cons 'there existing)) nil #'ignore)
                  :to-be nil))
        (expect called :to-be nil)))

    (it "clones a missing checkout over ssh, creating parent dirs"
      (let ((dir (expand-file-name "GitHub/agzam/khalendario.el" base))
            (recorded nil))
        (cl-letf (((symbol-function 'call-process)
                   (lambda (program _infile _dest _display &rest args)
                     (setq recorded (cons program args))
                     0)))
          (expect (ensure-local-dev-checkouts (list (cons 'khalendario dir)) nil #'ignore)
                  :to-equal '(khalendario)))
        (expect (file-directory-p (expand-file-name "GitHub/agzam" base)) :to-be t)
        (expect recorded :to-equal
                (list "git" "clone" "git@github.com:agzam/khalendario.el.git" dir))))

    (it "passes an override branch through to git clone"
      (let ((dir (expand-file-name "GitHub/agzam/emacs-reddigg" base))
            (recorded nil))
        (cl-letf (((symbol-function 'call-process)
                   (lambda (program _infile _dest _display &rest args)
                     (setq recorded (cons program args))
                     0)))
          (ensure-local-dev-checkouts
           (list (cons 'reddigg dir))
           '((reddigg :branch "next")) #'ignore))
        (expect recorded :to-equal
                (list "git" "clone" "--branch" "next"
                      "git@github.com:agzam/emacs-reddigg.git" dir))))

    (it "warns and does not error when the clone fails"
      (let ((dir (expand-file-name "GitHub/agzam/private.el" base))
            (emitted nil))
        (cl-letf (((symbol-function 'call-process) (lambda (&rest _) 1)))
          (expect (ensure-local-dev-checkouts
                   (list (cons 'private dir)) nil
                   (lambda (fmt &rest args) (push (apply #'format fmt args) emitted)))
                  :to-be nil))
        (expect (cl-some (lambda (l) (string-match-p "WARNING" l)) emitted)
                :to-be-truthy)))

    (it "removes a partially-written checkout when the clone fails"
      ;; A non-empty leftover would otherwise make `local-checkout-recipe'
      ;; build in place from junk on the next boot.
      (let ((dir (expand-file-name "GitHub/agzam/partial.el" base)))
        (cl-letf (((symbol-function 'call-process)
                   (lambda (&rest _)
                     (make-directory dir t)
                     (write-region "junk" nil (expand-file-name "f.el" dir)
                                   nil 'silent)
                     1)))
          (ensure-local-dev-checkouts (list (cons 'partial dir)) nil #'ignore))
        (expect (file-directory-p dir) :to-be nil)))))

(describe "drop-local-dev-builds"
  (let ((builds nil))
    (before-each
      (setq builds (make-temp-file "local-dev-builds" t))
      (dolist (name '("prisma" "occult"))
        (let ((dir (expand-file-name name builds)))
          (make-directory dir t)
          (write-region "stale" nil (expand-file-name (concat name ".elc") dir)
                        nil 'silent))))
    (after-each (when (and builds (file-directory-p builds))
                  (delete-directory builds t)))

    (it "deletes the build of the named packages only"
      (expect (drop-local-dev-builds '(prisma) builds #'ignore) :to-equal '(prisma))
      (expect (file-directory-p (expand-file-name "prisma" builds)) :to-be nil)
      (expect (file-directory-p (expand-file-name "occult" builds)) :to-be t))

    (it "reports nothing for a package elpaca never built"
      (expect (drop-local-dev-builds '(never-built) builds #'ignore) :to-be nil))

    (it "deletes nothing when elpaca's builds dir is unknown"
      (let ((elpaca-builds-directory nil))
        (expect (drop-local-dev-builds '(prisma) nil #'ignore) :to-be nil))
      (expect (file-directory-p (expand-file-name "prisma" builds)) :to-be t))))

;;; tests/lisp/local-dev-tests.el ends here
