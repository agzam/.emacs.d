;;; tests/lisp/local-dev-tests.el --- own-package checkout cloning specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

(load-module-file "lisp/local-dev.el")

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
  (it "honors a pinned :branch override (reddigg)"
    (expect (local-dev-clone-spec 'reddigg "/h/GitHub/agzam/emacs-reddigg"
                                  '((reddigg :branch "fetch-via-browser")))
            :to-equal '("git@github.com:agzam/emacs-reddigg.git" . "fetch-via-browser"))))

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
           '((reddigg :branch "fetch-via-browser")) #'ignore))
        (expect recorded :to-equal
                (list "git" "clone" "--branch" "fetch-via-browser"
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

;;; tests/lisp/local-dev-tests.el ends here
