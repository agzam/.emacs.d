;;; tests/scripts/e2e-check-tests.el --- e2e verdict specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

;; Loading the probe is safe here: elpaca is absent from the sandbox, so the
;; result-writing path only installs a hook that never fires, and the batch
;; process exits long before the watchdog timer could.
(load-module-file "scripts/e2e-check.el")

(defun e2e-tests--scenario-dir (&rest files)
  "A throwaway tests/e2e/ holding FILES, each a (NAME . CONTENTS) pair."
  (let* ((root (file-name-as-directory (make-temp-file "e2e-root" t)))
         (dir (expand-file-name "tests/e2e/" root)))
    (make-directory dir t)
    (dolist (file files)
      (with-temp-file (expand-file-name (car file) dir)
        (insert (cdr file))))
    root))

(describe "e2e-prewarm"
  ;; the sandbox has no jinx on the load-path: the require must fail
  ;; softly and the run proceed, same as a config without the package
  (it "tolerates an environment without jinx"
    (expect (e2e-prewarm) :not :to-throw)))

(describe "e2e-report"
  ;; the whole point of the tier is that nothing passes quietly; a run that
  ;; loaded no scenario, or whose only scenario died, must not read as green
  (it "fails a run that produced no results at all"
    (pcase-let ((`(,ok . ,text) (e2e-report nil)))
      (expect ok :to-be nil)
      (expect text :to-match "\\`E2E-FAILED\n")
      (expect text :to-match "0 cases, 0 failed\n")))

  (it "passes when every case passed"
    (pcase-let ((`(,ok . ,text)
                 (e2e-report '((:label "a" :ok t) (:label "b" :ok t)))))
      (expect ok :to-be-truthy)
      (expect text :to-match "\\`E2E-OK\n")
      (expect text :to-match "2 cases, 0 failed\n")
      (expect text :to-match "PASS a\n")))

  (it "fails on a single failed case and names it"
    (pcase-let ((`(,ok . ,text)
                 (e2e-report '((:label "a" :ok t)
                               (:label "b" :ok nil :got "x" :want "y")))))
      (expect ok :to-be nil)
      (expect text :to-match "\\`E2E-FAILED\n")
      (expect text :to-match "2 cases, 1 failed\n")
      (expect text :to-match "FAIL b\n")))

  (it "appends the *Warnings* text" ; a boot that warns is worth reading
    (let ((buf (get-buffer-create "*Warnings*")))
      (unwind-protect
          (progn
            (with-current-buffer buf (insert "boom happened\n"))
            (pcase-let ((`(,_ . ,text) (e2e-report '((:label "a" :ok t)))))
              (expect text :to-match "\\*Warnings\\*:\nboom happened\n")))
        (kill-buffer buf)))))

(describe "e2e-format-result"
  (it "reports the target, the command the keys resolved to and the probe"
    (expect (e2e-format-result
             '(:label "a ref -> markdown (c m)" :ok t :mode markdown-mode
               :type bug-reference-link :want-type bug-reference-link
               :keys "c m" :cmd link-bug-reference->link-markdown :probe t
               :got "x" :want "x"))
            :to-match
            "target=bug-reference-link (want bug-reference-link) keys=\"c m\" -> link-bug-reference->link-markdown probe=t"))

  (it "shows the wanted text only when it differs from what happened"
    (expect (e2e-format-result '(:label "a" :ok t :got "x" :want "x"))
            :not :to-match "want:")
    (expect (e2e-format-result '(:label "a" :ok nil :got "x" :want "y"))
            :to-match "     want: \"y\"\n"))

  (it "carries the error of a case that signalled"
    (expect (e2e-format-result '(:label "a" :ok nil :err (error "boom")))
            :to-match "error: (error \"boom\")")))

(describe "e2e-run-scenarios"
  (it "runs every registered scenario, oldest registration first"
    (let ((e2e-scenarios (list (lambda () '((:label "second" :ok t)))
                               (lambda () '((:label "first" :ok t))))))
      (expect (mapcar (lambda (r) (plist-get r :label)) (e2e-run-scenarios))
              :to-equal '("first" "second"))))

  (it "turns a scenario that signalled into a failure without losing the others"
    (let* ((e2e-scenarios (list (lambda () '((:label "kept" :ok t)))
                                (lambda () (error "boom"))))
           (results (e2e-run-scenarios)))
      (expect (length results) :to-equal 2)
      (expect (cl-remove-if-not (lambda (r) (plist-get r :ok)) results) :to-have-same-items-as
              '((:label "kept" :ok t)))
      (expect (plist-get (car results) :label) :to-match "signalled"))))

(describe "e2e-load-scenarios"
  (it "loads every scenario file in tests/e2e/"
    (let* ((e2e-scenarios nil)
           (e2e-root (e2e-tests--scenario-dir
                      '("one.el" . "(add-to-list 'e2e-scenarios #'ignore)"))))
      (expect (e2e-load-scenarios) :to-be nil)
      (expect e2e-scenarios :to-equal (list #'ignore))))

  ;; a scenario file that cannot load must fail the run by name: running
  ;; fewer cases than the tree holds is the silence this tier exists to end
  (it "reports a file that died on load, and loads the rest anyway"
    (let* ((e2e-scenarios nil)
           (e2e-root (e2e-tests--scenario-dir
                      '("broken.el" . "(error \"boom\")")
                      '("fine.el" . "(add-to-list 'e2e-scenarios #'ignore)")))
           (failures (e2e-load-scenarios)))
      (expect (length failures) :to-equal 1)
      (expect (plist-get (car failures) :label) :to-equal "loading broken.el")
      (expect (plist-get (car failures) :ok) :to-be nil)
      (expect e2e-scenarios :to-equal (list #'ignore)))))
