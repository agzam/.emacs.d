;;; tests/scripts/smoke-check-tests.el --- smoke verdict specs -*- lexical-binding: t; -*-

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
(load-module-file "scripts/smoke-check.el")

(describe "smoke-report"
  (it "passes a clean boot"
    (pcase-let ((`(,ok . ,text)
                 (smoke-report '((evil . finished) (magit . finished)) nil nil nil)))
      (expect ok :to-be-truthy)
      (expect text :to-match "\\`SMOKE-OK\n")
      (expect text :to-match "packages: 2 queued, 0 failed/blocked\n")))

  (it "fails on a failed package with an empty tolerance list"
    (pcase-let ((`(,ok . ,text)
                 (smoke-report '((evil . finished) (khalendario . failed)) nil nil nil)))
      (expect ok :to-be nil)
      (expect text :to-match "\\`SMOKE-FAILED\n")
      (expect text :to-match "packages: 2 queued, 1 failed/blocked\n")
      (expect text :to-match "  khalendario: failed\n")))

  (it "tolerates a listed private package, keeping the verdict green"
    (pcase-let ((`(,ok . ,text)
                 (smoke-report '((evil . finished) (khalendario . failed))
                               nil nil '(khalendario))))
      (expect ok :to-be-truthy)
      (expect text :to-match "\\`SMOKE-OK\n")
      (expect text :to-match "packages: 2 queued, 0 failed/blocked, 1 tolerated (private)\n")
      (expect text :to-match "  khalendario: failed (tolerated)\n")))

  (it "still fails when an unlisted package breaks alongside a tolerated one"
    (pcase-let ((`(,ok . ,text)
                 (smoke-report '((evil . blocked) (khalendario . failed))
                               nil nil '(khalendario))))
      (expect ok :to-be nil)
      (expect text :to-match "\\`SMOKE-FAILED\n")
      (expect text :to-match "packages: 2 queued, 1 failed/blocked, 1 tolerated (private)\n")
      (expect text :to-match "  evil: blocked\n")
      (expect text :to-match "  khalendario: failed (tolerated)\n")))

  (it "treats an init error as fatal even with all failures tolerated"
    (pcase-let ((`(,ok . ,text)
                 (smoke-report '((khalendario . failed)) t nil '(khalendario))))
      (expect ok :to-be nil)
      (expect text :to-match "\\`SMOKE-FAILED\n")
      (expect text :to-match "init\\.el signaled an error during startup\n")))

  (it "appends the *Warnings* text verbatim"
    (pcase-let ((`(,_ . ,text)
                 (smoke-report '((evil . finished)) nil "boom happened\n" nil)))
      (expect text :to-match "\\*Warnings\\*:\nboom happened\n")))

  (it "fails on a half-built package even when every status is finished"
    ;; finished status can hide a link-only build dir (interrupted update);
    ;; the half-built list must flip the verdict on its own
    (pcase-let ((`(,ok . ,text)
                 (smoke-report '((evil . finished) (vulpea . finished))
                               nil nil nil '((vulpea . "/builds/vulpea")))))
      (expect ok :to-be nil)
      (expect text :to-match "\\`SMOKE-FAILED\n")
      (expect text :to-match "packages: 2 queued, 0 failed/blocked, 1 half-built\n")
      (expect text :to-match "  vulpea: half-built (no autoloads in /builds/vulpea)\n")))

  (it "stays green with a nil half-built list"
    (pcase-let ((`(,ok . ,_)
                 (smoke-report '((evil . finished)) nil nil nil nil)))
      (expect ok :to-be-truthy)))

  (it "ships khalendario in the default tolerance list"
    (expect (memq 'khalendario smoke-tolerated-packages) :to-be-truthy)))

;;; tests/scripts/smoke-check-tests.el ends here
