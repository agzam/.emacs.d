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

  (it "prints event log lines indented under the matching fatal package"
    ;; a bare "consult-gh: failed" forced log archaeology to learn the
    ;; clone died mid-transfer; the reason must ride along in the marker
    (pcase-let ((`(,ok . ,text)
                 (smoke-report '((consult-gh . failed)) nil nil nil nil
                               '((consult-gh . ("$git clone --depth 1 ..."
                                                "fatal: early EOF"))))))
      (expect ok :to-be nil)
      (expect text :to-match "  consult-gh: failed\n")
      (expect text :to-match "      \\$git clone --depth 1 \\.\\.\\.\n")
      (expect text :to-match "      fatal: early EOF\n")))

  (it "omits log lines for tolerated packages"
    (pcase-let ((`(,_ . ,text)
                 (smoke-report '((khalendario . failed)) nil nil '(khalendario) nil
                               '((khalendario . ("fatal: could not read Username"))))))
      (expect text :not :to-match "could not read Username")))

  (it "ships an empty default tolerance list - private repos authenticate in CI"
    (expect smoke-tolerated-packages :to-be nil)))

(describe "smoke-package-events"
  (it "returns nil when elpaca never loaded"
    ;; the sandbox has no elpaca - the bootstrap-abort path must not
    ;; blow up collecting logs for a report that has no packages anyway
    (expect (smoke-package-events 'anything) :to-be nil)))

(describe "smoke-should-report-now-p"
  (it "reports immediately once elpaca has settled"
    (expect (smoke-should-report-now-p t nil t) :to-be-truthy))

  (it "reports immediately when init died before the queue was armed"
    ;; the poisoned-cache class: bootstrap error aborts init.el before
    ;; elpaca-process-queues reaches after-init-hook, so nothing ever
    ;; fires elpaca-after-init-hook and only the watchdog would end it
    (expect (smoke-should-report-now-p nil t nil) :to-be-truthy))

  (it "waits when init errored but queues are armed and settling"
    ;; killing mid-processing would leave half-built dirs; the settle
    ;; hook still reports the init error as fatal
    (expect (smoke-should-report-now-p nil t t) :to-be nil))

  (it "waits during a normal cold boot"
    (expect (smoke-should-report-now-p nil nil t) :to-be nil)))

(describe "smoke-write-result"
  (it "writes a failed marker without elpaca when init errored before queuing"
    ;; elpaca is absent in this sandbox - exactly the state after a
    ;; bootstrap abort; the marker must still appear, verdict failed
    (let* ((marker (make-temp-file "smoke-marker"))
           (smoke-result-file marker)
           (init-file-had-error t)
           (exit-code nil))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'kill-emacs)
                       (lambda (&optional code) (setq exit-code code))))
              (smoke-write-result))
            (let ((text (with-temp-buffer
                          (insert-file-contents marker)
                          (buffer-string))))
              (expect text :to-match "\\`SMOKE-FAILED\n")
              (expect text :to-match "packages: 0 queued, 0 failed/blocked\n")
              (expect text :to-match "init\\.el signaled an error during startup\n"))
            (expect exit-code :to-be 1))
        (delete-file marker)))))

;;; tests/scripts/smoke-check-tests.el ends here
