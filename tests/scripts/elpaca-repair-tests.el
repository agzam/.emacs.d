;;; tests/scripts/elpaca-repair-tests.el --- repair verdict specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

;; Loading the script is safe here: the driver is gated on (featurep 'elpaca),
;; which the sandbox never has - only the report function gets defined.
(load-module-file "scripts/elpaca-repair.el")

(describe "elpaca-repair-report"
  (it "passes a clean run with nothing to repair"
    (pcase-let ((`(,ok . ,text)
                 (elpaca-repair-report '((evil . finished) (magit . finished))
                                       nil nil)))
      (expect ok :to-be-truthy)
      (expect text :to-match "\\`REPAIR-OK\n")
      (expect text :to-match "packages: 2 processed, 0 failed/blocked, 0 rebuilt\n")
      (expect text :to-match "nothing to repair\n")))

  (it "passes and lists what it rebuilt"
    (pcase-let ((`(,ok . ,text)
                 (elpaca-repair-report '((vulpea . finished))
                                       '((vulpea . half-built)) nil)))
      (expect ok :to-be-truthy)
      (expect text :to-match "\\`REPAIR-OK\n")
      (expect text :to-match "packages: 1 processed, 0 failed/blocked, 1 rebuilt\n")
      (expect text :to-match "  rebuilt vulpea (half-built)\n")
      (expect text :not :to-match "nothing to repair")))

  (it "fails on a failed/blocked status"
    (pcase-let ((`(,ok . ,text)
                 (elpaca-repair-report '((evil . finished) (ghostel . failed))
                                       nil nil)))
      (expect ok :to-be nil)
      (expect text :to-match "\\`REPAIR-FAILED\n")
      (expect text :to-match "  ghostel: failed\n")))

  (it "fails when a rebuild did not actually heal the package"
    (pcase-let ((`(,ok . ,text)
                 (elpaca-repair-report '((vulpea . finished))
                                       '((vulpea . half-built))
                                       '((vulpea . half-built)))))
      (expect ok :to-be nil)
      (expect text :to-match "\\`REPAIR-FAILED\n")
      (expect text :to-match "  STILL BROKEN vulpea (half-built)\n"))))

;;; tests/scripts/elpaca-repair-tests.el ends here
