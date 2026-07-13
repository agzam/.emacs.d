;;; tests/pdf/nov-tests.el --- pdf/autoload/nov.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
;; text-scale-mode-step / -amount live here; nov--text-scale-adjust reads them.
(require 'face-remap)

;; nov.el isn't installed in the batch tier.
(defvar nov-history nil)
(defvar nov-variable-pitch nil)

(load-module-file "modules/pdf/autoload/nov.el")

(describe "nov-back-or-quit"
  (it "steps back when there is history"
    (let ((nov-history '(a b)) called)
      (cl-letf (((symbol-function 'nov-history-back)
                 (lambda () (setq called 'back)))
                ((symbol-function 'kill-current-buffer)
                 (lambda () (setq called 'kill))))
        (nov-back-or-quit)
        (expect called :to-be 'back))))

  (it "kills the buffer when there is no history"
    (let ((nov-history nil) called)
      (cl-letf (((symbol-function 'nov-history-back)
                 (lambda () (setq called 'back)))
                ((symbol-function 'kill-current-buffer)
                 (lambda () (setq called 'kill))))
        (nov-back-or-quit)
        (expect called :to-be 'kill)))))

(describe "nov text scaling"
  (it "increase remaps shr faces when variable-pitch"
    (let ((nov-variable-pitch t) seen)
      (cl-letf (((symbol-function 'nov--text-scale-adjust)
                 (lambda (n) (setq seen (list 'adjust n))))
                ((symbol-function 'text-scale-increase)
                 (lambda (n) (setq seen (list 'plain n)))))
        (nov-text-scale-increase)
        (expect seen :to-equal '(adjust 0.5)))))

  (it "increase uses plain text-scale when fixed-pitch"
    (let ((nov-variable-pitch nil) seen)
      (cl-letf (((symbol-function 'nov--text-scale-adjust)
                 (lambda (n) (setq seen (list 'adjust n))))
                ((symbol-function 'text-scale-increase)
                 (lambda (n) (setq seen (list 'plain n)))))
        (nov-text-scale-increase)
        (expect seen :to-equal '(plain 0.5)))))

  (it "decrease remaps shr faces when variable-pitch"
    (let ((nov-variable-pitch t) seen)
      (cl-letf (((symbol-function 'nov--text-scale-adjust)
                 (lambda (n) (setq seen (list 'adjust n))))
                ((symbol-function 'text-scale-decrease)
                 (lambda (n) (setq seen (list 'plain n)))))
        (nov-text-scale-decrease)
        (expect seen :to-equal '(adjust -0.5)))))

  (it "decrease uses plain text-scale when fixed-pitch"
    ;; text-scale-decrease itself subtracts, so it is handed a positive 0.5.
    (let ((nov-variable-pitch nil) seen)
      (cl-letf (((symbol-function 'nov--text-scale-adjust)
                 (lambda (n) (setq seen (list 'adjust n))))
                ((symbol-function 'text-scale-decrease)
                 (lambda (n) (setq seen (list 'plain n)))))
        (nov-text-scale-decrease)
        (expect seen :to-equal '(plain 0.5))))))

(describe "nov--text-scale-adjust"
  (it "scales the default face and remaps the shr faces nov uses"
    (with-temp-buffer
      (let (scaled remaps)
        (cl-letf (((symbol-function 'text-scale-increase)
                   (lambda (n) (setq scaled n)))
                  ((symbol-function 'face-remap-add-relative)
                   (lambda (face &rest _) (push face remaps))))
          (nov--text-scale-adjust 0.5)
          (expect scaled :to-equal 0.5)
          (expect (memq 'shr-text remaps) :to-be-truthy)
          (expect (memq 'shr-h1 remaps) :to-be-truthy)
          (expect (memq 'shr-link remaps) :to-be-truthy))))))

;;; nov-tests.el ends here
