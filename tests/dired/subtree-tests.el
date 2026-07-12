;;; tests/dired/subtree-tests.el --- dired/autoload/subtree.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; dired-subtree isn't installed in the batch tier; the fns under test call
;; it at runtime - stub per spec via cl-letf.  Real subtree expand/collapse
;; round-trips (and the treemacs icons advice) are probe territory.

(load-module-file "modules/dired/autoload/subtree.el")

(describe "dired-remove-subtree"
  (it "steps into the subtree before removing when point is on its root"
    (let (calls)
      (cl-letf (((symbol-function 'dired-subtree--is-expanded-p)
                 (lambda () t))
                ((symbol-function 'dired-next-line)
                 (lambda (n) (push (cons 'next-line n) calls)))
                ((symbol-function 'dired-subtree-remove)
                 (lambda () (push 'remove calls))))
        (dired-remove-subtree)
        (expect (nreverse calls) :to-equal '((next-line . 1) remove)))))

  (it "removes in place when point is already inside the subtree"
    (let (calls)
      (cl-letf (((symbol-function 'dired-subtree--is-expanded-p)
                 (lambda () nil))
                ((symbol-function 'dired-next-line)
                 (lambda (n) (push (cons 'next-line n) calls)))
                ((symbol-function 'dired-subtree-remove)
                 (lambda () (push 'remove calls))))
        (dired-remove-subtree)
        (expect calls :to-equal '(remove))))))

(describe "buffer-with-dired-item"
  (it "returns nil outside dired buffers"
    (with-temp-buffer
      (expect (buffer-with-dired-item) :to-be nil)))

  (it "returns the buffer visiting the item at point in dired"
    (let ((sentinel (generate-new-buffer " *subtree-item*")))
      (unwind-protect
          (with-temp-buffer
            (setq major-mode 'dired-mode)
            (cl-letf (((symbol-function 'dired-get-file-for-visit)
                       (lambda () "/tmp/some-item"))
                      ((symbol-function 'find-file-noselect)
                       (lambda (file)
                         (expect file :to-equal "/tmp/some-item")
                         sentinel)))
              (expect (buffer-with-dired-item) :to-be sentinel)))
        (kill-buffer sentinel)))))

(describe "dired-open-item-in-split"
  (it "splits via the given fn and shows the item buffer"
    (let ((sentinel (generate-new-buffer " *split-item*"))
          split-called)
      (unwind-protect
          (save-window-excursion
            (cl-letf (((symbol-function 'buffer-with-dired-item)
                       (lambda () sentinel)))
              (dired-open-item-in-split (lambda () (setq split-called t)))
              (expect split-called :to-be-truthy)
              (expect (current-buffer) :to-be sentinel)))
        (kill-buffer sentinel)))))
