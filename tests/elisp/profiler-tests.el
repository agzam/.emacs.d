;;; tests/elisp/profiler-tests.el --- elisp/autoload/profiler.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/elisp/autoload/profiler.el")

(describe "toggle-profiler"
  (before-each
    (setq toggle-profiler--running nil))

  (it "starts a cpu+mem profile on the first toggle"
    (let (calls)
      (cl-letf (((symbol-function 'profiler-start)
                 (lambda (mode) (push (list 'start mode) calls)))
                ((symbol-function 'profiler-report)
                 (lambda () (push '(report) calls)))
                ((symbol-function 'profiler-stop)
                 (lambda () (push '(stop) calls))))
        (toggle-profiler)
        (expect calls :to-equal '((start cpu+mem)))
        (expect toggle-profiler--running :to-be-truthy))))

  (it "pops the report before stopping on the second toggle"
    (let (calls)
      (cl-letf (((symbol-function 'profiler-start)
                 (lambda (_mode) (push '(start) calls)))
                ((symbol-function 'profiler-report)
                 (lambda () (push '(report) calls)))
                ((symbol-function 'profiler-stop)
                 (lambda () (push '(stop) calls))))
        (toggle-profiler)
        (setq calls nil)
        (toggle-profiler)
        ;; stopping first would throw the sample data away
        (expect (reverse calls) :to-equal '((report) (stop)))
        (expect toggle-profiler--running :to-be nil)))))

(describe "profiler-report-expand-all"
  (it "expands every entry in each report buffer, then rewinds"
    (let ((expansions 0)
          (buf (generate-new-buffer "*CPU-Profiler-Report 2026*")))
      (unwind-protect
          (cl-letf (((symbol-function 'profiler-report-expand-entry)
                     (lambda () (cl-incf expansions)))
                    ((symbol-function 'profiler-report-next-entry)
                     (lambda () (forward-line 1))))
            (with-current-buffer buf
              (insert "one\ntwo\nthree"))
            (profiler-report-expand-all)
            (expect expansions :to-equal 3)
            (expect (with-current-buffer buf (point)) :to-equal 1))
        (kill-buffer buf)))))

(describe "profiler-report-helpful-symbol-at-point"
  (it "sends the profiler-entry text property to helpful"
    (let (seen)
      (cl-letf (((symbol-function 'helpful-symbol)
                 (lambda (sym) (setq seen sym))))
        (with-temp-buffer
          (insert (propertize "some-fn" 'profiler-entry 'some-fn))
          (goto-char (point-min))
          (profiler-report-helpful-symbol-at-point))
        (expect seen :to-be 'some-fn)))))
