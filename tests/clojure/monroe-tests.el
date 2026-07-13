;;; tests/clojure/monroe-tests.el --- clojure/autoload/monroe.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/clojure/autoload/monroe.el")

(defvar monroe-default-port 7888)

(describe "hammerspoon-monroe--cleanup"
  (it "kills only monroe buffers"
    (let ((monroe-buf (generate-new-buffer "*monroe: localhost:7888*"))
          (other (generate-new-buffer "not-a-repl")))
      (unwind-protect
          (progn
            (hammerspoon-monroe--cleanup)
            (expect (buffer-live-p monroe-buf) :to-be nil)
            (expect (buffer-live-p other) :to-be-truthy))
        (dolist (b (list monroe-buf other))
          (when (buffer-live-p b) (kill-buffer b)))))))

(describe "hammerspoon-monroe-sentinel-a"
  (it "schedules a reconnect when a monroe process dies"
    (let (timer-args orig-ran)
      (setq hammerspoon-monroe--reconnect-timer nil)
      (cl-letf (((symbol-function 'process-name) (lambda (_) "monroe/nrepl"))
                ((symbol-function 'process-live-p) #'ignore)
                ((symbol-function 'run-with-timer)
                 (lambda (&rest args) (setq timer-args args) 'fake-timer)))
        (hammerspoon-monroe-sentinel-a
         (lambda (&rest _) (setq orig-ran t)) 'proc "finished")
        (expect orig-ran :to-be t)
        (expect (car timer-args) :to-equal 1.5)
        (expect (caddr timer-args) :to-be #'hammerspoon-monroe--try-reconnect)
        (expect hammerspoon-monroe--reconnect-timer :to-equal 'fake-timer))))

  (it "leaves live or foreign processes alone"
    (let (timer-set)
      (setq hammerspoon-monroe--reconnect-timer nil)
      (cl-letf (((symbol-function 'process-name) (lambda (_) "some-other"))
                ((symbol-function 'process-live-p) #'ignore)
                ((symbol-function 'run-with-timer)
                 (lambda (&rest _) (setq timer-set t))))
        (hammerspoon-monroe-sentinel-a #'ignore 'proc "finished")
        (expect timer-set :to-be nil)))))

(describe "hammerspoon-monroe-connect"
  (it "cleans up stale state, then connects to the configured port"
    (let (connected cleaned)
      (cl-letf (((symbol-function 'hammerspoon-monroe--cleanup)
                 (lambda () (setq cleaned t)))
                ((symbol-function 'monroe)
                 (lambda (host) (setq connected host))))
        (hammerspoon-monroe-connect)
        (expect cleaned :to-be t)
        (expect connected :to-equal "localhost:7888")))))
