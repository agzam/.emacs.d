;;; tests/hammerspoon/monroe-tests.el --- hammerspoon/autoload/monroe.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/hammerspoon/autoload/monroe.el")

(defvar monroe-default-host "localhost:7888")

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
  (it "cleans up stale state, then connects to the configured endpoint"
    (let (connected cleaned)
      (cl-letf (((symbol-function 'hammerspoon-monroe--cleanup)
                 (lambda () (setq cleaned t)))
                ((symbol-function 'monroe)
                 (lambda (host) (setq connected host))))
        (hammerspoon-monroe-connect)
        (expect cleaned :to-be t)
        (expect connected :to-equal "localhost:7888"))))

  ;; monroe carries host and port in one option, and names its buffers
  ;; after the string it was handed - so every endpoint here reads that
  ;; option instead of assembling its own
  (it "takes the endpoint from monroe-default-host"
    (let ((monroe-default-host "elsewhere:1234")
          connected)
      (cl-letf (((symbol-function 'hammerspoon-monroe--cleanup) #'ignore)
                ((symbol-function 'monroe)
                 (lambda (host) (setq connected host))))
        (hammerspoon-monroe-connect)
        (expect connected :to-equal "elsewhere:1234")))))

(describe "hammerspoon-monroe--repl-buffer"
  (it "finds the repl monroe named after the configured endpoint"
    (let* ((monroe-default-host "elsewhere:1234")
           (repl (generate-new-buffer "*monroe: elsewhere:1234*")))
      (unwind-protect
          (cl-letf (((symbol-function 'get-buffer-process) (lambda (_) 'proc))
                    ((symbol-function 'process-live-p) (lambda (_) t)))
            (expect (hammerspoon-monroe--repl-buffer) :to-be repl))
        (kill-buffer repl))))

  (it "reports nothing when the endpoint has no repl buffer"
    (let ((monroe-default-host "nowhere:1"))
      (expect (hammerspoon-monroe--repl-buffer) :to-be nil))))

(describe "hammerspoon-monroe-eval-sync"
  (it "user-errors when no connection is live"
    (cl-letf (((symbol-function 'hammerspoon-monroe--repl-buffer) #'ignore))
      (expect (hammerspoon-monroe-eval-sync "(+ 1 2)") :to-throw 'user-error)))

  (it "accumulates value, out and err across response chunks until done"
    (cl-letf (((symbol-function 'hammerspoon-monroe--repl-buffer)
               (lambda () (current-buffer)))
              ((symbol-function 'monroe-send-eval-string)
               (lambda (_form callback)
                 (funcall callback '(("out" . "hel")))
                 (funcall callback '(("out" . "lo") ("err" . "warn")))
                 (funcall callback '(("value" . "3") ("status" . ("done")))))))
      (expect (hammerspoon-monroe-eval-sync "(+ 1 2)")
              :to-equal '(:value "3" :out "hello" :err "warn"))))

  (it "errors when done never arrives within the timeout"
    (cl-letf (((symbol-function 'hammerspoon-monroe--repl-buffer)
               (lambda () (current-buffer)))
              ((symbol-function 'monroe-send-eval-string)
               (lambda (_form _callback))))
      (expect (hammerspoon-monroe-eval-sync "(+ 1 2)" 0.1) :to-throw 'error))))

(describe "hammerspoon-monroe-eval-async"
  (it "sends the form with a discard callback on a live connection"
    (let (sent)
      (cl-letf (((symbol-function 'hammerspoon-monroe--repl-buffer)
                 (lambda () (current-buffer)))
                ((symbol-function 'monroe-send-eval-string)
                 (lambda (form callback) (setq sent (list form callback)))))
        (hammerspoon-monroe-eval-async "(print :hi)")
        (expect (car sent) :to-equal "(print :hi)")
        (expect (cadr sent) :to-be #'ignore))))

  (it "silently does nothing when disconnected"
    (let (sent)
      (cl-letf (((symbol-function 'hammerspoon-monroe--repl-buffer) #'ignore)
                ((symbol-function 'monroe-send-eval-string)
                 (lambda (&rest args) (setq sent args))))
        (hammerspoon-monroe-eval-async "(print :hi)")
        (expect sent :to-be nil)))))
