;;; tests/multimedia/mpv-tests.el --- multimedia/autoload/mpv.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'transient)

;; Loading registers advice against not-yet-defined mpv functions; that is
;; the boot-time behavior too (advice takes effect when mpv loads).
(load-module-file "modules/multimedia/autoload/mpv.el")

(describe "mpv.el after the transient moved to media.el"
  (it "no longer defines mpv-transient"
    ;; the playback transient is media-transient (media-tests.el); a
    ;; resurrected local prefix here would shadow it silently
    (expect (get 'mpv-transient 'transient--layout) :to-be nil)
    (expect (fboundp 'mpv-transient) :to-be nil))

  (it "keeps the mpv-side helpers the unified transient binds"
    (expect (fboundp 'mpv-mute-toggle) :to-be-truthy)
    (expect (fboundp 'mpv-fullscreen-toggle) :to-be-truthy)
    (expect (fboundp 'mpv-speed-reset) :to-be-truthy)))

(defun mpv-tests--fake-player (socket)
  "A live process whose command line names SOCKET, like an mpv would."
  (make-process :name "fake-mpv" :noquery t
                :command (list "sh" "-c" "sleep 30" "sh"
                               (concat "--input-ipc-server=" socket))))

(defmacro mpv-tests--with-ipc-server (socket received &rest body)
  "Run BODY with a local socket server at SOCKET collecting input in RECEIVED."
  (declare (indent 2))
  `(let ((server (make-network-process
                  :name "fake-mpv-ipc" :server t :family 'local :service ,socket
                  :noquery t
                  :filter (lambda (_ s) (setq ,received (concat ,received s))))))
     (unwind-protect (progn ,@body)
       (delete-process server)
       (ignore-errors (delete-file ,socket)))))

(describe "mpv-connect"
  :var (socket received exits)

  (before-all (require 'mpv))
  (before-each
    (setq socket (expand-file-name "mpv-connect.sock" test-sandbox-dir)
          received ""
          exits 0)
    (ignore-errors (delete-file socket)))
  (after-each
    (ignore-errors (mpv-kill))
    (dolist (p (process-list))
      (when (string-prefix-p "fake-mpv" (process-name p))
        (delete-process p))))

  (it "reads the ipc socket off the player's command line"
    (let ((player (mpv-tests--fake-player "/tmp/some.sock"))
          (plain (make-process :name "fake-mpv-plain" :command '("sleep" "30") :noquery t)))
      (expect (mpv-ipc-socket player) :to-equal "/tmp/some.sock")
      (expect (mpv-ipc-socket plain) :to-be nil)
      (expect (mpv-connect plain) :to-throw 'error)))

  (it "adopts the player: its commands reach the socket, and it counts as live"
    (mpv-tests--with-ipc-server socket received
      (let ((player (mpv-tests--fake-player socket))
            (mpv-on-start-hook (list (lambda (_) (setq exits 'started)))))
        (expect (mpv-connect player) :to-be-truthy)
        (expect mpv--process :to-be player)
        (expect (mpv-live-p) :to-be-truthy)
        (expect exits :to-be 'started)
        (mpv-pause)
        (accept-process-output nil 0.3)
        (expect received :to-match "\"cycle\",\"pause\""))))

  (it "leaves the socket to the starter and waits for it to appear"
    (let ((player (mpv-tests--fake-player socket))
          (mpv-start-timeout 0.2))
      ;; no server yet: nothing to connect to, and mpv.el stays detached
      (expect (mpv-connect player) :to-throw 'error)
      (expect mpv--process :to-be nil)
      (expect (process-live-p player) :to-be-truthy)))

  (it "kills the player mpv.el started itself, once"
    (mpv-tests--with-ipc-server socket received
      (let ((own (make-process :name "fake-mpv-own" :command '("sleep" "30") :noquery t))
            (player (mpv-tests--fake-player socket)))
        (setq mpv--process own)
        (mpv-connect player)
        (expect (process-live-p own) :to-be nil)
        (expect mpv--process :to-be player)
        ;; a second connect to the same player is a no-op
        (let ((queue mpv--queue))
          (expect (mpv-connect player) :to-be nil)
          (expect mpv--queue :to-be queue)))))

  (it "asks the player to quit before killing it"
    ;; mpv.el kills with SIGKILL; only a quit request lets mpv's own
    ;; scripts run their shutdown handlers (the window-frame save)
    (mpv-tests--with-ipc-server socket received
      (let ((player (mpv-tests--fake-player socket)))
        (mpv-connect player)
        (mpv-kill)
        (expect received :to-match "\"quit\"")
        (expect (process-live-p player) :to-be nil)
        (expect (mpv-live-p) :to-be nil))))

  (it "detaches when the player exits, running the exit hook"
    (mpv-tests--with-ipc-server socket received
      (let ((player (mpv-tests--fake-player socket))
            (mpv-on-exit-hook (list (lambda () (cl-incf exits)))))
        (mpv-connect player)
        (kill-process player)
        (with-timeout (2 (error "player did not exit"))
          (while mpv--process (accept-process-output nil 0.05)))
        (expect mpv--process :to-be nil)
        (expect mpv--queue :to-be nil)
        (expect exits :to-equal 1)))))

(describe "mpv-open"
  :var (calls)

  (before-each (setq calls nil))

  (it "plays an explicit path directly"
    (cl-letf (((symbol-function 'mpv-play-url)
               (lambda (url) (push (list 'play-url url) calls)))
              ((symbol-function 'transient-setup)
               (lambda (&rest args) (push (cons 'setup args) calls))))
      (let ((transient-current-command nil))
        (mpv-open "https://vid.example/1")))
    (expect (reverse calls)
            :to-equal '((setup media-transient) (play-url "https://vid.example/1"))))

  (it "resolves a url from the kill-ring"
    (cl-letf (((symbol-function 'mpv-play-url)
               (lambda (url) (push (list 'play-url url) calls)))
              ((symbol-function 'transient-setup)
               (lambda (&rest args) (push (cons 'setup args) calls))))
      (with-temp-buffer
        (let ((kill-ring '("https://youtu.be/xyz"))
              (transient-current-command nil))
          (mpv-open))))
    (expect calls :to-contain '(play-url "https://youtu.be/xyz")))

  (it "plays the file at point in dired and skips the transient"
    (cl-letf (((symbol-function 'mpv-play)
               (lambda (f) (push (list 'play f) calls)))
              ((symbol-function 'mpv-play-url)
               (lambda (url) (push (list 'play-url url) calls)))
              ((symbol-function 'transient-setup)
               (lambda (&rest args) (push (cons 'setup args) calls)))
              ((symbol-function 'dired-get-file-for-visit)
               (lambda () "/tmp/vid.mp4")))
      (with-temp-buffer
        (setq major-mode 'dired-mode)
        (let ((transient-current-command nil))
          (mpv-open))))
    (expect calls :to-equal '((play "/tmp/vid.mp4"))))

  (it "resolves org yt: links at point"
    (cl-letf (((symbol-function 'mpv-play-url)
               (lambda (url) (push (list 'play-url url) calls)))
              ((symbol-function 'transient-setup)
               (lambda (&rest args) (push (cons 'setup args) calls))))
      (with-temp-buffer
        (delay-mode-hooks (org-mode))
        (insert "[[yt://www.youtube.com/watch?v=q][vid]]")
        (goto-char 3)
        (let ((kill-ring '("not a url"))
              (transient-current-command nil))
          (mpv-open))))
    (expect calls :to-contain
            '(play-url "https://www.youtube.com/watch?v=q"))))

(describe "mpv-report-load-failure"
  :var (saved-event saved-exit)
  (before-all (require 'mpv))
  (before-each
    (setq saved-event mpv-on-event-hook
          saved-exit mpv-on-exit-hook)
    (spy-on 'message))
  (after-each
    (setq mpv-on-event-hook saved-event
          mpv-on-exit-hook saved-exit))

  (it "says why mpv could not load the url, once"
    (mpv-report-load-failure "https://youtu.be/x")
    (run-hook-with-args 'mpv-on-event-hook
                        '((event . "end-file") (reason . "error")
                          (file_error . "loading failed")))
    (run-hooks 'mpv-on-exit-hook)
    (expect 'message :to-have-been-called-with
            "mpv could not play %s: %s" "https://youtu.be/x" "loading failed")
    (expect (spy-calls-count 'message) :to-equal 1)
    (expect mpv-on-event-hook :to-equal saved-event)
    (expect mpv-on-exit-hook :to-equal saved-exit))

  (it "says so when mpv quits before playing"
    (mpv-report-load-failure "https://youtu.be/x")
    (run-hooks 'mpv-on-exit-hook)
    (expect 'message :to-have-been-called-with
            "mpv quit before it played %s" "https://youtu.be/x"))

  (it "stays quiet once the file loads, and through the files after it"
    (mpv-report-load-failure "https://youtu.be/x")
    (run-hook-with-args 'mpv-on-event-hook '((event . "end-file") (reason . "stop")))
    (run-hook-with-args 'mpv-on-event-hook '((event . "file-loaded")))
    (run-hook-with-args 'mpv-on-event-hook '((event . "end-file") (reason . "error")))
    (run-hooks 'mpv-on-exit-hook)
    (expect 'message :not :to-have-been-called)
    (expect mpv-on-event-hook :to-equal saved-event)))

(describe "mpv-play-url-at"
  (before-all (require 'mpv))
  (before-each (spy-on 'mpv-report-load-failure))

  (it "starts mpv at the position and speed"
    (spy-on 'mpv-live-p :and-return-value nil)
    (spy-on 'mpv-start)
    (mpv-play-url-at "https://youtu.be/x" 754.3 1.5)
    (expect 'mpv-start :to-have-been-called-with
            "--start=754.300" "--speed=1.5" "https://youtu.be/x")
    (expect 'mpv-report-load-failure :to-have-been-called-with "https://youtu.be/x"))

  (it "plays at normal speed when none is given"
    (spy-on 'mpv-live-p :and-return-value nil)
    (spy-on 'mpv-start)
    (mpv-play-url-at "https://youtu.be/x" 0)
    (expect 'mpv-start :to-have-been-called-with
            "--start=0.000" "--speed=1" "https://youtu.be/x"))

  (it "plays next in a running mpv and keeps its playlist"
    (spy-on 'mpv-live-p :and-return-value t)
    (spy-on 'mpv-start)
    (spy-on 'mpv-run-command)
    (spy-on 'mpv-set-property)
    (mpv-play-url-at "https://youtu.be/x" 12 1.25)
    ;; insert-next-play would start only an idle player
    (expect (spy-calls-all-args 'mpv-run-command)
            :to-equal '(("loadfile" "https://youtu.be/x" "insert-next" "-1" "start=12.000")
                        ("playlist-next")))
    (expect 'mpv-set-property :to-have-been-called-with "speed" 1.25)
    (expect 'mpv-report-load-failure :to-have-been-called-with "https://youtu.be/x")
    (expect 'mpv-start :not :to-have-been-called)))
