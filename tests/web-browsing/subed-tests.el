;;; tests/web-browsing/subed-tests.el --- web-browsing/autoload/subed.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/web-browsing/autoload/mpv.el")
(load-module-file "modules/web-browsing/autoload/subed.el")

(defvar subed-tests--srt
  "1\n00:00:01,000 --> 00:00:02,000\nHello world\n\n2\n00:00:03,000 --> 00:00:04,000\nSecond line\n\n")

(defvar subed-tests--vtt
  "WEBVTT\nKind: captions\nLanguage: en\n\n00:00:01.000 --> 00:00:02.000\nHola mundo\n\n00:00:03.000 --> 00:00:04.000\nSegunda linea\n\n")

(defun subed-tests--metadata-overlays ()
  (seq-filter (lambda (ov) (overlay-get ov 'subtitle-metadata))
              (overlays-in (point-min) (point-max))))

(describe "subed-toggle-srt-metadata"
  (it "hides SRT metadata behind invisible overlays and toggles back"
    (with-temp-buffer
      (insert subed-tests--srt)
      (subed-toggle-srt-metadata)
      (let ((ovs (subed-tests--metadata-overlays)))
        ;; 2 per entry: the id+timestamp block and the trailing newline
        (expect (length ovs) :to-equal 4)
        (expect (seq-every-p (lambda (ov) (overlay-get ov 'invisible)) ovs)
                :to-be-truthy))
      (expect subed--subtitle-metadata-hidden :to-be-truthy)
      (subed-toggle-srt-metadata)
      (expect (subed-tests--metadata-overlays) :to-equal nil)
      (expect subed--subtitle-metadata-hidden :to-be nil)))

  (it "skips the WEBVTT header and covers id-less VTT entries"
    (with-temp-buffer
      (insert subed-tests--vtt)
      (subed-toggle-srt-metadata)
      (let ((ovs (subed-tests--metadata-overlays)))
        (expect (length ovs) :to-equal 4)
        ;; header must not be hidden
        (expect (seq-every-p (lambda (ov) (> (overlay-start ov) (length "WEBVTT")))
                             ovs)
                :to-be-truthy)))))

(describe "subed-view-plain-text"
  :var (shown)

  (before-each (setq shown nil))
  (after-each (when (get-buffer "*Subtitle Text Only*")
                (kill-buffer "*Subtitle Text Only*")))

  (it "extracts only the subtitle text from SRT"
    (cl-letf (((symbol-function 'switch-to-buffer-other-window)
               (lambda (buf) (setq shown buf))))
      (with-temp-buffer
        (insert subed-tests--srt)
        (subed-view-plain-text)))
    (expect (buffer-name shown) :to-equal "*Subtitle Text Only*")
    (expect (with-current-buffer shown (buffer-string))
            :to-equal "Hello world\nSecond line"))

  (it "extracts only the subtitle text from VTT"
    (cl-letf (((symbol-function 'switch-to-buffer-other-window)
               (lambda (buf) (setq shown buf))))
      (with-temp-buffer
        (insert subed-tests--vtt)
        (subed-view-plain-text)))
    (expect (with-current-buffer shown (buffer-string))
            :to-equal "Hola mundo\nSegunda linea")))

(defmacro subed-tests--with-player (file-name existing guess &rest body)
  "Run BODY with the player stubbed: the buffer visits FILE-NAME, EXISTING
files exist, subed guesses GUESS.  Every call lands in `calls'."
  (declare (indent 3))
  `(cl-letf (((symbol-function 'subed-mpv-play-from-file)
              (lambda (f) (push (list 'play f) calls)))
             ((symbol-function 'call-interactively)
              (lambda (cmd &rest _) (push (list 'prompt cmd) calls)))
             ((symbol-function 'subed-mpv-unpause)
              (lambda () (push '(unpause) calls)))
             ((symbol-function 'subed-guess-media-file)
              (lambda (&rest _) ,guess))
             ((symbol-function 'buffer-file-name)
              (lambda (&optional _) ,file-name))
             ((symbol-function 'file-exists-p)
              (lambda (f) (member f ,existing))))
     ,@body))

(describe "subed-mpv-play-media"
  :var (calls)

  (before-each (setq calls nil))

  (it "plays an explicit file as given"
    (subed-tests--with-player "/tmp/talk.srt" '("/tmp/talk.mp3") "/tmp/talk.mkv"
      (subed-mpv-play-media "/tmp/other.mp4"))
    (expect (reverse calls)
            :to-equal '((play "/tmp/other.mp4") (unpause))))

  (it "prefers the sibling mp3 of an srt over the video"
    (subed-tests--with-player "/tmp/talk.srt" '("/tmp/talk.mp3") "/tmp/talk.mkv"
      (subed-mpv-play-media))
    (expect (reverse calls)
            :to-equal '((play "/tmp/talk.mp3") (unpause))))

  (it "plays the media subed guesses when no mp3 sits next to the file"
    (subed-tests--with-player "/tmp/talk.vtt" '("/tmp/talk.mp3") "/tmp/talk.mkv"
      (subed-mpv-play-media))
    (expect (reverse calls)
            :to-equal '((play "/tmp/talk.mkv") (unpause))))

  (it "prompts instead of throwing when nothing is next to the file"
    (subed-tests--with-player "/tmp/talk.srt" nil nil
      (subed-mpv-play-media))
    (expect (reverse calls)
            :to-equal '((prompt subed-mpv-play-from-file) (unpause)))))

;;; One player for subed and mpv.el

(defun subed-tests--fake-player (socket)
  "A live process whose command line names SOCKET, like an mpv would."
  (make-process :name "fake-mpv" :noquery t
                :command (list "sh" "-c" "sleep 30" "sh"
                               (concat "--input-ipc-server=" socket))))

(defmacro subed-tests--with-mpv-el-player (socket received &rest body)
  "Run BODY with mpv.el attached to a fake IPC server at SOCKET.
Everything the server receives lands in RECEIVED."
  (declare (indent 2))
  `(let ((server (make-network-process
                  :name "fake-mpv-ipc" :server t :family 'local :service ,socket
                  :noquery t
                  :filter (lambda (_ s) (setq ,received (concat ,received s))))))
     (unwind-protect
         (progn (mpv-connect (subed-tests--fake-player ,socket)) ,@body)
       (ignore-errors (mpv-kill))
       (dolist (p (process-list))
         (when (string-prefix-p "fake-mpv" (process-name p))
           (delete-process p)))
       (delete-process server)
       (ignore-errors (delete-file ,socket)))))

(describe "subed-mpv-attach"
  :var (socket received plays subed-mpv-socket-dir)

  (before-all (require 'subed) (require 'mpv))
  (before-each
    ;; unix socket paths cap at 104 bytes on macOS; the sandbox path plus
    ;; subed's 38-char socket name would overflow it
    (setq subed-mpv-socket-dir (make-temp-file "/tmp/subed-sk" t)
          socket (expand-file-name "mpv-el.sock" test-sandbox-dir)
          received ""
          plays nil)
    (ignore-errors (delete-file socket)))
  (after-each
    (dolist (b (buffer-list))
      (when (string-match-p "subed-mpv-buffer" (buffer-name b))
        (kill-buffer b)))
    (delete-directory subed-mpv-socket-dir t))

  (defun subed-tests--attach (playing)
    "Attach a talk.srt buffer while mpv.el's player reports PLAYING; return the buffer."
    (let ((buf (generate-new-buffer "talk.srt")))
      (with-current-buffer buf
        (setq buffer-file-name "/tmp/talk.srt")
        (cl-letf (((symbol-function 'mpv-get-property)
                   (lambda (prop) (pcase prop ("path" playing) ("pause" :json-false))))
                  ((symbol-function 'mpv-play)
                   (lambda (f) (push f plays))))
          (subed-mpv-attach "/tmp/talk.mkv")))
      buf))

  (it "joins mpv.el's player through a link at subed's own socket path"
    (subed-tests--with-mpv-el-player socket received
      (let ((buf (subed-tests--attach "/tmp/talk.mkv")))
        (unwind-protect
            (with-current-buffer buf
              (expect (file-symlink-p (subed-mpv--socket t)) :to-equal socket)
              (expect (subed-mpv--client-connected-p) :to-be-truthy)
              (expect subed-mpv--server-proc :to-be nil)
              (expect subed-mpv-media-file :to-equal "/tmp/talk.mkv")
              (expect subed-mpv-is-playing :to-be t)
              (expect plays :to-be nil)
              (accept-process-output nil 0.3)
              (expect received :to-match "\"sub-add\",\"/tmp/talk.srt\",\"select\"")
              (expect received :to-match "\"observe_property\",1,\"time-pos\"")
              ;; subed's cleanup drops the link and its client, never mpv.el's side
              (subed-mpv-kill)
              (expect (file-exists-p (subed-mpv--socket t)) :to-be nil)
              (expect (file-exists-p socket) :to-be-truthy)
              (expect (mpv-live-p) :to-be-truthy))
          (kill-buffer buf)))))

  (it "loads the file into the running player when it plays something else"
    (subed-tests--with-mpv-el-player socket received
      (let ((buf (subed-tests--attach "/tmp/other.mp4")))
        (unwind-protect
            (expect plays :to-equal '("/tmp/talk.mkv"))
          (with-current-buffer buf (subed-mpv-kill))
          (kill-buffer buf))))))

(describe "subed-mpv-share-player-a"
  :var (calls)

  (before-all (require 'subed) (require 'mpv))
  (before-each (setq calls nil))

  (defmacro subed-tests--with-play-stubs (&rest body)
    "Run BODY with the player start, the attach and the adoption recorded in `calls'."
    `(cl-letf (((symbol-function 'subed-mpv--play)
                (lambda (f) (push (list 'start f) calls)))
               ((symbol-function 'subed-mpv-attach)
                (lambda (f) (push (list 'attach f) calls)))
               ((symbol-function 'mpv-connect)
                (lambda (proc) (push (list 'adopt proc) calls))))
       ,@body))

  (it "is around subed-mpv-play-from-file, the hook subed offers being dead"
    (expect (advice-member-p 'subed-mpv-share-player-a 'subed-mpv-play-from-file)
            :to-be-truthy))

  (it "joins mpv.el's player instead of starting a second one"
    (let ((mpv--process 'their-player))
      (cl-letf (((symbol-function 'mpv-live-p) (lambda () t)))
        (subed-tests--with-play-stubs
         (with-temp-buffer
           (setq buffer-file-name "/tmp/talk.srt")
           (subed-mpv-play-from-file "/tmp/talk.mkv")
           (setq buffer-file-name nil)))))
    (expect calls :to-equal '((attach "/tmp/talk.mkv"))))

  (it "starts subed's player and lets mpv.el adopt it when mpv.el has none"
    (cl-letf (((symbol-function 'mpv-live-p) (lambda () nil)))
      (subed-tests--with-play-stubs
       (with-temp-buffer
         (setq buffer-file-name "/tmp/talk.srt")
         (setq-local subed-mpv--server-proc 'own-player)
         (subed-mpv-play-from-file "/tmp/talk.mkv")
         (setq buffer-file-name nil))))
    (expect (reverse calls) :to-equal '((start "/tmp/talk.mkv") (adopt own-player))))

  (it "restarts the buffer's own player the subed way when mpv.el runs that one"
    (let ((mpv--process 'own-player))
      (cl-letf (((symbol-function 'mpv-live-p) (lambda () t)))
        (subed-tests--with-play-stubs
         (with-temp-buffer
           (setq buffer-file-name "/tmp/talk.srt")
           (setq-local subed-mpv--server-proc 'own-player)
           (subed-mpv-play-from-file "/tmp/talk.mkv")
           (setq buffer-file-name nil)))))
    (expect (reverse calls) :to-equal '((start "/tmp/talk.mkv") (adopt own-player)))))

(defun subed-tests--use-package-forms (keyword)
  "The KEYWORD forms of the `use-package subed' block in web-browsing/config.el."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "modules/web-browsing/config.el" test-config-root))
    (goto-char (point-min))
    (let (form)
      (while (and (setq form (ignore-errors (read (current-buffer))))
                  (not (and (eq (car-safe form) 'use-package)
                            (eq (cadr form) 'subed)))))
      (expect form :to-be-truthy)
      (use-package-body-forms (cddr form) keyword))))

(describe "subed :init"
  (before-all (require 'doom-defaults) (require 'so-long))

  (it "keeps subtitle files out of the so-long line count"
    (let ((doom-file-lines-threshold-alist '(("." . 5)))
          (buf (get-buffer-create "subed-tests-lines-probe")))
      (eval `(progn ,@(subed-tests--use-package-forms :init)) t)
      (unwind-protect
          (with-current-buffer buf
            (insert (make-string 10 ?\n))
            (dolist (name '("/tmp/talk.srt" "/tmp/talk.en.vtt" "/tmp/talk.ass"))
              (setq buffer-file-name name)
              (expect (doom-so-long-p) :to-be nil))
            (setq buffer-file-name "/tmp/talk.txt")
            (expect (doom-so-long-p) :to-be-truthy))
        (with-current-buffer buf (setq buffer-file-name nil))
        (kill-buffer buf)))))
