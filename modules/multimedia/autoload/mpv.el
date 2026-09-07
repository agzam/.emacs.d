;;; modules/multimedia/autoload/mpv.el -*- lexical-binding: t; -*-

;;;###autoload
(defun mpv-speed-reset ()
  (interactive)
  (mpv-speed-set 1))

;;;###autoload
(defun mpv-mute-toggle ()
  (interactive)
  (mpv-run-command "cycle" "mute"))

;;;###autoload
(defun mpv-fullscreen-toggle ()
  (interactive)
  (mpv-run-command "cycle" "fullscreen"))

;;;###autoload
(defun mpv-open (&optional path)
  (interactive)
  (catch 'exit
    (let* ((url-regex "\\`https?://")
           (path (or path
                     (cond
                      ((eq major-mode 'dired-mode)
                       (mpv-play (dired-get-file-for-visit))
                       (throw 'exit nil))

                      ((and (car kill-ring)
                            (string-match url-regex (car kill-ring)))
                       (car kill-ring))

                      ((derived-mode-p 'org-mode)
                       (replace-regexp-in-string
                        "^yt:" "https:"
                        (or
                         (org-element-property :raw-link (org-element-context))
                         (thing-at-point 'url))))

                      (t (thing-at-point 'url))))))
      (unless (eq transient-current-command 'media-transient)
        (transient-setup 'media-transient))
      (mpv-play-url path))))

;;;###autoload
(defun mpv-ipc-socket (process)
  "Return the --input-ipc-server path PROCESS was started with, or nil."
  (seq-some (lambda (arg)
              (and (string-prefix-p "--input-ipc-server=" arg)
                   (substring arg (length "--input-ipc-server="))))
            (process-command process)))

(defun mpv-connected-sentinel (process _event)
  "Forget a connected player once it is gone.
Its starter owns the socket file, so only mpv.el's side is cleaned up."
  (when (and (eq process mpv--process)
             (memq (process-status process) '(exit signal)))
    (mpv-kill)
    (run-hooks 'mpv-on-exit-hook)))

;;;###autoload
(defun mpv-connect (process)
  "Drive PROCESS, an mpv another package started with --input-ipc-server.
mpv.el opens its own client on that socket, so the starter keeps its
connection and its features.  A player mpv.el started itself is killed
first: one player at a time is mpv.el's model.  Return non-nil when the
connection is new."
  (require 'mpv)
  (let ((socket (mpv-ipc-socket process)))
    (unless socket
      (error "%s runs without --input-ipc-server" (process-name process)))
    (unless (eq mpv--process process)
      (mpv-kill)
      ;; mpv creates the socket a moment after it starts; the starter's
      ;; hook may run before that
      (with-timeout (mpv-start-timeout (error "No mpv socket at %s" socket))
        (while (not (file-exists-p socket))
          (sleep-for 0.05)))
      (setq mpv--process process)
      (set-process-query-on-exit-flag process nil)
      (set-process-sentinel process #'mpv-connected-sentinel)
      (setq mpv--queue (tq-create (make-network-process :name "mpv-socket"
                                                        :family 'local
                                                        :service socket)))
      (set-process-filter (tq-process mpv--queue)
                          (lambda (_proc string) (mpv--tq-filter mpv--queue string)))
      (run-hook-with-args 'mpv-on-start-hook nil)
      t)))

(defadvice! mpv-play-next-without-stopping-a (orig-fn arg)
  ;; don't quit mpv, just to play a file/url
  ;; send it into a queue and switch to it immediately
  :around #'mpv-play
  :around #'mpv-play-url
  (if (mpv-live-p)
      (progn
       (mpv--enqueue `(loadfile ,arg append) #'ignore)
       (mpv-run-command "playlist-next"))
    (funcall orig-fn arg)))

(defadvice! message-when-mpv-starts-a (orig-fn &rest args)
  :around #'mpv-play
  :around #'mpv-play-url
  (message "Starting mpv to play %s" (car args))
  (apply orig-fn args))

;; The playback transient lives in media.el (media-transient): one
;; prefix, one keybind, backend-conditional groups (mpv / browser).

(defvar mpv--osc-style "auto")
(defvar mpv--subtitle-visible "auto")

;;;###autoload
(defun mpv-toggle-osc ()
  (interactive)
  ;; get https://github.com/tomasklaen/uosc
  (mpv-run-command "script-message-to" "uosc" "toggle-ui"))

;;;###autoload
(defun mpv-get-path ()
  "Get path/url to current video"
  (interactive)
  (let ((p (mpv-run-command "get_property" "path")))
    (kill-new p)
    (message p)))

;;;###autoload
(defun mpv-toggle-subtitles ()
  (interactive)
  (mpv-run-command
   "set" "sub-visibility"
   (setq mpv--subtitle-visible
         (if (string-match-p "yes\\|auto" mpv--subtitle-visible)
             "no" "yes"))))
