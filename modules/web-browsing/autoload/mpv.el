;;; modules/web-browsing/autoload/mpv.el -*- lexical-binding: t; -*-

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
