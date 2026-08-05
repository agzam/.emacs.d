;;; tests/web-browsing/media-tests.el --- web-browsing/autoload/media.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'transient)
(require 'cl-lib)

(load-module-file "modules/web-browsing/autoload/media.el")

(defun media-tests--layout-keys (node)
  "Collect (COMMAND . KEY) suffix pairs from a transient layout NODE.
Same dual-dialect traversal as `transient-layout-commands'."
  (cond
   ((vectorp node)
    (mapcan #'media-tests--layout-keys (append node nil)))
   ((proper-list-p node)
    (let ((pl (if (keywordp (car node)) node (cdr node))))
      (if-let* ((cmd (plist-get pl :command)))
          (list (cons cmd (plist-get pl :key)))
        (unless (keywordp (car node))
          (mapcan #'media-tests--layout-keys node)))))))

(describe "media-backend-active"
  (it "honors an explicit backend"
    (let ((media-backend 'browser))
      (expect (media-backend-active) :to-be 'browser))
    (let ((media-backend 'mpv))
      (expect (media-backend-active) :to-be 'mpv)))

  (it "auto picks mpv while an mpv process is live"
    (let ((media-backend 'auto))
      (with-fake-feature 'mpv
        (cl-letf (((symbol-function 'mpv-live-p) (lambda () t)))
          (expect (media-backend-active) :to-be 'mpv)))))

  (it "auto falls to the browser lane on macOS when mpv is dead"
    (let ((media-backend 'auto)
          (system-type 'darwin))
      (with-fake-feature 'mpv
        (cl-letf (((symbol-function 'mpv-live-p) (lambda () nil)))
          (expect (media-backend-active) :to-be 'browser)))))

  (it "auto falls to the browser lane on macOS when mpv never loaded"
    (let ((media-backend 'auto)
          (system-type 'darwin))
      (expect (media-backend-active) :to-be 'browser)))

  (it "auto stays on mpv on Linux until the MPRIS lane lands"
    (let ((media-backend 'auto)
          (system-type 'gnu/linux))
      (expect (media-backend-active) :to-be 'mpv))))

(describe "media-transient layout"
  :var* ((cmds (transient-layout-commands
                (get 'media-transient 'transient--layout))))

  (it "pins both backend suffix sets"
    (expect cmds :to-have-same-items-as
            '(;; mpv group - the historical mpv-transient set + mute/fullscreen
              mpv-volume-increase mpv-volume-decrease mpv-mute-toggle
              mpv-playlist-prev mpv-playlist-next
              mpv-seek-backward mpv-seek-forward
              mpv-toggle-osc mpv-get-path mpv-toggle-subtitles
              mpv-speed-decrease mpv-speed-increase mpv-speed-reset
              mpv-fullscreen-toggle mpv-pause mpv-open mpv-kill
              ;; browser group
              navegosa-media-volume-up navegosa-media-volume-down
              navegosa-media-mute-toggle
              navegosa-media-prev navegosa-media-next
              navegosa-media-seek-backward navegosa-media-seek-forward
              navegosa-media-theater-toggle navegosa-media-copy-url
              navegosa-media-subs-toggle
              navegosa-media-speed-down navegosa-media-speed-up
              navegosa-media-speed-reset
              navegosa-media-fullscreen-toggle navegosa-media-play-pause
              navegosa-media-select-tab media-open
              navegosa-media-status)))

  (it "keeps one muscle-memory key set across backends"
    (let ((pairs (media-tests--layout-keys
                  (get 'media-transient 'transient--layout)))
          (mirror '((mpv-pause . navegosa-media-play-pause)
                    (mpv-seek-backward . navegosa-media-seek-backward)
                    (mpv-seek-forward . navegosa-media-seek-forward)
                    (mpv-speed-decrease . navegosa-media-speed-down)
                    (mpv-speed-increase . navegosa-media-speed-up)
                    (mpv-speed-reset . navegosa-media-speed-reset)
                    (mpv-volume-increase . navegosa-media-volume-up)
                    (mpv-volume-decrease . navegosa-media-volume-down)
                    (mpv-mute-toggle . navegosa-media-mute-toggle)
                    (mpv-toggle-subtitles . navegosa-media-subs-toggle)
                    (mpv-playlist-prev . navegosa-media-prev)
                    (mpv-playlist-next . navegosa-media-next)
                    (mpv-get-path . navegosa-media-copy-url)
                    (mpv-fullscreen-toggle . navegosa-media-fullscreen-toggle)
                    (mpv-open . media-open))))
      (dolist (pair mirror)
        (let ((mpv-key (cdr (assq (car pair) pairs)))
              (browser-key (cdr (assq (cdr pair) pairs))))
          (expect mpv-key :to-be-truthy)
          (expect browser-key :to-equal mpv-key)))))

  (it "pulls the shared bypass engine itself"
    ;; media.el must (require 'transient-bypass): this suite loads only
    ;; media.el, so a dropped require would leave transient-bypass-keys void.
    (expect (fboundp 'transient-bypass-keys) :to-be-truthy)
    (expect (transient-bypass-keys 'media-transient '(("M-x"))) :to-be-truthy)))

(describe "media-url-at-point"
  (it "normalizes org yt: links at point"
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "[[yt://www.youtube.com/watch?v=q][vid]]")
      (goto-char 3)
      (expect (media-url-at-point)
              :to-equal "https://www.youtube.com/watch?v=q")))

  (it "falls back to the kill-ring head when it is a URL"
    (with-temp-buffer
      (let ((kill-ring '("https://youtu.be/xyz")))
        (expect (media-url-at-point) :to-equal "https://youtu.be/xyz"))))

  (it "ignores non-URL kill-ring content"
    (with-temp-buffer
      (let ((kill-ring '("not a url")))
        (expect (media-url-at-point) :to-be nil)))))

(describe "media-open"
  (it "sends URLs to the browser lane when it is the active backend"
    (let ((media-backend 'browser))
      (spy-on 'navegosa-media-open-url)
      (spy-on 'mpv-open)
      (with-temp-buffer
        (let ((kill-ring '("https://youtu.be/xyz")))
          (media-open)))
      (expect 'navegosa-media-open-url :to-have-been-called-with
              "https://youtu.be/xyz")
      (expect 'mpv-open :not :to-have-been-called)))

  (it "delegates to mpv-open when mpv is the active backend"
    (let ((media-backend 'mpv))
      (spy-on 'navegosa-media-open-url)
      (spy-on 'mpv-open)
      (media-open)
      (expect 'mpv-open :to-have-been-called)
      (expect 'navegosa-media-open-url :not :to-have-been-called)))

  (it "keeps local files on mpv even on the browser backend"
    (let ((media-backend 'browser))
      (spy-on 'navegosa-media-open-url)
      (spy-on 'mpv-open)
      (with-temp-buffer
        (setq major-mode 'dired-mode)
        (media-open))
      (expect 'mpv-open :to-have-been-called)
      (expect 'navegosa-media-open-url :not :to-have-been-called)))

  (it "errors when no URL can be resolved"
    (let ((media-backend 'browser))
      (spy-on 'navegosa-media-open-url)
      (with-temp-buffer
        (let ((kill-ring '("not a url")))
          (expect (media-open) :to-throw 'user-error))))))
