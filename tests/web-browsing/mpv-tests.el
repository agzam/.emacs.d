;;; tests/web-browsing/mpv-tests.el --- web-browsing/autoload/mpv.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'transient)

;; Loading registers advice against not-yet-defined mpv functions; that is
;; the boot-time behavior too (advice takes effect when mpv loads).
(load-module-file "modules/web-browsing/autoload/mpv.el")

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
