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

(describe "mpv-transient layout"
  :var* ((cmds (transient-layout-commands
                (get 'mpv-transient 'transient--layout))))

  (it "carries no elfeed-tube residue after the drop"
    (expect (seq-filter (lambda (c) (string-match-p "elfeed" (symbol-name c)))
                        cmds)
            :to-equal nil))

  (it "pins the slimmed suffix set, mpv-open included"
    (expect cmds :to-have-same-items-as
            '(mpv-volume-increase mpv-volume-decrease
              mpv-playlist-prev mpv-playlist-next
              mpv-seek-backward mpv-seek-forward
              mpv-toggle-osc mpv-get-path mpv-toggle-subtitles
              mpv-speed-decrease mpv-speed-increase mpv-speed-reset
              mpv-pause mpv-open mpv-kill))))

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
            :to-equal '((setup mpv-transient) (play-url "https://vid.example/1"))))

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
