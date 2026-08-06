;;; modules/web-browsing/autoload/media.el -*- lexical-binding: t; -*-

;; The transient's bypass section needs the shared engine at layout-build
;; time; requiring it here keeps the prefix self-contained (the engine
;; move once surfaced transients that leaned on expreg.el loading first).
(require 'transient-bypass)

(defcustom media-backend 'auto
  "Which playback backend `media-transient' controls.
`auto' picks mpv while an mpv process is live, otherwise the
platform's browser lane (navegosa JXA on macOS, navegosa MPRIS on
Linux).  Set to `mpv', `browser', or `mpris' to pin one."
  :type '(choice (const auto) (const mpv) (const browser) (const mpris))
  :group 'multimedia)

(defun media-backend-active ()
  "Resolve `media-backend', mapping `auto' to a concrete backend."
  (cond
   ((not (eq media-backend 'auto)) media-backend)
   ((and (featurep 'mpv) (mpv-live-p)) 'mpv)
   ((eq system-type 'darwin) 'browser)
   ((and (eq system-type 'gnu/linux) (featurep 'dbusbind)) 'mpris)
   (t 'mpv)))

(defun media-backend-choices ()
  "Backends offerable here: auto, mpv, and this platform's browser lane."
  (append '(auto mpv)
          (cond
           ((eq system-type 'darwin) '(browser))
           ((and (eq system-type 'gnu/linux) (featurep 'dbusbind)) '(mpris)))))

(defun media-backend-description ()
  "Menu label for the backend switch: the pin, plus its resolution when auto."
  (if (eq media-backend 'auto)
      (format "backend: auto(%s)" (media-backend-active))
    (format "backend: %s" media-backend)))

;;;###autoload
(defun media-backend-switch ()
  "Cycle `media-backend' among auto, mpv, and this platform's browser lane.
Session-only re-pin (plain `setq', nothing persisted).  Inside
`media-transient' the prefix's `:refresh-suffixes' re-evaluates the
group `:if' predicates, so the visible group follows the new backend."
  (interactive)
  (let* ((choices (media-backend-choices))
         (old media-backend)
         (new (or (cadr (memq old choices)) (car choices))))
    (setq media-backend new)
    (message "backend: %s%s (was %s)"
             new
             (if (eq new 'auto) (format " = %s" (media-backend-active)) "")
             old)))

(defun media-url-at-point ()
  "Resolve a media URL: org link at point, URL at point, else kill-ring.
Mirrors `mpv-open's resolution chain (minus dired); org yt: links
are normalized to https."
  (let ((url-regex "\\`https?://"))
    (cond
     ((derived-mode-p 'org-mode)
      (replace-regexp-in-string
       "^yt:" "https:"
       (or (org-element-property :raw-link (org-element-context))
           (thing-at-point 'url)
           "")))
     ((thing-at-point 'url))
     ((and (car kill-ring)
           (string-match url-regex (car kill-ring)))
      (car kill-ring)))))

;;;###autoload
(defun media-open (&optional url)
  "Open URL, or the media at point / in the kill-ring, in the active backend.
An explicit URL (e.g. from a link follow handler) bypasses at-point
resolution on both lanes.  Local files (dired) always go to mpv;
URLs go to the browser lane (JXA or MPRIS) or mpv per
`media-backend'."
  (interactive)
  (cond
   ((or (and (null url) (eq major-mode 'dired-mode))
        (eq (media-backend-active) 'mpv))
    (mpv-open url))
   (t
    (let ((url (or url (media-url-at-point))))
      (when (or (null url) (string-empty-p url))
        (user-error "No media URL at point or in the kill-ring"))
      (navegosa-media-open-url url)))))

;;;###autoload
(transient-define-prefix media-transient ()
  "Playback control; backend resolved via `media-backend'."
  ;; re-create suffix objects after every press: group :if predicates
  ;; re-evaluate, so a backend hop (b) flips the visible group in place
  :refresh-suffixes t
  ["bypass keys"
   :class transient-column
   :hide always
   :setup-children
   (lambda (_)
     (transient-bypass-keys
      'media-transient
      '(("M-x")
        ("d" t dired-flag-file-deletion)
        ("j" t evil-next-visual-line)
        ("k" t evil-previous-visual-line))))]
  ["mpv"
   :if (lambda () (eq (media-backend-active) 'mpv))
   [("K" "vol up" mpv-volume-increase :transient t)
    ("J" "vol down" mpv-volume-decrease :transient t)
    ("m" "mute" mpv-mute-toggle :transient t)]
   [("p" "prev" mpv-playlist-prev :transient t)
    ("n" "next" mpv-playlist-next :transient t)]
   [("h" "<<" mpv-seek-backward :transient t)
    ("l" ">>" mpv-seek-forward :transient t)]
   [("i" "osc" mpv-toggle-osc :transient t)
    ("y" "get path" mpv-get-path :transient t)
    ("c" "subs" mpv-toggle-subtitles :transient t)]
   [("," "slower" mpv-speed-decrease :transient t)
    ("." "faster" mpv-speed-increase :transient t)
    ("0" "reset" mpv-speed-reset)]
   [("f" "fullscreen" mpv-fullscreen-toggle :transient t)
    ("SPC" "pause" mpv-pause :transient t)]
   [("o" "play" mpv-open :transient t)
    ("Q" "quit" mpv-kill)
    ("b" media-backend-switch :description media-backend-description
     :transient t)]]
  ["browser"
   :if (lambda () (eq (media-backend-active) 'browser))
   [("K" "vol up" navegosa-media-volume-up :transient t)
    ("J" "vol down" navegosa-media-volume-down :transient t)
    ("m" "mute" navegosa-media-mute-toggle :transient t)]
   [("p" "prev" navegosa-media-prev :transient t)
    ("n" "next" navegosa-media-next :transient t)]
   [("h" "<<" navegosa-media-seek-backward :transient t)
    ("l" ">>" navegosa-media-seek-forward :transient t)]
   [("t" "theater" navegosa-media-theater-toggle :transient t)
    ("y" "copy url" navegosa-media-copy-url :transient t)
    ("c" "subs" navegosa-media-subs-toggle :transient t)]
   [("," "slower" navegosa-media-speed-down :transient t)
    ("." "faster" navegosa-media-speed-up :transient t)
    ("0" "reset" navegosa-media-speed-reset)]
   [("f" "fullscreen" navegosa-media-fullscreen-toggle :transient t)
    ("SPC" "pause" navegosa-media-play-pause :transient t)]
   [("s" "pick tab" navegosa-media-select-tab :transient t)
    ("o" "open" media-open :transient t)
    ("g" "status" navegosa-media-status :transient t)
    ("b" media-backend-switch :description media-backend-description
     :transient t)]]
  ;; same keys as the browser group: what MPRIS cannot do (speed, volume,
  ;; mute, subs, theater, fullscreen) answers with an honest user-error
  ["mpris"
   :if (lambda () (eq (media-backend-active) 'mpris))
   [("K" "vol up" navegosa-media-volume-up :transient t)
    ("J" "vol down" navegosa-media-volume-down :transient t)
    ("m" "mute" navegosa-media-mute-toggle :transient t)]
   [("p" "prev" navegosa-media-prev :transient t)
    ("n" "next" navegosa-media-next :transient t)]
   [("h" "<<" navegosa-media-seek-backward :transient t)
    ("l" ">>" navegosa-media-seek-forward :transient t)]
   [("t" "theater" navegosa-media-theater-toggle :transient t)
    ("y" "copy url" navegosa-media-copy-url :transient t)
    ("c" "subs" navegosa-media-subs-toggle :transient t)]
   [("," "slower" navegosa-media-speed-down :transient t)
    ("." "faster" navegosa-media-speed-up :transient t)
    ("0" "reset" navegosa-media-speed-reset)]
   [("f" "fullscreen" navegosa-media-fullscreen-toggle :transient t)
    ("SPC" "pause" navegosa-media-play-pause :transient t)]
   [("s" "pick player" navegosa-media-select-tab :transient t)
    ("o" "open" media-open :transient t)
    ("g" "status" navegosa-media-status :transient t)
    ("b" media-backend-switch :description media-backend-description
     :transient t)]])
