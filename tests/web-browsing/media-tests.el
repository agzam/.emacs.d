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

(defun media-tests--group-plist (node description)
  "Find the group plist carrying DESCRIPTION anywhere in layout NODE.
Both transient layout dialects park a group's plist as a keyword-headed
list among the node's elements - 0.7.x at slot 2 of a [LEVEL CLASS PLIST
CHILDREN] vector, 0.13.x at slot 1 of [CLASS PLIST CHILDREN] - so a
uniform descent dodges the slot arithmetic."
  (cond
   ((vectorp node)
    (seq-some (lambda (child) (media-tests--group-plist child description))
              (append node nil)))
   ((proper-list-p node)
    (if (keywordp (car node))
        (and (equal (plist-get node :description) description) node)
      (seq-some (lambda (child) (media-tests--group-plist child description))
                node)))))

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
      (expect (media-backend-active) :to-be 'mpv))
    (let ((media-backend 'mpris))
      (expect (media-backend-active) :to-be 'mpris)))

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

  ;; featurep is spied, not features let-rebound: featurep reads the C-side
  ;; feature list and ignores a let-binding of the Lisp variable
  (it "auto picks MPRIS on Linux when Emacs has D-Bus"
    (let ((media-backend 'auto)
          (system-type 'gnu/linux))
      (spy-on 'featurep :and-call-fake (lambda (f &rest _) (eq f 'dbusbind)))
      (expect (media-backend-active) :to-be 'mpris)))

  (it "auto stays on mpv on a Linux build without D-Bus"
    (let ((media-backend 'auto)
          (system-type 'gnu/linux))
      (spy-on 'featurep :and-return-value nil)
      (expect (media-backend-active) :to-be 'mpv))))

(describe "media-backend-switch"
  (it "offers only the platform's own browser lane"
    (let ((system-type 'darwin))
      (expect (media-backend-choices) :to-equal '(auto mpv browser)))
    (let ((system-type 'gnu/linux))
      (spy-on 'featurep :and-call-fake (lambda (f &rest _) (eq f 'dbusbind)))
      (expect (media-backend-choices) :to-equal '(auto mpv mpris))))

  (it "cycles auto -> mpv -> mpris -> auto on Linux"
    (let ((media-backend 'auto)
          (system-type 'gnu/linux))
      (spy-on 'featurep :and-call-fake (lambda (f &rest _) (eq f 'dbusbind)))
      (media-backend-switch)
      (expect media-backend :to-be 'mpv)
      (media-backend-switch)
      (expect media-backend :to-be 'mpris)
      (media-backend-switch)
      (expect media-backend :to-be 'auto)))

  (it "cycles into browser, never mpris, on macOS"
    (let ((media-backend 'mpv)
          (system-type 'darwin))
      (media-backend-switch)
      (expect media-backend :to-be 'browser)))

  (it "skips the browser lane on a Linux build without D-Bus"
    (let ((media-backend 'mpv)
          (system-type 'gnu/linux))
      (spy-on 'featurep :and-return-value nil)
      (media-backend-switch)
      (expect media-backend :to-be 'auto)))

  (it "recovers to auto when pinned to the foreign lane"
    ;; a hand-setopt 'browser on Linux is outside the cycle: next press
    ;; must land on a lane that exists here, not error or stick
    (let ((media-backend 'browser)
          (system-type 'gnu/linux))
      (spy-on 'featurep :and-call-fake (lambda (f &rest _) (eq f 'dbusbind)))
      (media-backend-switch)
      (expect media-backend :to-be 'auto)))

  (it "re-pins with plain setq, never the customize machinery"
    (let ((media-backend 'auto)
          (system-type 'darwin))
      (spy-on 'featurep :and-return-value nil)
      (spy-on 'customize-save-variable)
      (spy-on 'customize-set-variable)
      (media-backend-switch)
      (expect 'customize-save-variable :not :to-have-been-called)
      (expect 'customize-set-variable :not :to-have-been-called)
      (expect (get 'media-backend 'saved-value) :to-be nil)))

  (it "labels the switch with the pin, plus its resolution when auto"
    (spy-on 'featurep :and-return-value nil)
    (let ((media-backend 'auto)
          (system-type 'darwin))
      (expect (media-backend-description) :to-equal "backend: auto(browser)"))
    (let ((media-backend 'mpris))
      (expect (media-backend-description) :to-equal "backend: mpris"))))

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
              navegosa-media-status
              ;; the mpris group re-binds the same navegosa-media-*
              ;; commands - same set, no additions
              ;; the backend switch is shared by all three groups
              media-backend-switch)))

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

  (it "re-binds every shared command to the same key in every group"
    ;; the browser and mpris groups bind the same navegosa-media-*
    ;; commands: a command appearing in several groups must keep one key
    (let ((pairs (media-tests--layout-keys
                  (get 'media-transient 'transient--layout)))
          (seen (make-hash-table :test #'eq)))
      (dolist (pair pairs)
        (when-let* ((prev (gethash (car pair) seen)))
          (expect (cdr pair) :to-equal prev))
        (puthash (car pair) (cdr pair) seen))))

  (it "puts the backend switch on b in every group"
    ;; one per group: a backend hop must keep b under the finger
    (let* ((pairs (media-tests--layout-keys
                   (get 'media-transient 'transient--layout)))
           (keys (mapcar #'cdr (seq-filter
                                (lambda (pair)
                                  (eq (car pair) 'media-backend-switch))
                                pairs))))
      (expect keys :to-equal '("b" "b" "b"))))

  (it "re-inits suffixes after every press so a backend hop redraws"
    ;; group :if predicates evaluate at (re)init time: without
    ;; refresh-suffixes the visible group could not flip in place
    (expect (oref (get 'media-transient 'transient--prefix) refresh-suffixes)
            :to-be-truthy))

  (it "digs the group plist out of both layout dialects"
    ;; shapes lifted from transient--parse-group: v0.7.2 is Emacs 30's
    ;; bundled copy (what CI runs), the other the local elpaca build -
    ;; the mpris gate below must keep passing on both
    (let ((v07-layout '([1 transient-columns (:description "mpris" :if ignore)
                         ((1 transient-suffix (:key "x" :command ignore)))]))
          (v013-layout [transient-prefix nil
                        ([transient-columns (:description "mpris" :if ignore)
                          ([transient-column nil ()])])]))
      (expect (plist-get (media-tests--group-plist v07-layout "mpris") :if)
              :to-be 'ignore)
      (expect (plist-get (media-tests--group-plist v013-layout "mpris") :if)
              :to-be 'ignore)))

  (it "gates the mpris group on the resolved backend"
    (let ((pred (plist-get (media-tests--group-plist
                            (get 'media-transient 'transient--layout) "mpris")
                           :if)))
      (expect pred :to-be-truthy)
      (let ((media-backend 'mpris))
        (expect (funcall pred) :to-be-truthy))
      (let ((media-backend 'browser))
        (expect (funcall pred) :to-be nil))))

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

  (it "resolves a plain URL at point outside org-mode"
    ;; the contract embark's yt-video actions lean on: point lands on
    ;; the target, thing-at-point must win before the kill-ring
    (with-temp-buffer
      (let ((kill-ring '("https://youtu.be/stale")))
        (insert "see https://youtu.be/xyz here")
        (search-backward "youtu")
        (expect (media-url-at-point) :to-equal "https://youtu.be/xyz"))))

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

  (it "sends URLs to the same lane on the mpris backend"
    ;; navegosa-media-open-url does the browse-url fallback itself there
    (let ((media-backend 'mpris))
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

  ;; the explicit-URL specs pin the org yt: handler contract: what the
  ;; follow handler passes in must win over any at-point/kill-ring state
  (it "prefers an explicit URL over the kill-ring on the browser lane"
    (let ((media-backend 'browser))
      (spy-on 'navegosa-media-open-url)
      (spy-on 'mpv-open)
      (with-temp-buffer
        (let ((kill-ring '("https://youtu.be/stale")))
          (media-open "https://youtu.be/xyz")))
      (expect 'navegosa-media-open-url :to-have-been-called-with
              "https://youtu.be/xyz")
      (expect 'mpv-open :not :to-have-been-called)))

  (it "threads an explicit URL through to mpv"
    (let ((media-backend 'mpv))
      (spy-on 'navegosa-media-open-url)
      (spy-on 'mpv-open)
      (with-temp-buffer
        (let ((kill-ring '("https://youtu.be/stale")))
          (media-open "https://youtu.be/xyz")))
      (expect 'mpv-open :to-have-been-called-with "https://youtu.be/xyz")
      (expect 'navegosa-media-open-url :not :to-have-been-called)))

  (it "sends an explicit URL to the browser lane even from dired"
    (let ((media-backend 'browser))
      (spy-on 'navegosa-media-open-url)
      (spy-on 'mpv-open)
      (with-temp-buffer
        (setq major-mode 'dired-mode)
        (media-open "https://youtu.be/xyz"))
      (expect 'navegosa-media-open-url :to-have-been-called-with
              "https://youtu.be/xyz")
      (expect 'mpv-open :not :to-have-been-called)))

  (it "errors when no URL can be resolved"
    (let ((media-backend 'browser))
      (spy-on 'navegosa-media-open-url)
      (with-temp-buffer
        (let ((kill-ring '("not a url")))
          (expect (media-open) :to-throw 'user-error))))))
