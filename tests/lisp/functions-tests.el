;;; tests/lisp/functions-tests.el --- lisp/functions.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'transient)

(load-module-file "lisp/functions.el")

(describe "system-dist-name"
  (it "prefixes the macOS product version"
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_) "15.5\n")))
      (let ((system-type 'darwin))
        (expect (system-dist-name) :to-equal "macOS 15.5"))))
  (it "returns the bare distro name on linux"
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_) "Arch\n")))
      (let ((system-type 'gnu/linux))
        (expect (system-dist-name) :to-equal "Arch")))))

(defvar active-modules)

(describe "reload-config"
  (it "re-runs the init.el layers in order, then processes elpaca queues"
    ;; let*: the hook lambdas must lexically capture `calls'
    (let* ((calls nil)
           (active-modules '(mod-a mod-b))
           (custom-file "/probe/custom.el")
           (doom-before-reload-hook (list (lambda () (push '(hook before) calls))))
           (doom-after-reload-hook (list (lambda () (push '(hook after) calls)))))
      (cl-letf (((symbol-function 'load)
                 (lambda (file &rest _)
                   (push (list 'load (file-name-nondirectory file)) calls)))
                ((symbol-function 'load-module)
                 (lambda (name) (push (list 'module name) calls)))
                ((symbol-function 'elpaca-process-queues)
                 (lambda () (push '(elpaca) calls)))
                ((symbol-function 'message) #'ignore))
        (reload-config))
      (expect (nreverse calls) :to-equal
              '((hook before)
                (load "doom-defaults") (load "functions")
                (module mod-a) (module mod-b)
                (load "config") (load "custom.el")
                (elpaca)
                (hook after)))))
  (it "skips custom-file when unset"
    (let ((calls nil)
          (active-modules nil)
          (custom-file nil)
          (doom-before-reload-hook nil)
          (doom-after-reload-hook nil))
      (cl-letf (((symbol-function 'load)
                 (lambda (file &rest _)
                   (push (file-name-nondirectory file) calls)))
                ((symbol-function 'load-module) #'ignore)
                ((symbol-function 'elpaca-process-queues) #'ignore)
                ((symbol-function 'message) #'ignore))
        (reload-config))
      (expect (nreverse calls) :to-equal '("doom-defaults" "functions" "config")))))

(describe "display-buffer-in-quadrant"
  (it "creates a window for a new buffer and reuses it on redisplay"
    (let ((buf (generate-new-buffer "quadrant-probe")))
      (unwind-protect
          (progn
            (delete-other-windows)
            (let ((w1 (display-buffer-in-quadrant
                       buf '((direction . right)))))
              (expect (window-live-p w1) :to-be-truthy)
              (expect (window-buffer w1) :to-be buf)
              ;; second display finds the existing window, no new split
              (let ((count (length (window-list))))
                (display-buffer-in-quadrant buf '((direction . right)))
                (expect (length (window-list)) :to-equal count))))
        (delete-other-windows)
        (kill-buffer buf)))))

(describe "transient-remap-suffix-key"
  ;; Two throwaway prefixes mirror gptel's two keying shapes: a key defined on
  ;; the suffix itself (like `gptel--suffix-send') and a key defined inline in
  ;; the layout (like gptel-tools' confirm).  transient is built-in, so no
  ;; package is needed here.
  (before-each
    (transient-define-suffix probe-suffix-on-def ()
      :key "RET" :description "send"
      (interactive))
    (transient-define-prefix probe-menu-inherited ()
      "probe" [(probe-suffix-on-def)])
    (transient-define-prefix probe-menu-in-layout ()
      "probe" [("RET" "confirm" ignore)]))
  (after-each
    (dolist (s '(probe-suffix-on-def probe-menu-inherited probe-menu-in-layout
                 probe-menu-no-ret))
      (when (fboundp s) (fmakunbound s))
      (put s 'transient--prefix nil)
      (put s 'transient--layout nil)))

  (it "remaps whether the key lives on the suffix or in the layout"
    (transient-remap-suffix-key 'probe-menu-inherited "RET" "s-<return>")
    (transient-remap-suffix-key 'probe-menu-in-layout "RET" "s-<return>")
    (expect (ignore-errors (transient-get-suffix 'probe-menu-inherited "s-<return>"))
            :to-be-truthy)
    (expect (ignore-errors (transient-get-suffix 'probe-menu-in-layout "s-<return>"))
            :to-be-truthy)
    (expect (ignore-errors (transient-get-suffix 'probe-menu-inherited "RET")) :to-be nil)
    (expect (ignore-errors (transient-get-suffix 'probe-menu-in-layout "RET")) :to-be nil))

  (it "is idempotent - a second run (as reload-config triggers) does not error"
    (transient-remap-suffix-key 'probe-menu-inherited "RET" "s-<return>")
    (expect (transient-remap-suffix-key 'probe-menu-inherited "RET" "s-<return>")
            :not :to-throw)
    (expect (ignore-errors (transient-get-suffix 'probe-menu-inherited "s-<return>"))
            :to-be-truthy))

  (it "no-ops on a prefix without the FROM key"
    (transient-define-prefix probe-menu-no-ret ()
      "probe" [("x" "x" ignore)])
    (expect (transient-remap-suffix-key 'probe-menu-no-ret "RET" "s-<return>")
            :not :to-throw)))

(describe "transient-set-suffix-key"
  ;; Mirrors gptel-menu's "m"/"-m" exchange: the selector keys itself on the
  ;; suffix definition, the switch keys itself in the layout.  The switch's
  ;; command is the one transient generates for the argument, "m".
  (before-each
    (transient-define-suffix probe-suffix-provider ()
      :key "-m" :description "provider"
      (interactive))
    (transient-define-prefix probe-swap-menu ()
      "probe"
      [(probe-suffix-provider)
       ("m" "minibuffer" "m")]))
  (after-each
    (dolist (s '(probe-suffix-provider probe-swap-menu transient:probe-swap-menu:m))
      (when (fboundp s) (fmakunbound s))
      (put s 'transient--prefix nil)
      (put s 'transient--layout nil)
      (put s 'transient--suffix nil)))

  (cl-flet ((command-on (key)
              (plist-get (cdr (transient-get-suffix 'probe-swap-menu key))
                         :command))
            (swap ()
              (transient-set-suffix-key 'probe-swap-menu 'transient:probe-swap-menu:m "-m")
              (transient-set-suffix-key 'probe-swap-menu 'probe-suffix-provider "m")))

    (it "exchanges two keys, wherever each key is defined"
      (swap)
      (expect (command-on "m") :to-be 'probe-suffix-provider)
      (expect (command-on "-m") :to-be 'transient:probe-swap-menu:m))

    (it "is idempotent - a second run (as reload-config triggers) keeps the keys"
      (swap)
      (swap)
      (expect (command-on "m") :to-be 'probe-suffix-provider)
      (expect (command-on "-m") :to-be 'transient:probe-swap-menu:m))

    (it "no-ops on a prefix without that command"
      (expect (transient-set-suffix-key 'probe-swap-menu 'ignore "z") :not :to-throw)
      (expect (ignore-errors (transient-get-suffix 'probe-swap-menu "z")) :to-be nil))))

(describe "build-dir-half-built-p"
  ;; A link-only build dir (interrupted between elpaca's link and
  ;; autoloads/compile steps) is the state that silently voids commands.
  (let ((dir nil))
    (before-each (setq dir (make-temp-file "half-built-probe" t)))
    (after-each (delete-directory dir t))

    (it "flags a dir holding only sources"
      (with-temp-file (expand-file-name "pkg.el" dir))
      (expect (build-dir-half-built-p dir "pkg-autoloads.el") :to-be-truthy))

    (it "passes a dir with the autoloads file"
      (with-temp-file (expand-file-name "pkg-autoloads.el" dir))
      (expect (build-dir-half-built-p dir "pkg-autoloads.el") :to-be nil))

    (it "passes a dir with compiled files but a custom autoloads name (org)"
      (with-temp-file (expand-file-name "pkg.elc" dir))
      (expect (build-dir-half-built-p dir "pkg-autoloads.el") :to-be nil))

    (it "flags a dangling autoloads symlink despite real compiled files"
      ;; relocated sources tree (renamed repo/config dir, moved CI
      ;; workspace): absolute symlink targets all sever, .elc stay real
      (with-temp-file (expand-file-name "pkg.elc" dir))
      (make-symbolic-link (expand-file-name "gone/pkg-autoloads.el" dir)
                          (expand-file-name "pkg-autoloads.el" dir))
      (expect (build-dir-half-built-p dir "pkg-autoloads.el") :to-be-truthy))

    (it "passes a live autoloads symlink (the normal linked build)"
      (with-temp-file (expand-file-name "target.el" dir))
      (make-symbolic-link (expand-file-name "target.el" dir)
                          (expand-file-name "pkg-autoloads.el" dir))
      (expect (build-dir-half-built-p dir "pkg-autoloads.el") :to-be nil))

    (it "passes a missing dir (package not yet built at all)"
      (expect (build-dir-half-built-p
               (expand-file-name "nonexistent" dir) "pkg-autoloads.el")
              :to-be nil))))

(describe "stale-elc-files"
  (let ((dir nil))
    (before-each (setq dir (make-temp-file "stale-elc-probe" t)))
    (after-each (delete-directory dir t))

    (it "flags an elc older than its source"
      (let ((el (expand-file-name "pkg.el" dir))
            (elc (expand-file-name "pkg.elc" dir))
            (now (current-time)))
        (with-temp-file elc)
        (with-temp-file el)
        (set-file-times elc (time-subtract now 100))
        (set-file-times el now)
        (expect (stale-elc-files dir) :to-equal (list elc))))

    (it "passes a fresh elc"
      (let ((el (expand-file-name "pkg.el" dir))
            (elc (expand-file-name "pkg.elc" dir))
            (now (current-time)))
        (with-temp-file el)
        (with-temp-file elc)
        (set-file-times el (time-subtract now 100))
        (set-file-times elc now)
        (expect (stale-elc-files dir) :to-be nil)))

    (it "ignores an elc with no sibling source"
      (with-temp-file (expand-file-name "pkg.elc" dir))
      (expect (stale-elc-files dir) :to-be nil))))

(describe "stale-elpaca-builds"
  (it "flags stale builds inside the builds dir, skips build-in-place ones"
    (let* ((builds-root (make-temp-file "stale-builds-root" t))
           (pkg-dir (expand-file-name "pkg" builds-root))
           (local-dir (make-temp-file "stale-local-checkout" t))
           (now (current-time)))
      (unwind-protect
          (progn
            (make-directory pkg-dir)
            (dolist (dir (list pkg-dir local-dir))
              (let ((el (expand-file-name "f.el" dir))
                    (elc (expand-file-name "f.elc" dir)))
                (with-temp-file elc)
                (with-temp-file el)
                (set-file-times elc (time-subtract now 100))
                (set-file-times el now)))
            (cl-letf (((symbol-function 'elpaca--queued)
                       (lambda () `((pkg . (:build-dir ,pkg-dir))
                                    (local . (:build-dir ,local-dir)))))
                      ((symbol-function 'elpaca<-build-dir)
                       (lambda (e) (plist-get e :build-dir))))
              (defvar elpaca-builds-directory)
              (let ((elpaca-builds-directory builds-root))
                (expect (stale-elpaca-builds)
                        :to-equal `((pkg . (,(expand-file-name "f.elc" pkg-dir))))))))
        (delete-directory builds-root t)
        (delete-directory local-dir t)))))

(describe "broken-elpaca-builds"
  (it "merges both scans with their reasons"
    (cl-letf (((symbol-function 'half-built-elpaca-packages)
               (lambda () '((vulpea . "/b/vulpea"))))
              ((symbol-function 'stale-elpaca-builds)
               (lambda () '((magit-section "/b/magit-section/f.elc")))))
      (expect (broken-elpaca-builds)
              :to-equal '((vulpea . half-built) (magit-section . stale)))))

  (it "adds the dirty scan when elpaca-local is loaded, with exemptions"
    ;; entries: vulpea already flagged (dedup), remoto build-in-place local
    ;; (skipped), org custom :autoloads (skipped), consult custom :build
    ;; (skipped), forge genuinely dirty (flagged), magit clean
    (let ((queued '((vulpea . (:recipe nil :dirty t))
                    (remoto . (:recipe nil :local t :dirty t))
                    (org . (:recipe (:autoloads "org-loaddefs.el") :dirty t))
                    (consult . (:recipe (:build (:not x)) :dirty t))
                    (forge . (:recipe nil :dirty t))
                    (magit . (:recipe nil)))))
      (cl-letf (((symbol-function 'half-built-elpaca-packages)
                 (lambda () '((vulpea . "/b/vulpea"))))
                ((symbol-function 'stale-elpaca-builds) #'ignore)
                ((symbol-function 'elpaca--queued) (lambda () queued))
                ((symbol-function 'elpaca<-recipe)
                 (lambda (e) (plist-get e :recipe)))
                ((symbol-function 'elpaca-local-package-p)
                 (lambda (e) (plist-get e :local)))
                ((symbol-function 'elpaca-local-build-dirty-p)
                 (lambda (e) (plist-get e :dirty))))
        (expect (broken-elpaca-builds)
                :to-equal '((vulpea . half-built) (forge . dirty)))))))

(describe "rebuild-broken-elpaca-builds"
  (it "queues a rebuild per broken package, kicks the queue once, reports each"
    (let (rebuilt kicked emitted)
      (cl-letf (((symbol-function 'half-built-elpaca-packages)
                 (lambda () '((vulpea . "/b/vulpea"))))
                ((symbol-function 'stale-elpaca-builds)
                 (lambda () '((magit-section "/b/f.elc"))))
                ((symbol-function 'elpaca-rebuild)
                 (lambda (id) (push id rebuilt)))
                ((symbol-function 'elpaca-process-queues)
                 (lambda () (setq kicked t))))
        (expect (rebuild-broken-elpaca-builds
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) emitted)))
                :to-equal '((vulpea . half-built) (magit-section . stale)))
        (expect (nreverse rebuilt) :to-equal '(vulpea magit-section))
        (expect kicked :to-be-truthy)
        (expect (nreverse emitted)
                :to-equal '("rebuild (half-built): vulpea"
                            "rebuild (stale): magit-section")))))

  (it "does nothing on a healthy tree"
    (let (rebuilt kicked)
      (cl-letf (((symbol-function 'half-built-elpaca-packages) #'ignore)
                ((symbol-function 'stale-elpaca-builds) #'ignore)
                ((symbol-function 'elpaca-rebuild)
                 (lambda (id) (push id rebuilt)))
                ((symbol-function 'elpaca-process-queues)
                 (lambda () (setq kicked t))))
        (expect (rebuild-broken-elpaca-builds) :to-be nil)
        (expect rebuilt :to-be nil)
        (expect kicked :to-be nil)))))

(describe "half-built-elpaca-packages"
  (it "returns nil when elpaca is absent"
    (expect (fboundp 'elpaca--queued) :to-be nil)
    (expect (half-built-elpaca-packages) :to-be nil))

  (it "flags only broken entries, honoring the recipe :autoloads override"
    (let* ((broken-dir (make-temp-file "half-built-broken" t))
           (org-dir (make-temp-file "half-built-org" t))
           (healthy-dir (make-temp-file "half-built-ok" t))
           ;; fake elpaca structs as plists; accessors stubbed below
           (queued `((vulpea . (:build-dir ,broken-dir :recipe nil :package "vulpea"))
                     (org . (:build-dir ,org-dir
                             :recipe (:autoloads "org-loaddefs.el") :package "org"))
                     (evil . (:build-dir ,healthy-dir :recipe nil :package "evil")))))
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name "vulpea.el" broken-dir))
            (with-temp-file (expand-file-name "org-loaddefs.el" org-dir))
            (with-temp-file (expand-file-name "evil-autoloads.el" healthy-dir))
            (cl-letf (((symbol-function 'elpaca--queued) (lambda () queued))
                      ((symbol-function 'elpaca<-build-dir)
                       (lambda (e) (plist-get e :build-dir)))
                      ((symbol-function 'elpaca<-recipe)
                       (lambda (e) (plist-get e :recipe)))
                      ((symbol-function 'elpaca<-package)
                       (lambda (e) (plist-get e :package))))
              (expect (half-built-elpaca-packages)
                      :to-equal `((vulpea . ,broken-dir)))))
        (dolist (d (list broken-dir org-dir healthy-dir))
          (delete-directory d t))))))
