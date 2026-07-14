;;; scripts/install-docsets.el --- batch Dash docset install -*- lexical-binding: t; -*-
;; Port of the doom.d Makefile `dash-docsets' target to the Elpaca layout.
;; Installs the docsets the completion module expects, skipping any already
;; at the latest version.  Runs standalone - no full config boot:
;;   emacs -Q --batch -l scripts/install-docsets.el
;; `bb docsets' drives this.  The config root is derived from this file's
;; location; every elpaca build is put on `load-path' so dash-docs and its
;; deps resolve.

(require 'cl-lib)

(defvar install-docsets-user '("ClojureDocs" "ClojureScript" "Hammerspoon")
  "Unofficial (user-contributed) docsets to install.")

(defvar install-docsets-official '("TypeScript" "CSS" "Python" "Lua")
  "Official Kapeli docsets to install.")

(let* ((script (or load-file-name buffer-file-name))
       (root (file-name-directory (directory-file-name (file-name-directory script))))
       (builds (expand-file-name ".local/elpaca/builds/" root)))
  (unless (file-directory-p builds)
    (error "install-docsets: %s missing - boot the config once first" builds))
  ;; Every elpaca build on load-path so dash-docs + its deps (async,
  ;; format-spec, ...) resolve without pulling in the whole config.
  (let ((default-directory builds))
    (normal-top-level-add-subdirs-to-load-path))
  (unless (require 'dash-docs nil t)
    (error "install-docsets: dash-docs not built - run `bb smoke' or start the config first"))
  ;; The ported ensure-* install helpers and the versioned unofficial-docsets
  ;; override live in the completion module's autoload file.
  (load (expand-file-name "modules/completion/autoload/dash-docs.el" root)))

;; Stock `dash-docs-unofficial-docsets' hits a long-dead endpoint and omits
;; the version field the ensure-* helpers compare against.  The config
;; installs this override via use-package's :config; we boot without that.
(advice-add 'dash-docs-unofficial-docsets :override
            #'dash-docs-unofficial-docsets-versioned)

;; Pre-create the docsets dir so the install helpers never reach the
;; interactive `y-or-n-p' create prompt, which would hang --batch.
(make-directory (dash-docs-docsets-path) t)

(let ((failures 0))
  (cl-flet ((install (installer docset)
              (condition-case err
                  (funcall installer docset)
                (error (cl-incf failures)
                       (message "install-docsets: %s FAILED: %s"
                                docset (error-message-string err))))))
    (dolist (d install-docsets-user) (install #'dash-docs-ensure-user-docset d))
    (dolist (d install-docsets-official) (install #'dash-docs-ensure-docset d)))
  (message "install-docsets: %s -> done (%d failure(s))"
           (dash-docs-docsets-path) failures)
  (kill-emacs (if (zerop failures) 0 1)))
