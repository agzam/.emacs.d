;;; tests/shell/ghostel-module-tests.el --- shell/autoload/ghostel-module.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; ghostel is not installed in the batch tier; this stand-in carries the
;; `ghostel-module-install' entry points the fetch calls.
(defconst ghostel--minimum-module-version "0.60.0")
(defun ghostel--module-directory ())
(defun ghostel--module-download-url (&optional _version))
(defun ghostel--write-module-sidecar-version (_dir _version))
(defun ghostel--load-module (&optional _prompt-user))
(provide 'ghostel-module-install)

(load-module-file "modules/shell/autoload/ghostel-module.el")

(defun ghostel-module-tests--response (head body)
  "A unibyte buffer holding the HTTP response HEAD followed by BODY."
  (let ((buf (generate-new-buffer " *ghostel-module-response*")))
    (with-current-buffer buf
      (set-buffer-multibyte nil)
      (insert head body))
    buf))

(describe "fetch-ghostel-module"
  (before-each
    (spy-on 'message)
    (spy-on 'url-retrieve)
    (spy-on 'ghostel--module-directory :and-return-value "/tmp/ghostel/")
    (spy-on 'ghostel--module-download-url :and-call-fake
            (lambda (version) (format "https://example.invalid/v%s/module" version))))

  (it "downloads the minimum module version when none is loaded"
    (fetch-ghostel-module)
    (expect 'url-retrieve :to-have-been-called-with
            "https://example.invalid/v0.60.0/module" #'install-ghostel-module
            '("/tmp/ghostel/" "0.60.0") t t))

  (it "does nothing once a module is loaded"
    (with-fake-feature 'ghostel-module
      (fetch-ghostel-module))
    (expect 'url-retrieve :not :to-have-been-called)))

(describe "install-ghostel-module"
  :var (root dir module on-disk-at-load)
  (before-each
    ;; DIR does not exist yet, the way it is gone after a wiped .local
    (setq root (make-temp-file "ghostel-module" t)
          dir (expand-file-name "ghostel/" root)
          module (expand-file-name (concat "ghostel-module" module-file-suffix) dir)
          on-disk-at-load nil)
    (spy-on 'message)
    (spy-on 'ghostel--write-module-sidecar-version)
    (spy-on 'ghostel--load-module :and-call-fake
            (lambda (&optional _) (setq on-disk-at-load (file-exists-p module)))))
  (after-each
    (delete-directory root t))

  (it "saves the body byte for byte as an executable module, then loads it"
    ;; the body repeats the header terminator, so only the first may split
    (let* ((body (unibyte-string #x7f ?E ?L ?F 13 10 13 10 0 #xff))
           (buf (ghostel-module-tests--response
                 "HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\n" body)))
      (with-current-buffer buf
        (install-ghostel-module '(:redirect "https://example.invalid/asset")
                                dir "0.60.0"))
      (expect (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert-file-contents-literally module)
                (buffer-string))
              :to-equal body)
      (expect (file-executable-p module) :to-be t)
      (expect (directory-files dir nil "\\.part\\.") :to-equal nil)
      (expect 'ghostel--write-module-sidecar-version
              :to-have-been-called-with dir "0.60.0")
      (expect on-disk-at-load :to-be t)
      (expect (buffer-live-p buf) :to-be nil)))

  (it "writes and loads nothing when the download failed"
    (let ((buf (ghostel-module-tests--response
                "HTTP/1.1 404 Not Found\r\n\r\n" "Not Found")))
      (with-current-buffer buf
        (install-ghostel-module '(:error (error http 404)) dir "0.60.0"))
      (expect (file-exists-p dir) :to-be nil)
      (expect 'ghostel--write-module-sidecar-version :not :to-have-been-called)
      (expect 'ghostel--load-module :not :to-have-been-called)
      (expect (buffer-live-p buf) :to-be nil))))

;;; ghostel-module-tests.el ends here
