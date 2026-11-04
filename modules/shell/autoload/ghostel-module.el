;;; modules/shell/autoload/ghostel-module.el --- ghostel's native module, fetched in the background -*- lexical-binding: t; -*-

(require 'ghostel-module-install)

;;;###autoload
(defun fetch-ghostel-module ()
  "Download ghostel's native module in the background unless one is loaded."
  (unless (featurep 'ghostel-module)
    (let ((version ghostel--minimum-module-version))
      (message "ghostel: downloading native module %s" version)
      (url-retrieve (ghostel--module-download-url version) #'install-ghostel-module
                    (list (ghostel--module-directory) version) t t))))

(defun install-ghostel-module (status dir version)
  "Save the module VERSION this download holds into DIR, then load it.
STATUS is the plist `url-retrieve' hands its callback."
  (unwind-protect
      (if-let* ((err (plist-get status :error)))
          (message "ghostel: native module download failed: %S" err)
        (let* ((module (expand-file-name (concat "ghostel-module" module-file-suffix) dir))
               (part (make-temp-name (concat module ".part.")))
               (coding-system-for-write 'binary))
          (make-directory dir t)
          (goto-char (point-min))
          (re-search-forward "\r?\n\r?\n")
          (write-region (point) (point-max) part nil 'silent)
          (set-file-modes part #o755)
          (rename-file part module t))
        (ghostel--write-module-sidecar-version dir version)
        (ghostel--load-module)
        (message "ghostel: native module %s installed" version))
    (kill-buffer)))

;;; ghostel-module.el ends here
