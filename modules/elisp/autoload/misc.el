;;; modules/elisp/autoload/misc.el --- time/symbol/info helpers -*- lexical-binding: t; -*-
;;; Commentary:
;; Rot fixed on port: datetime->timestamp passed a raw number to `message'
;; (wrong-type error) and multiplied `time-convert' ticks whose HZ varies -
;; float-time math now.  elisp-fully-qualified-name passed a hardcoded
;; symbol to find-function-library (copy-paste slip) - uses the symbol at
;; point, falling back to the visited file's stem.
;;; Code:

;;;###autoload
(defun datetime->timestamp (&optional date-string)
  "Convert DATE-STRING (default: word at point) to Unix milliseconds."
  (interactive)
  (let ((date-str (or date-string (word-at-point))))
    (message "%d" (truncate (* 1000 (float-time (date-to-time date-str)))))))

;;;###autoload
(defun timestamp->datetime (&optional timestamp)
  "Convert Unix TIMESTAMP in milliseconds (default: word at point) to a datetime."
  (interactive)
  (let ((ts (or timestamp (string-to-number (word-at-point)))))
    (message "%s"
             (format-time-string
              "%Y-%m-%d %H:%M:%S"
              (seconds-to-time
               (string-to-number (substring (number-to-string ts) 0 10)))))))

;;;###autoload
(defun elisp-fully-qualified-name ()
  "Fully qualified ns/name of the symbol at point."
  (when-let* ((sym (symbol-at-point))
              (file (or (ignore-errors (cdr (find-function-library sym)))
                        buffer-file-name))
              (ns (file-name-sans-extension (file-name-nondirectory file))))
    (format "%s/%s" ns sym)))

;;;###autoload
(defun elisp-fully-qualified-symbol-with-gh-link (&optional main-branch?)
  "Kill a markdown link to the symbol at point on GitHub.
With MAIN-BRANCH? prefix, pin the link to the main branch."
  (interactive "P")
  (require 'git-link)  ; the branch let-binding below must be dynamic
  (when-let* ((url (let ((git-link-default-branch
                          (when main-branch? (magit-main-branch))))
                     (git-link-kill)))
              (symbol (elisp-fully-qualified-name))
              (link (format "[%s](%s)" symbol url)))
    (message "%s" link)
    (kill-new link)
    link))

;;;###autoload
(defun info-copy-node-url ()
  "Copy the current Info node's URL to the kill ring."
  (interactive)
  (unless (derived-mode-p 'Info-mode)
    (error "Not in Info mode"))
  (let* ((manual (file-name-sans-extension
                  (file-name-nondirectory Info-current-file)))
         (url (Info-url-for-node (format "(%s)%s" manual Info-current-node))))
    (kill-new url)
    (message "Copied: %s" url)))

;;; misc.el ends here
