;;; modules/dired/autoload/rsync.el --- rsync-backed dired renames -*- lexical-binding: t; -*-
;;; Commentary:
;; Big renames block dired for minutes; route them through rsync with
;; progress output instead (advice registered in config.el).
;;; Code:

(defvar dired-rsync-size-threshold (* 50 1024 1024)
  "Use rsync for renames when the payload exceeds this many bytes.")

(defun dired--calculate-size (file)
  "Recursive byte size of FILE."
  (if (file-directory-p file)
      (seq-reduce (lambda (acc f)
                    (+ acc (file-attribute-size (file-attributes f))))
                  (directory-files-recursively file ".*" t)
                  0)
    (file-attribute-size (file-attributes file))))

(defun dired-should-use-rsync-p (files)
  "Non-nil when FILES total more than `dired-rsync-size-threshold'."
  (< dired-rsync-size-threshold
     (seq-reduce (lambda (acc file)
                   (+ acc (dired--calculate-size file)))
                 files 0)))

(defun dired-do-rsync (dest)
  "Rsync marked files to DEST with progress, removing sources."
  (async-shell-command
   (format "rsync -av --progress --remove-source-files %s %s"
           (mapconcat #'shell-quote-argument (dired-get-marked-files) " ")
           (shell-quote-argument dest))))

;;;###autoload
(defun dired-do-rename-wrapper-a (orig-fun &optional arg)
  "Route oversized renames through `dired-do-rsync'."
  (let ((files (dired-get-marked-files nil arg)))
    (if (dired-should-use-rsync-p files)
        (let ((dest (expand-file-name
                     (read-directory-name
                      "Rsync to directory: " (dired-dwim-target-directory)))))
          (dired-do-rsync dest)
          (revert-buffer))
      (funcall orig-fun arg))))

;;; rsync.el ends here
