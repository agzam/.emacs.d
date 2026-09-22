;;; modules/web-browsing/autoload/misc.el -*- lexical-binding: t; -*-
(require 'bug-reference)

;;;###autoload
(defun process-external-url (&optional url)
  "Open URL with the handler `embark-url-config' prescribes.
Single source of truth with embark: first matching url-type's RET
action, else the shared (nil-type) RET.  Plain ticket references
\(non-URLs matching `bug-reference-bug-regexp') route through forge."
  (interactive (list (read-string "Enter URL: ")))
  (require 'embark) ; builds `embark-url-patterns' + type keymaps
  (if (string-match-p bug-reference-bug-regexp url)
      (bug-reference-visit-topic url)
    (let* ((type (cl-loop for (type pattern) in embark-url-patterns
                          when (if (functionp pattern)
                                   (funcall pattern url)
                                 (string-match-p pattern url))
                          return type))
           (ret-action (lambda (ty)
                         (cdr (assoc "RET" (plist-get (alist-get ty embark-url-config)
                                                      :actions)))))
           (action (or (funcall ret-action type)
                       (funcall ret-action nil))))
      (funcall action url))))

;;;###autoload
(defun browse-url-externally (url &rest args)
  "Always use default (external) browser."
  (interactive (browse-url-interactive-arg "URL: "))
  ;; eww resets browse-url-function, I don't want that; the handler lists
  ;; win over it anyway, and code-review claims every github /pull/ url there
  (let ((browse-url-browser-function 'browse-url-default-browser)
        (browse-url-handlers nil)
        (browse-url-default-handlers nil))
    (funcall-interactively #'browse-url url args)))
