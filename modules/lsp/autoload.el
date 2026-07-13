;;; modules/lsp/autoload.el -*- lexical-binding: t; -*-

;; Eager on purpose: the json-parse advice below must be live BEFORE the
;; first lsp server starts - lsp-booster rewrites all server->client JSON
;; into elisp bytecode, and doom.d registered this advice from a lazily
;; loaded autoload file (first completion), leaving the startup window where
;; a booster-wrapped server's output hit an unadvised parser.  Both defuns
;; are verbatim from the emacs-lsp-booster README (blahgeek/emacs-lsp-booster),
;; names kept for recognizability.  The final-command advice registers in
;; config.el's lsp-mode :config; both no-op when the booster binary is absent.

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let* ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
