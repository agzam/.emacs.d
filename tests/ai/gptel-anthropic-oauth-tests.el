;;; tests/ai/gptel-anthropic-oauth-tests.el --- OAuth gptel specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

;; Keep these tests independent of the asynchronously installed gptel package.
(cl-defstruct (gptel-backend (:constructor gptel--make-backend))
  name host header protocol stream endpoint key models url request-params curl-args)
(cl-defstruct (gptel-anthropic (:include gptel-backend)
                               (:constructor gptel--make-anthropic)))
(cl-defgeneric gptel--request-data (backend prompts))
(defvar gptel--known-backends nil)
(defconst gptel--anthropic-models
  '((claude-opus-4-8
     :description "Most capable model for complex reasoning and advanced coding")))
(defun gptel--process-models (models)
  (mapcar (lambda (model)
            (if (consp model) (car model) model))
          models))
(provide 'gptel)
(provide 'gptel-anthropic)
(load-module-file "modules/ai/gptel-anthropic-oauth/gptel-anthropic-oauth.el")

(describe "gptel-anthropic-oauth model discovery"
  (it "converts API models and preserves known metadata"
    (let ((models
           (gptel-anthropic-oauth--model-specs
            '((data . [((id . "claude-opus-4-8")
                        (display_name . "Claude Opus 4.8"))
                       ((id . "claude-new-20260807")
                        (display_name . "Claude New"))])))))
      (expect (mapcar #'car models)
              :to-equal '(claude-opus-4-8 claude-new-20260807))
      (expect (plist-get (cdr (car models)) :description)
              :to-equal "Most capable model for complex reasoning and advanced coding")
      (expect (plist-get (cdr (cadr models)) :description)
              :to-equal "Claude New")))

  (it "updates every registered OAuth backend"
    (let ((backend (gptel-make-anthropic-oauth
                    "OAuth-test" :models '(claude-old))))
      (unwind-protect
          (progn
            (gptel-anthropic-oauth--refresh-backends
             '((claude-new :description "new")))
            (expect (gptel-backend-models backend)
                    :to-equal '(claude-new)))
        (setq gptel--known-backends
              (cl-remove-if
               (lambda (entry) (equal (car entry) "OAuth-test"))
               gptel--known-backends))))))
