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
  (let ((model 'claude-sonnet-5))
    (setf (symbol-plist model)
          '(:description "The best combination of speed and intelligence"))
    (list model)))
(defun gptel--process-models (models)
  ;; Mirrors upstream: a (NAME . PLIST) pair overwrites NAME's whole plist.
  (mapcar (lambda (model)
            (if (consp model)
                (progn (setf (symbol-plist (car model)) (cdr model))
                       (car model))
              model))
          models))
(provide 'gptel)
(provide 'gptel-anthropic)
(load-module-file "modules/ai/gptel-anthropic-oauth/gptel-anthropic-oauth.el")

(describe "gptel-anthropic-oauth model discovery"
  (it "converts API models and preserves known metadata"
    (let ((models
           (gptel-anthropic-oauth--model-specs
            '((data . [((id . "claude-sonnet-5")
                        (display_name . "Claude Sonnet 5"))
                       ((id . "claude-new-20260807")
                        (display_name . "Claude New"))])))))
      (expect (mapcar #'car models)
              :to-equal '(claude-sonnet-5 claude-new-20260807))
      (expect (plist-get (cdr (car models)) :description)
              :to-equal "The best combination of speed and intelligence")
      (expect (plist-get (cdr (cadr models)) :description)
              :to-equal "Claude New")))

  (it "accepts unprocessed bundled model metadata"
    (let* ((gptel--anthropic-models
            '((claude-opus-4-8 :description "Most capable model")))
           (models
            (gptel-anthropic-oauth--model-specs
             '((data . [((id . "claude-opus-4-8"))])))))
      (expect (plist-get (cdr (car models)) :description)
              :to-equal "Most capable model")))

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

(describe "gptel-anthropic-oauth--model-version"
  (it "reads major and minor"
    (expect (gptel-anthropic-oauth--model-version 'claude-opus-4-7)
            :to-equal '(4 . 7)))

  (it "defaults a bare major version to minor zero"
    (expect (gptel-anthropic-oauth--model-version 'claude-opus-5)
            :to-equal '(5 . 0)))

  (it "ignores a trailing release date"
    (expect (gptel-anthropic-oauth--model-version 'claude-opus-4-5-20251101)
            :to-equal '(4 . 5)))

  (it "accepts a string as well as a symbol"
    (expect (gptel-anthropic-oauth--model-version "claude-sonnet-4-6")
            :to-equal '(4 . 6)))

  (it "returns nil for an unrecognized name"
    (expect (gptel-anthropic-oauth--model-version 'gpt-5) :to-be nil)))

(describe "gptel-anthropic-oauth--adaptive-thinking-p"
  ;; Every expectation here was measured against api.anthropic.com: the
  ;; rejected models answer `adaptive' with "adaptive thinking is not
  ;; supported on this model".
  (it "accepts models from 4.6 up"
    (dolist (model '(claude-opus-5 claude-sonnet-5 claude-opus-4-8
                     claude-opus-4-7 claude-opus-4-6 claude-sonnet-4-6))
      (expect (gptel-anthropic-oauth--adaptive-thinking-p model)
              :to-be-truthy)))

  (it "rejects models below 4.6"
    (dolist (model '(claude-opus-4-5-20251101 claude-sonnet-4-5-20250929
                     claude-haiku-4-5-20251001 claude-opus-4-20250514))
      (expect (gptel-anthropic-oauth--adaptive-thinking-p model) :to-be nil)))

  (it "rejects an unparsable model name"
    (expect (gptel-anthropic-oauth--adaptive-thinking-p 'mystery-model)
            :to-be nil)))

(describe "gptel-anthropic-oauth--annotate-thinking"
  (it "adds thinking params to an eligible model"
    (let ((models (gptel-anthropic-oauth--annotate-thinking
                   '((claude-opus-5 :description "new")))))
      (expect (plist-get (cdr (car models)) :request-params)
              :to-equal '(:thinking (:type "adaptive" :display "summarized")))))

  (it "keeps the other properties of an annotated model"
    ;; `gptel--process-models' overwrites the whole symbol plist, so dropping
    ;; them here would silently strip every capability.
    (let ((models (gptel-anthropic-oauth--annotate-thinking
                   '((claude-opus-5 :description "new"
                                    :capabilities (media tool-use cache))))))
      (expect (plist-get (cdr (car models)) :description) :to-equal "new")
      (expect (plist-get (cdr (car models)) :capabilities)
              :to-equal '(media tool-use cache))))

  (it "leaves an ineligible model untouched"
    (expect (gptel-anthropic-oauth--annotate-thinking
             '((claude-sonnet-4-5-20250929 :description "old")))
            :to-equal '((claude-sonnet-4-5-20250929 :description "old"))))

  (it "annotates a bare symbol, carrying its existing plist"
    (let* ((model 'claude-opus-4-7-test)
           (_ (setf (symbol-plist model) '(:description "kept")))
           (models (gptel-anthropic-oauth--annotate-thinking (list model))))
      (expect (plist-get (cdr (car models)) :description) :to-equal "kept")
      (expect (plist-get (cdr (car models)) :request-params) :to-be-truthy)))

  (it "does not mutate the plist it was given"
    (let* ((spec (list (cons 'claude-opus-5 (list :description "new"))))
           (_ (gptel-anthropic-oauth--annotate-thinking spec)))
      (expect (plist-get (cdr (car spec)) :request-params) :to-be nil)))

  (it "reaches the models of a newly built backend"
    (let ((backend (gptel-make-anthropic-oauth
                    "OAuth-thinking-test"
                    :models '((claude-opus-5 :description "new")
                              (claude-sonnet-4-5-20250929 :description "old")))))
      (unwind-protect
          (progn
            (expect (get 'claude-opus-5 :request-params)
                    :to-equal '(:thinking (:type "adaptive" :display "summarized")))
            (expect (get 'claude-sonnet-4-5-20250929 :request-params) :to-be nil)
            (expect (gptel-backend-models backend)
                    :to-equal '(claude-opus-5 claude-sonnet-4-5-20250929)))
        (setf (symbol-plist 'claude-opus-5) nil)
        (setf (symbol-plist 'claude-sonnet-4-5-20250929) nil)
        (setq gptel--known-backends
              (cl-remove-if
               (lambda (entry) (equal (car entry) "OAuth-thinking-test"))
               gptel--known-backends))))))
