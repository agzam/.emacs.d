;;; tests/ai/gptel-copilot-tests.el --- ai/autoload/gptel-copilot.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)
(require 'cl-lib)

;; gptel is absent from the batch tier; stub the parts the module touches.
(cl-defstruct (gptel-backend (:constructor gptel--make-backend))
  name host header protocol stream endpoint key models url request-params curl-args)
(cl-defstruct (gptel-openai (:include gptel-backend)
                            (:constructor gptel--make-openai)))
(cl-defstruct (gptel--gh (:include gptel-openai)
                         (:constructor gptel--make-gh))
  token github-token sessionid machineid responses-backend)
;; Special, so the module binds it dynamically, yet unbound: other suites
;; read a bound `gptel-backend' as a loaded gptel.
(defvar gptel-backend nil)
(makunbound 'gptel-backend)
(defvar gptel--known-backends nil)
(defvar gptel-gh-github-token-file "/nonexistent/github-token")
(defvar gptel--gh-models
  '((copilot-test-known :description "Bundled description"
                        :capabilities (url responses-api)
                        :input-cost 5 :output-cost 30 :cutoff-date "2026-02")))
(defun gptel-oauth--read-token (_file) nil)
(defun gptel--gh-auth () nil)
(defun gptel--process-models (models)
  ;; Mirrors upstream: a (NAME . PLIST) pair overwrites NAME's whole plist.
  (mapcar (lambda (model)
            (if (consp model)
                (progn (setf (symbol-plist (car model)) (cdr model))
                       (car model))
              model))
          models))
(provide 'gptel)
(provide 'gptel-gh)
(load-module-file "modules/ai/autoload/gptel-copilot.el")

(defun copilot-tests--entry (id &rest props)
  "A Copilot catalog entry for ID; PROPS override its default fields."
  (append props
          (list :id id :name (upcase id)
                :model_picker_enabled t
                :supported_endpoints '("/chat/completions")
                :capabilities '(:limits (:max_context_window_tokens 200000
                                         :vision (:supported_media_types ("image/png")))
                                :supports (:tool_calls t :vision t)))))

(defun copilot-tests--response (status body)
  "A buffer shaped like the one `url-retrieve-synchronously' returns."
  (with-current-buffer (generate-new-buffer " *copilot-tests-response*")
    (insert (format "HTTP/1.1 %d X\n\n" status))
    (setq-local url-http-end-of-headers (point-marker))
    (setq-local url-http-response-status status)
    (insert body)
    (current-buffer)))

(describe "gptel-copilot--model-specs"
  (it "drops the entries Copilot's own picker hides"
    (expect (mapcar #'car
                    (gptel-copilot--model-specs
                     (list :data
                           (list (copilot-tests--entry "shown")
                                 (copilot-tests--entry "internal"
                                                       :model_picker_enabled :json-false)
                                 (copilot-tests--entry "off-plan"
                                                       :policy '(:state "disabled"))))))
            :to-equal '(shown)))

  (it "keeps an entry waiting for consent or without a picker flag"
    ;; an unconfigured policy works once the terms are accepted
    (expect (mapcar #'car
                    (gptel-copilot--model-specs
                     (list :data
                           (list (copilot-tests--entry "consent"
                                                       :policy '(:state "unconfigured"))
                                 (copilot-tests--entry "unflagged"
                                                       :model_picker_enabled nil)))))
            :to-equal '(consent unflagged))))

(describe "gptel-copilot--model-spec"
  (it "routes a model served only on /responses through the responses API"
    (expect (plist-get (cdr (gptel-copilot--model-spec
                             (copilot-tests--entry
                              "resp" :supported_endpoints '("/responses" "ws:/responses"))))
                       :capabilities)
            :to-contain 'responses-api))

  (it "keeps a /chat/completions model off the responses API"
    (expect (plist-get (cdr (gptel-copilot--model-spec
                             (copilot-tests--entry
                              "chat" :supported_endpoints '("/v1/messages" "/chat/completions"))))
                       :capabilities)
            :not :to-contain 'responses-api))

  (it "reads media, tools and structured output from the supports flags"
    (let ((spec (cdr (gptel-copilot--model-spec
                      (copilot-tests--entry
                       "full" :capabilities
                       '(:limits (:max_context_window_tokens 400000
                                  :vision (:supported_media_types
                                           ("image/png" "application/pdf")))
                         :supports (:tool_calls t :vision t :structured_outputs t)))))))
      (expect (plist-get spec :capabilities) :to-equal '(media tool-use json))
      (expect (plist-get spec :mime-types) :to-equal '("image/png" "application/pdf"))
      (expect (plist-get spec :context-window) :to-be 400)))

  (it "claims nothing a false flag denies"
    ;; JSON false parses to :json-false, which is non-nil
    (let ((spec (cdr (gptel-copilot--model-spec
                      (copilot-tests--entry
                       "bare" :capabilities
                       '(:supports (:tool_calls :json-false :vision :json-false)))))))
      (expect (plist-get spec :capabilities) :to-be nil)
      (expect (plist-get spec :context-window) :to-be nil)))

  (it "describes an unknown model by its catalog name"
    (let ((spec (cdr (gptel-copilot--model-spec (copilot-tests--entry "gpt-new")))))
      (expect (plist-get spec :description) :to-equal "GPT-NEW")
      (expect (plist-get spec :input-cost) :to-be nil)))

  (it "lends a known model gptel's description and prices, not its capabilities"
    (let ((spec (cdr (gptel-copilot--model-spec
                      (copilot-tests--entry "copilot-test-known")))))
      (expect (plist-get spec :description) :to-equal "Bundled description")
      (expect (plist-get spec :input-cost) :to-be 5)
      (expect (plist-get spec :cutoff-date) :to-equal "2026-02")
      (expect (plist-get spec :capabilities) :to-equal '(media tool-use)))))

(describe "gptel-copilot-update-models"
  (let (copilot other)
    (before-each
      (setq copilot (gptel--make-gh :name "Copilot-test" :models '(copilot-test-old))
            other (gptel--make-backend :name "Other-test" :models '(copilot-test-shared)))
      (setf (symbol-plist 'copilot-test-shared) '(:request-params (:thinking t)))
      (push (cons "Copilot-test" copilot) gptel--known-backends)
      (push (cons "Other-test" other) gptel--known-backends))

    (after-each
      (dolist (model '(copilot-test-shared copilot-test-new))
        (setf (symbol-plist model) nil))
      (setq gptel--known-backends
            (cl-remove-if (lambda (entry)
                            (member (car entry) '("Copilot-test" "Other-test")))
                          gptel--known-backends)))

    (it "replaces the backend's models with the catalog's"
      (spy-on 'gptel-copilot--fetch-models
              :and-return-value '((copilot-test-new :description "new")))
      (gptel-copilot-update-models copilot)
      (expect (gptel-backend-models copilot) :to-equal '(copilot-test-new))
      (expect (get 'copilot-test-new :description) :to-equal "new"))

    (it "leaves the metadata of a model another backend lists"
      ;; the plist lives on the shared symbol; replacing it would strip the
      ;; other backend's request params
      (spy-on 'gptel-copilot--fetch-models
              :and-return-value '((copilot-test-shared :description "copilot")))
      (gptel-copilot-update-models copilot)
      (expect (gptel-backend-models copilot) :to-equal '(copilot-test-shared))
      (expect (symbol-plist 'copilot-test-shared)
              :to-equal '(:request-params (:thinking t))))

    (it "keeps the current models when the catalog is out of reach"
      (spy-on 'gptel-copilot--fetch-models :and-return-value nil)
      (expect (gptel-copilot-update-models copilot) :to-be nil)
      (expect (gptel-backend-models copilot) :to-equal '(copilot-test-old)))))

(describe "gptel-copilot--fetch-models"
  (let ((backend (gptel--make-gh :name "Copilot-test"
                                 :protocol "https" :host "copilot.test")))
    (it "never starts a login"
      (spy-on 'gptel--gh-auth)
      (spy-on 'url-retrieve-synchronously)
      (expect (gptel-copilot--fetch-models backend) :to-be nil)
      (expect 'gptel--gh-auth :not :to-have-been-called)
      (expect 'url-retrieve-synchronously :not :to-have-been-called))

    (it "asks the backend's host with the session token and integration id"
      ;; Copilot answers 400 to a bearer token without an integration id
      (let (seen-url seen-headers)
        (spy-on 'gptel-oauth--read-token :and-return-value "github-token")
        (spy-on 'gptel--gh-auth :and-call-fake
                (lambda ()
                  (setf (gptel--gh-token gptel-backend) '(:token "session"))))
        (spy-on 'url-retrieve-synchronously :and-call-fake
                (lambda (url &rest _)
                  (setq seen-url url
                        seen-headers url-request-extra-headers)
                  (copilot-tests--response
                   200 "{\"data\":[{\"id\":\"gpt-new\",\"name\":\"GPT New\"}]}")))
        (expect (mapcar #'car (gptel-copilot--fetch-models backend))
                :to-equal '(gpt-new))
        (expect seen-url :to-equal "https://copilot.test/models")
        (expect (alist-get "authorization" seen-headers nil nil #'equal)
                :to-equal "Bearer session")
        (expect (alist-get "copilot-integration-id" seen-headers nil nil #'equal)
                :to-equal "vscode-chat")))

    (it "warns and returns nil on an HTTP error"
      (spy-on 'gptel-oauth--read-token :and-return-value "github-token")
      (spy-on 'gptel--gh-auth)
      (spy-on 'display-warning)
      (spy-on 'url-retrieve-synchronously :and-call-fake
              (lambda (&rest _) (copilot-tests--response 401 "{}")))
      (expect (gptel-copilot--fetch-models backend) :to-be nil)
      (expect 'display-warning :to-have-been-called))))
