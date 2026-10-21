;;; modules/ai/autoload/gptel-copilot.el -*- lexical-binding: t; -*-

;;; Copilot model discovery

(require 'cl-lib)
(require 'url-http)
(require 'gptel-gh)

(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(defun gptel-copilot--hidden-p (entry)
  "Return non-nil if Copilot's own model picker hides catalog ENTRY."
  (or (eq (plist-get entry :model_picker_enabled) :json-false)
      (equal (plist-get (plist-get entry :policy) :state) "disabled")))

(defun gptel-copilot--model-spec (entry)
  "Convert Copilot catalog ENTRY into a (NAME . PLIST) model spec.
The catalog decides what the model can do; `gptel--gh-models' only lends
its description, prices and cutoff date."
  (let* ((name (intern (plist-get entry :id)))
         (known (alist-get name gptel--gh-models))
         (capabilities (plist-get entry :capabilities))
         (supports (plist-get capabilities :supports))
         (limits (plist-get capabilities :limits))
         (context (plist-get limits :max_context_window_tokens)))
    (cons name
          (list :description (or (plist-get known :description)
                                 (plist-get entry :name))
                :capabilities
                (delq nil
                      (list (and (eq t (plist-get supports :vision)) 'media)
                            (and (eq t (plist-get supports :tool_calls)) 'tool-use)
                            (and (eq t (plist-get supports :structured_outputs)) 'json)
                            (and (member "/responses" (plist-get entry :supported_endpoints))
                                 'responses-api)))
                :mime-types (plist-get (plist-get limits :vision) :supported_media_types)
                :context-window (and (numberp context) (/ context 1000))
                :input-cost (plist-get known :input-cost)
                :output-cost (plist-get known :output-cost)
                :cutoff-date (plist-get known :cutoff-date)))))

(defun gptel-copilot--model-specs (response)
  "Convert the visible models of Copilot catalog RESPONSE into model specs."
  (mapcar #'gptel-copilot--model-spec
          (seq-remove #'gptel-copilot--hidden-p (plist-get response :data))))

(defun gptel-copilot--fetch-models (backend)
  "Return model specs from the Copilot catalog of BACKEND's account.
Return nil without a stored GitHub login instead of starting one."
  (when (or (gptel--gh-github-token backend)
            (gptel-oauth--read-token gptel-gh-github-token-file))
    (condition-case err
        (let ((gptel-backend backend))
          (gptel--gh-auth)
          (let ((url-request-method "GET")
                ;; without an integration id Copilot rejects the token
                (url-request-extra-headers
                 `(("authorization"
                    . ,(concat "Bearer " (plist-get (gptel--gh-token backend) :token)))
                   ("copilot-integration-id" . "vscode-chat"))))
            (with-current-buffer
                (or (url-retrieve-synchronously
                     (format "%s://%s/models"
                             (gptel-backend-protocol backend)
                             (gptel-backend-host backend))
                     t nil 10)
                    (error "No response"))
              (unwind-protect
                  (progn
                    (unless (eq url-http-response-status 200)
                      (error "HTTP %s" url-http-response-status))
                    (goto-char url-http-end-of-headers)
                    (gptel-copilot--model-specs
                     (json-parse-buffer :object-type 'plist :array-type 'list
                                        :null-object nil :false-object :json-false)))
                (kill-buffer)))))
      (error
       (display-warning 'gptel-copilot
                        (format "Could not refresh Copilot models: %s"
                                (error-message-string err))
                        :warning)
       nil))))

;;;###autoload
(defun gptel-copilot-update-models (backend)
  "Replace BACKEND's models with the Copilot catalog; return them, or nil.
A model another backend lists keeps its metadata: gptel stores it on the
model symbol, which every backend listing that name shares."
  (when-let* ((specs (gptel-copilot--fetch-models backend)))
    (let ((shared (cl-loop for (_ . other) in gptel--known-backends
                           unless (eq other backend)
                           append (gptel-backend-models other))))
      (setf (gptel-backend-models backend)
            (gptel--process-models
             (mapcar (lambda (spec)
                       (if (memq (car spec) shared) (car spec) spec))
                     specs))))))
