;;; gptel-anthropic-oauth.el --- OAuth gptel backend for Claude  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Package-Requires: ((emacs "27.1") (gptel))
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Minimal OAuth implementation for Claude/Anthropic API access.
;; Uses device flow authentication with PKCE.
;; Automatically injects required system prompt for OAuth tokens.

;; To use:
;; (setq gptel-model 'claude-sonnet-4-5-20250929
;;       gptel-backend (gptel-make-anthropic-oauth "Claude-OAuth" :stream t))
;;    Use `gptel-anthropic-oauth-refresh-models' to refresh an existing backend.

;;; Code:

(require 'gptel)
(require 'gptel-anthropic)
(require 'json)
(require 'url)
(require 'url-http)
(require 'browse-url)

(defgroup gptel-anthropic-oauth nil
  "OAuth authentication for Claude in gptel."
  :group 'gptel)

(cl-defstruct (gptel-anthropic-oauth (:include gptel-anthropic)
                                     (:copier nil)
                                     (:constructor gptel--make-anthropic-oauth)))

;;; Configuration

(defcustom gptel-anthropic-oauth-cache-dir
  (expand-file-name ".cache/anthropic-oauth/" user-emacs-directory)
  "Directory for OAuth token cache."
  :type 'string
  :group 'gptel-anthropic-oauth)

(defconst gptel-anthropic-oauth--client-id "9d1c250a-e61b-44d9-88ed-5944d1962f5e"
  "OAuth client ID from OpenCode.")

(defconst gptel-anthropic-oauth--auth-url "https://claude.ai/oauth/authorize"
  "OAuth authorization endpoint.")

(defconst gptel-anthropic-oauth--token-url "https://console.anthropic.com/v1/oauth/token"
  "OAuth token endpoint.")

(defconst gptel-anthropic-oauth--redirect-uri "https://console.anthropic.com/oauth/code/callback"
  "OAuth redirect URI.")

(defconst gptel-anthropic-oauth--models-url "https://api.anthropic.com/v1/models"
  "Anthropic model discovery endpoint.")

(defconst gptel-anthropic-oauth--required-system-prompt
  "You are Claude Code, Anthropic's official CLI for Claude."
  "Required system prompt for OAuth token validation.")

;;; Token Storage

(defvar gptel-anthropic-oauth--token-cache nil
  "In-memory token cache: (access-token refresh-token expiry).")

(defun gptel-anthropic-oauth--ensure-cache-dir ()
  "Ensure cache directory exists with proper permissions."
  (unless (file-directory-p gptel-anthropic-oauth-cache-dir)
    (make-directory gptel-anthropic-oauth-cache-dir t)
    (set-file-modes gptel-anthropic-oauth-cache-dir #o700)))

(defun gptel-anthropic-oauth--save-tokens (access-token refresh-token expiry)
  "Save tokens to secure cache."
  (gptel-anthropic-oauth--ensure-cache-dir)
  (let ((token-file (expand-file-name "tokens.el" gptel-anthropic-oauth-cache-dir)))
    (with-temp-buffer
      (insert (format "(%S %S %S)"
                      access-token
                      refresh-token
                      (and expiry (float-time expiry))))
      (write-region (point-min) (point-max) token-file nil 'silent))
    (set-file-modes token-file #o600)
    (setq gptel-anthropic-oauth--token-cache
          (list access-token refresh-token expiry))))

(defun gptel-anthropic-oauth--load-tokens ()
  "Load tokens from cache."
  (let ((token-file (expand-file-name "tokens.el" gptel-anthropic-oauth-cache-dir)))
    (when (file-exists-p token-file)
      (with-temp-buffer
        (insert-file-contents token-file)
        (let ((data (read (current-buffer))))
          (when (and (listp data) (= (length data) 3))
            (setq gptel-anthropic-oauth--token-cache
                  (list (nth 0 data)
                        (nth 1 data)
                        (and (nth 2 data)
                             (seconds-to-time (nth 2 data)))))))))))

(defun gptel-anthropic-oauth--token-valid-p ()
  "Check if current token is valid."
  (when gptel-anthropic-oauth--token-cache
    (let ((expiry (nth 2 gptel-anthropic-oauth--token-cache)))
      (or (null expiry)  ; No expiry means valid
          (time-less-p (current-time) expiry)))))

;;; PKCE Implementation

(defun gptel-anthropic-oauth--base64url-encode (str)
  "Base64url encode STR."
  (let ((b64 (base64-encode-string str t)))
    (setq b64 (replace-regexp-in-string "+" "-" b64))
    (setq b64 (replace-regexp-in-string "/" "_" b64))
    (replace-regexp-in-string "=+$" "" b64)))

(defun gptel-anthropic-oauth--generate-verifier ()
  "Generate PKCE code verifier."
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~"))
    (apply #'string
           (cl-loop repeat 128
                    collect (aref chars (random (length chars)))))))

(defun gptel-anthropic-oauth--generate-challenge (verifier)
  "Generate PKCE code challenge from VERIFIER."
  (gptel-anthropic-oauth--base64url-encode
   (secure-hash 'sha256 verifier nil nil t)))

;;; OAuth Flow

(defun gptel-anthropic-oauth--exchange-code (code verifier)
  "Exchange authorization CODE for tokens using VERIFIER."
  ;; Parse the code - it might contain state after #
  (let* ((parts (split-string code "#"))
         (auth-code (car parts))
         (state (cadr parts))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data
          (json-encode
           `(("code" . ,auth-code)
             ,@(when state `(("state" . ,state)))
             ("grant_type" . "authorization_code")
             ("client_id" . ,gptel-anthropic-oauth--client-id)
             ("redirect_uri" . ,gptel-anthropic-oauth--redirect-uri)
             ("code_verifier" . ,verifier)))))
    (condition-case err
        (with-current-buffer
            (url-retrieve-synchronously gptel-anthropic-oauth--token-url)
          (goto-char (point-min))
          (when (re-search-forward "^$" nil t)
            (json-read)))
      (error
       (message "Error exchanging code: %s" err)
       nil))))

(defun gptel-anthropic-oauth--refresh-token ()
  "Refresh access token using refresh token."
  (when-let ((refresh-token (nth 1 gptel-anthropic-oauth--token-cache)))
    (let* ((url-request-method "POST")
           (url-request-extra-headers '(("Content-Type" . "application/json")))
           (url-request-data
            (json-encode
             `(("grant_type" . "refresh_token")
               ("refresh_token" . ,refresh-token)
               ("client_id" . ,gptel-anthropic-oauth--client-id)))))
      (condition-case nil
          (with-current-buffer
              (url-retrieve-synchronously gptel-anthropic-oauth--token-url)
            (goto-char (point-min))
            (when (re-search-forward "^$" nil t)
              (let ((response (json-read)))
                (when-let ((access-token (cdr (assoc 'access_token response))))
                  (let ((new-refresh (or (cdr (assoc 'refresh_token response))
                                         refresh-token))
                        (expires-in (or (cdr (assoc 'expires_in response)) 3600)))
                    (gptel-anthropic-oauth--save-tokens
                     access-token
                     new-refresh
                     (time-add (current-time) (seconds-to-time expires-in)))
                    t)))))
        (error nil)))))

;;; Parsing
(cl-defmethod gptel--request-data :around ((backend gptel-anthropic-oauth) prompts)
  (let* ((prompts-plist (cl-call-next-method backend prompts))
         (system (plist-get prompts-plist :system)))
    (plist-put prompts-plist :system
               (cl-typecase system
                 (string                ;simple system message
                  `[(:type "text" :text ,gptel-anthropic-oauth--required-system-prompt)
                    (:type "text" :text ,system)])
                 (vector                ;compound system message
                  (vconcat
                   `[(:type "text" :text ,gptel-anthropic-oauth--required-system-prompt)]
                   system))))))

;;; Public Interface

(defun gptel-anthropic-oauth-status ()
  "Check OAuth authentication status."
  (interactive)
  (unless gptel-anthropic-oauth--token-cache
    (gptel-anthropic-oauth--load-tokens))
  (let ((status (cond
                 ((gptel-anthropic-oauth--token-valid-p)
                  (format "Authenticated (expires %s)"
                          (format-time-string "%Y-%m-%d %H:%M:%S"
                                              (nth 2 gptel-anthropic-oauth--token-cache))))
                 ((nth 1 gptel-anthropic-oauth--token-cache)
                  "Token expired, refresh token available")
                 (t "Not authenticated"))))
    (message "Claude OAuth: %s" status)))

(defun gptel-anthropic-oauth-login ()
  "Authenticate with Claude using OAuth device flow."
  (interactive)
  (let* ((verifier (gptel-anthropic-oauth--generate-verifier))
         (challenge (gptel-anthropic-oauth--generate-challenge verifier))
         (auth-url (format "%s?code=true&client_id=%s&response_type=code&redirect_uri=%s&scope=%s&code_challenge=%s&code_challenge_method=S256&state=%s"
                           gptel-anthropic-oauth--auth-url
                           gptel-anthropic-oauth--client-id
                           (url-hexify-string gptel-anthropic-oauth--redirect-uri)
                           (url-hexify-string "org:create_api_key user:profile user:inference")
                           challenge
                           verifier)))
    (browse-url auth-url)
    (message "Opening browser for authentication...")
    (let ((code (read-passwd "Paste the authorization code from browser: ")))
      (when (string-empty-p code)
        (user-error "No authorization code provided"))
      (if-let ((response (gptel-anthropic-oauth--exchange-code code verifier))
               (access-token (cdr (assoc 'access_token response))))
          (progn
            (gptel-anthropic-oauth--save-tokens
             access-token
             (cdr (assoc 'refresh_token response))
             (time-add (current-time)
                       (seconds-to-time (or (cdr (assoc 'expires_in response)) 3600))))
            (message "Successfully authenticated with Claude!"))
        (user-error "Failed to authenticate")))))

(defun gptel-anthropic-oauth-logout ()
  "Clear OAuth tokens."
  (interactive)
  (setq gptel-anthropic-oauth--token-cache nil)
  (let ((token-file (expand-file-name "tokens.el" gptel-anthropic-oauth-cache-dir)))
    (when (file-exists-p token-file)
      (delete-file token-file)))
  (message "Logged out from Claude OAuth"))

(defun gptel-anthropic-oauth--get-token ()
  "Get valid OAuth token, refreshing if necessary."
  (unless gptel-anthropic-oauth--token-cache
    (gptel-anthropic-oauth--load-tokens))
  (unless (gptel-anthropic-oauth--token-valid-p)
    (unless (gptel-anthropic-oauth--refresh-token)
      (gptel-anthropic-oauth-login)))
  (car gptel-anthropic-oauth--token-cache))

;;; Model discovery

(defun gptel-anthropic-oauth--json-value (key object)
  "Return KEY from OBJECT, accepting symbol or string JSON keys."
  (or (alist-get key object)
      (alist-get (symbol-name key) object nil nil #'equal)))

(defun gptel-anthropic-oauth--model-specs (response)
  "Convert a model-list RESPONSE into gptel model specifications."
  (let ((known (mapcar (lambda (model)
                         (cons (symbol-name (car model)) (cdr model)))
                       gptel--anthropic-models))
        models)
    (dolist (model (append (gptel-anthropic-oauth--json-value 'data response)
                           nil))
      (when-let* ((id (gptel-anthropic-oauth--json-value 'id model))
                  (name (intern id)))
        (push
         (if-let* ((metadata (cdr (assoc id known))))
             (cons name metadata)
           (list name
                 :description
                 (or (gptel-anthropic-oauth--json-value
                      'display_name model)
                     id)
                 :capabilities '(media tool-use cache)
                 :mime-types
                 '("image/jpeg" "image/png" "image/gif" "image/webp"
                   "application/pdf")
                 :context-window 200))
         models)))
    (sort (nreverse models)
          (lambda (left right)
            (string< (symbol-name (car right))
                     (symbol-name (car left)))))))

(defun gptel-anthropic-oauth--cached-token ()
  "Return a cached OAuth token without starting an interactive login."
  (unless gptel-anthropic-oauth--token-cache
    (gptel-anthropic-oauth--load-tokens))
  (unless (gptel-anthropic-oauth--token-valid-p)
    (gptel-anthropic-oauth--refresh-token))
  (when (gptel-anthropic-oauth--token-valid-p)
    (car gptel-anthropic-oauth--token-cache)))

(defun gptel-anthropic-oauth--fetch-models ()
  "Fetch model specifications available to the current OAuth account."
  (when-let* ((token (gptel-anthropic-oauth--cached-token)))
    (condition-case err
        (let ((url-request-method "GET")
              (url-request-extra-headers
               `(("Authorization" . ,(concat "Bearer " token))
                 ("anthropic-version" . "2023-06-01")
                 ("anthropic-beta" . "oauth-2025-04-20"))))
          (with-current-buffer
              (url-retrieve-synchronously
               gptel-anthropic-oauth--models-url nil nil 10)
            (unwind-protect
                (progn
                  (unless (and (boundp 'url-http-response-status)
                               (< url-http-response-status 300))
                    (error "HTTP %s" url-http-response-status))
                  (goto-char (point-min))
                  (unless (re-search-forward "^$" nil t)
                    (error "Malformed response headers"))
                  (gptel-anthropic-oauth--model-specs (json-read)))
              (kill-buffer (current-buffer)))))
      (error
       (display-warning
        'gptel-anthropic-oauth
        (format "Could not refresh Claude models: %s"
                (error-message-string err))
        :warning)
       nil))))

(defun gptel-anthropic-oauth--refresh-backends (models)
  "Set OAuth backend model lists to MODELS."
  (dolist (entry gptel--known-backends)
    (when (gptel-anthropic-oauth-p (cdr entry))
      (setf (gptel-backend-models (cdr entry))
            (gptel--process-models models)))))

(defun gptel-anthropic-oauth-refresh-models ()
  "Refresh Claude OAuth models from Anthropic."
  (interactive)
  (if-let* ((models (gptel-anthropic-oauth--fetch-models)))
      (progn
        (gptel-anthropic-oauth--refresh-backends models)
        (message "Claude OAuth: loaded %d models" (length models)))
    (user-error "Claude OAuth model discovery failed")))

;;; Backend Creation

;;;###autoload
(cl-defun gptel-make-anthropic-oauth
    (name &key stream key
          models)
  "Create Claude backend with OAuth authentication.

NAME is the backend name.
STREAM enables streaming responses.
MODELS is the list of available models."
  (declare (indent 1))
  (let* ((models (or models
                     (gptel-anthropic-oauth--fetch-models)
                     (progn
                       (message "Claude OAuth: using bundled model metadata")
                       gptel--anthropic-models)))
         (backend
         (gptel--make-anthropic-oauth
          :name name
          :host "api.anthropic.com"
          :header (lambda ()
                    `(("authorization" . ,(format "Bearer %s" (gptel-anthropic-oauth--get-token)))
                      ("anthropic-version" . "2023-06-01")
                      ("anthropic-beta" . "oauth-2025-04-20,claude-code-20250219,interleaved-thinking-2025-05-14,fine-grained-tool-streaming-2025-05-14")))
          :models (gptel--process-models models)
          :protocol "https"
          :endpoint "/v1/messages"
          :stream stream
          :url "https://api.anthropic.com/v1/messages")))
    (prog1 backend
      (setf (alist-get name gptel--known-backends nil nil #'equal) backend))))

(provide 'gptel-anthropic-oauth)
;;; gptel-anthropic-oauth.el ends here
