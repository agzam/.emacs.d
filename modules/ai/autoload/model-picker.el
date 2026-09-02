;;; modules/ai/autoload/model-picker.el -*- lexical-binding: t; -*-

;;; Provider headers and narrowing in the model pickers

;; gptel and eca list every backend's models as one flat PROVIDER:MODEL or
;; PROVIDER/MODEL list.  Typed input filters on the model name, so "opu"
;; leaves the same model from each provider standing next to each other and
;; the wrong provider is one keystroke away.  A header per provider keeps
;; them apart, and `consult-narrow-key' plus the letter the header spells
;; out cuts the list down to one provider.  Both go in from
;; `minibuffer-setup-hook': the pickers call `completing-read' with a plain
;; list, and the minibuffer is where consult's narrowing lives anyway.

(defvar consult--narrow)
(defvar consult-narrow-key)
(defvar consult-narrow-map)
(declare-function consult--narrow-setup "consult")
(declare-function consult--key-parse "consult")

(defvar model-picker-commands
  '(gptel--infix-provider eca-chat-select-model eca-chat-inline-select-model)
  "Commands whose `completing-read' lists models as PROVIDER<sep>MODEL.")

(defvar model-picker-narrow-pins '((?a . "Claude-OAuth") (?g . "Copilot"))
  "Narrowing keys fixed for the gptel backends: eca's initials for the same providers.")

(defun model-picker-provider (candidate)
  "The text before the first colon or slash in CANDIDATE, nil without one.
An alist entry counts by its key: that is what a completion predicate
receives for one."
  (let ((string (if (consp candidate) (car candidate) candidate)))
    (when (string-match "\\`\\([^:/]+\\)[:/]" string)
      (match-string 1 string))))

(defun model-picker-narrowing-keys (providers)
  "Alist of narrowing key to provider, one entry per name in PROVIDERS.
A provider in `model-picker-narrow-pins' keeps its pin, reserved ahead
of the others; every other one takes the first letter of its own name
not yet in use, so two sharing an initial still get distinct keys.  One
whose letters are all taken gets no key."
  (let ((taken (mapcar #'car (seq-filter (lambda (pin) (member (cdr pin) providers))
                                         model-picker-narrow-pins))))
    (delq nil
          (mapcar (lambda (provider)
                    (or (rassoc provider model-picker-narrow-pins)
                        (when-let* ((key (seq-find (lambda (char)
                                                     (and (<= ?a char ?z)
                                                          (not (memq char taken))))
                                                   (downcase provider))))
                          (push key taken)
                          (cons key provider))))
                  providers))))

(defun model-picker-group (keys)
  "A completion group function titling candidates by provider.
Each title also spells out the key from KEYS that narrows to the
provider, so the list itself shows what `consult-narrow-key' followed
by that letter does; under a transient which-key is off."
  (let ((narrow-key (and consult-narrow-key
                         (key-description (consult--key-parse consult-narrow-key)))))
    (lambda (candidate transform)
      (if transform
          candidate
        (when-let* ((provider (model-picker-provider candidate)))
          (if-let* ((key (and narrow-key (car (rassoc provider keys)))))
              (format "%s  (%s %c)" provider narrow-key key)
            provider))))))

(defun model-picker-narrow-predicate (keys)
  "A completion predicate keeping the provider `consult--narrow' names in KEYS."
  (lambda (candidate)
    (equal (model-picker-provider candidate) (alist-get consult--narrow keys))))

;;;###autoload
(defun model-picker-setup-h ()
  "Group a model picker's list by provider, narrowable to one with `consult-narrow-key'.
For `minibuffer-setup-hook', at a depth past vertico's own setup, which
installs the local map the narrowing keys compose onto."
  (when (memq this-command model-picker-commands)
    (require 'consult)
    (let ((keys (model-picker-narrowing-keys
                 (seq-uniq (delq nil (mapcar #'model-picker-provider
                                             (all-completions "" minibuffer-completion-table
                                                              minibuffer-completion-predicate))))))
          (map (make-sparse-keymap)))
      (setq-local completion-extra-properties
                  (append (list :group-function (model-picker-group keys))
                          completion-extra-properties))
      (consult--narrow-setup (list :predicate (model-picker-narrow-predicate keys) :keys keys)
                             map)
      (use-local-map (make-composed-keymap (list consult-narrow-map map) (current-local-map)))
      ;; Narrowing hides the current model, which the picker passes as the
      ;; default; vertico then selects the prompt, and RET yields that
      ;; hidden default - the wrong provider, silently.  With no default,
      ;; RET takes the first candidate in view.
      (setq minibuffer-default nil))))
