;;; modules/writing/autoload/input-method.el -*- lexical-binding: t; -*-

;; Alt-Tab for input methods: one press flips between no input method and
;; the one used last, repeated presses walk on.  Evil keeps a buffer's
;; method off outside insert state, so the switch goes to `evil-input-method'.

(require 'evil nil t)

(defvar input-method-cycle '("russian-computer" "spanish-postfix")
  "Input methods `cycle-input-method' switches to, besides none.")

(defvar input-method-recent nil
  "Members of `input-method-cycle', most recently used first.")

(defvar cycle-input-method--walk nil
  "The (RING . INDEX) of the ongoing run of `cycle-input-method' presses.")

(defun input-method-held-by-evil-p ()
  "Return non-nil when evil keeps the input method off in the current state."
  (and (bound-and-true-p evil-local-mode)
       (not (evil-state-property evil-state :input-method))))

;;;###autoload
(defun input-method-in-effect ()
  "Return the input method of the current buffer, counting one evil holds."
  (if (input-method-held-by-evil-p) evil-input-method current-input-method))

(defun input-method-note (method)
  "Move METHOD to the front of `input-method-recent' if it is cycled."
  (when (member method input-method-cycle)
    (setq input-method-recent (cons method (remove method input-method-recent)))))

;;;###autoload
(defun input-method-note-h ()
  "Record the input method just activated as the most recently used."
  (input-method-note current-input-method))

(defun cycle-input-method--ring (current)
  "Return the switching order that starts at CURRENT.
From a method, no input method comes first; the rest follow by recency."
  (let ((recent (seq-uniq (append (seq-filter (lambda (m) (member m input-method-cycle))
                                              input-method-recent)
                                  input-method-cycle))))
    (cons current (if current
                      (cons nil (remove current recent))
                    recent))))

(defun input-method-switch (method)
  "Give the current buffer METHOD, or no input method when it is nil."
  (if (input-method-held-by-evil-p)
      (progn (setq evil-input-method method)
             (force-mode-line-update))
    (if method
        (activate-input-method method)
      (deactivate-input-method)))
  (input-method-note method))

;;;###autoload
(defun cycle-input-method (&optional arg)
  "Switch between no input method and the one used last.
Pressing it again right away steps on through `input-method-cycle'.
With ARG, read any input method, as `toggle-input-method' does."
  (interactive "P")
  (if arg
      (toggle-input-method arg t)
    (let* ((walk (if (and (eq last-command 'cycle-input-method)
                          cycle-input-method--walk)
                     cycle-input-method--walk
                   (cons (cycle-input-method--ring (input-method-in-effect)) 0)))
           (ring (car walk))
           (index (mod (1+ (cdr walk)) (length ring))))
      (setq cycle-input-method--walk (cons ring index))
      (input-method-switch (nth index ring)))))
