;;; tests/e2e/ai-leader-keys.el --- gptel under SPC g beside git -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs.
;;
;; gptel shares SPC g with git and eca shares SPC e with edit, so a key
;; written into either group can shadow a neighbour.  Only the real leader
;; map, in the state the keys are typed in, shows which command wins.

(defun ai-leader-keys-e2e ()
  "Every gptel leader key resolves, and the keys around it still do."
  (with-temp-buffer
    (fundamental-mode)
    (evil-normal-state)
    (mapcar (pcase-lambda (`(,keys . ,command))
              (let ((got (key-binding (kbd keys))))
                (list :label (format "ai leader: %s -> %s" keys command)
                      :ok (eq got command)
                      :got (format "%S" got) :want (format "%S" command))))
            '(("SPC g g" . gptel-menu)
              ("SPC g e" . gptel-improve-text-transient)
              ("SPC g i" . gptel-inline-dwim)
              ("SPC g m" . gptel-mode)
              ("SPC g n" . open-gptel)
              ("SPC g q" . gptel-quick-question-buffer)
              ("SPC g RET" . gptel-send)
              ("SPC g v" . gptel-inline-visit-last-chat)
              ("SPC g /" . gptel-log-find)
              ;; the neighbours the ai keys must not shadow
              ("SPC g s" . magit-status)
              ("SPC g f" . magit-file-dispatch)
              ("SPC e i" . edit-indirect-region)
              ("SPC x x" . jinx-correct-word)
              ;; nothing is left behind under the text prefix
              ("SPC x g" . nil)
              ("SPC x e" . nil)))))

(add-to-list 'e2e-scenarios #'ai-leader-keys-e2e)
