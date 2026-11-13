;;; modules/ai/autoload/keep-awake.el -*- lexical-binding: t; -*-

;; A lease renewed by a timer needs no teardown: when no agent works, or
;; Emacs hangs, the renewals stop and `caffeinate' runs out on its own.
;; Its -w flag ends it at once should Emacs die.

(defvar keep-awake-interval 30
  "Seconds between two checks for agent work.")

(defvar keep-awake-lease 120
  "Seconds a `caffeinate' lasts unless a later check replaces it.")

(defvar keep-awake-max-age (* 3 60 60)
  "Seconds after which work still busy counts as stuck.")

(defvar keep-awake--process nil
  "The `caffeinate' process holding the current lease, or nil.")

(defvar keep-awake--timer nil
  "The timer that runs `keep-awake-check'.")

(defvar keep-awake--first-seen nil
  "Alist of each busy item and the time a check first saw it busy.")

(defun keep-awake--eca-items ()
  "Return the working ECA chat buffers and the running ECA background jobs."
  (when (and (bound-and-true-p eca--sessions) (fboundp 'eca-chat-buffers))
    (seq-mapcat
     (lambda (session)
       (append
        (seq-filter (lambda (chat) (eq 'running (eca-chat-status chat)))
                    (eca-chat-buffers session))
        (when (fboundp 'eca--session-jobs)
          (thread-last
            (eca--session-jobs session)
            (seq-filter (lambda (job) (equal "running" (plist-get job :status))))
            (mapcar (lambda (job)
                      (list (plist-get job :chatId) (plist-get job :id))))))))
     (eca-vals eca--sessions))))

(defun keep-awake--agent-shell-items ()
  "Return the `agent-shell' buffers waiting on their agent."
  (when (fboundp 'agent-shell-buffers)
    (seq-filter (lambda (shell) (with-current-buffer shell (shell-maker-busy)))
                (agent-shell-buffers))))

(defun keep-awake--ghostel-items ()
  "Return the names of the eca-ghostel buffers whose command still runs."
  (when (fboundp 'eca-ghostel-list)
    (thread-last
      (eca-ghostel-list)
      (seq-filter (lambda (run) (string-prefix-p "running" (plist-get run :status))))
      (mapcar (lambda (run) (plist-get run :buffer))))))

(defun keep-awake--busy-items ()
  "Return one key for each piece of agent work in progress."
  (append (keep-awake--eca-items)
          (keep-awake--agent-shell-items)
          (keep-awake--ghostel-items)))

(defun keep-awake--first-seen-alist (items now)
  "Pair each of ITEMS with the time it was first seen busy, NOW for new ones."
  (mapcar (lambda (item)
            (cons item (or (alist-get item keep-awake--first-seen nil nil #'equal)
                           now)))
          items))

(defun keep-awake--renew ()
  "Start a new `caffeinate' lease, then end the one it replaces."
  (let ((previous keep-awake--process))
    (setq keep-awake--process
          (make-process
           :name "keep-awake"
           :command (list "caffeinate" "-i"
                          "-t" (number-to-string keep-awake-lease)
                          "-w" (number-to-string (emacs-pid)))
           :connection-type 'pipe
           :noquery t))
    (when (process-live-p previous)
      (delete-process previous))))

(defun keep-awake-check ()
  "Renew the lease while some agent work is busy and not stuck."
  (let ((now (float-time)))
    (setq keep-awake--first-seen
          (keep-awake--first-seen-alist (keep-awake--busy-items) now))
    (when (seq-some (lambda (entry) (< (- now (cdr entry)) keep-awake-max-age))
                    keep-awake--first-seen)
      (keep-awake--renew))))

;;;###autoload
(define-minor-mode keep-awake-mode
  "Keep the Mac from idle sleep while agents work."
  :global t
  :group 'convenience
  (when (timerp keep-awake--timer)
    (cancel-timer keep-awake--timer))
  (setq keep-awake--timer
        (and keep-awake-mode
             (run-with-timer 0 keep-awake-interval #'keep-awake-check)))
  (unless keep-awake-mode
    (when (process-live-p keep-awake--process)
      (delete-process keep-awake--process))
    (setq keep-awake--process nil
          keep-awake--first-seen nil)))
