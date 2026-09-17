;;; tests/ai/eca-handoff-tests.el --- ai/autoload/eca-handoff.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/ai/autoload/eca-handoff.el")

;; eca is absent here.  The lock reads the loading flag to tell a running
;; turn from an idle chat, and arms itself on the hook eca runs when a turn
;; ends; both are plain variables without it.
(defvar eca-chat--chat-loading nil)

(defun handoff-test-typing-refused-p ()
  "Non-nil when the buffer refuses what `self-insert-command' does.
That command carries an `interactive' spec starting with `*', which is
this barf, so the barf is what the reader hits."
  (condition-case err
      (progn (barf-if-buffer-read-only) nil)
    (buffer-read-only (and err t))))

(describe "eca-chat-handoff-lock-mode"
  (it "refuses typing while it is on"
    (with-temp-buffer
      (expect (handoff-test-typing-refused-p) :to-be nil)
      (eca-chat-handoff-lock-mode 1)
      (expect (handoff-test-typing-refused-p) :to-be t)))

  (it "gives the buffer back when it goes off"
    (with-temp-buffer
      (eca-chat-handoff-lock-mode 1)
      (eca-chat-handoff-lock-mode -1)
      (expect buffer-read-only :to-be nil)
      (expect (handoff-test-typing-refused-p) :to-be nil)))

  (it "stays a buffer-local affair"
    (let ((other (generate-new-buffer " *handoff-other*")))
      (unwind-protect
          (with-temp-buffer
            (eca-chat-handoff-lock-mode 1)
            (expect (buffer-local-value 'buffer-read-only other) :to-be nil))
        (kill-buffer other))))

  (it "sends any key bound to `read-only-mode' to the reclaim command"
    (expect (lookup-key eca-chat-handoff-lock-mode-map [remap read-only-mode])
            :to-be #'eca-chat-handoff-reclaim)))

(describe "eca-chat-handoff--segment"
  (it "shows nothing in a chat nobody handed off"
    (with-temp-buffer
      (expect (eca-chat-handoff--segment) :to-be nil)))

  (it "names the successor once the chat is locked"
    (with-temp-buffer
      (setq-local eca-chat-handoff-successor "eca-chat - handoff: port-callers")
      (eca-chat-handoff-lock-mode 1)
      (expect (eca-chat-handoff--segment)
              :to-match "handed off to eca-chat - handoff: port-callers")))

  (it "still says the chat is locked without a successor name"
    (with-temp-buffer
      (eca-chat-handoff-lock-mode 1)
      (expect (eca-chat-handoff--segment) :to-match "handed off - read-only")))

  (it "ends in a newline, as every transient area segment does"
    (with-temp-buffer
      (eca-chat-handoff-lock-mode 1)
      (expect (eca-chat-handoff--segment) :to-match "\n\\'"))))

(describe "eca-chat-handoff-lock"
  (it "locks an idle chat right away"
    (with-temp-buffer
      (eca-chat-handoff-lock "eca-chat - handoff: port-callers")
      (expect eca-chat-handoff-lock-mode :to-be t)
      (expect eca-chat-handoff-successor
              :to-equal "eca-chat - handoff: port-callers")))

  ;; The handoff itself runs inside a turn: locking there would take the
  ;; reader's steering away from the turn they are still watching.
  (it "only arms the lock while a turn is running"
    (with-temp-buffer
      (setq-local eca-chat--chat-loading t)
      (eca-chat-handoff-lock "eca-chat - handoff: port-callers")
      (expect eca-chat-handoff-lock-mode :to-be nil)
      (expect (handoff-test-typing-refused-p) :to-be nil)
      (expect (memq #'eca-chat-handoff--lock-h eca-chat-finished-hook) :to-be-truthy)))

  (it "closes the armed lock when the turn ends"
    (with-temp-buffer
      (setq-local eca-chat--chat-loading t)
      (eca-chat-handoff-lock "eca-chat - handoff: port-callers")
      (run-hooks 'eca-chat-finished-hook)
      (expect eca-chat-handoff-lock-mode :to-be t)
      (expect (handoff-test-typing-refused-p) :to-be t)))

  (it "locks the chat once and then leaves the turns alone"
    (with-temp-buffer
      (setq-local eca-chat--chat-loading t)
      (eca-chat-handoff-lock "eca-chat - handoff: port-callers")
      (run-hooks 'eca-chat-finished-hook)
      (expect (memq #'eca-chat-handoff--lock-h eca-chat-finished-hook) :to-be nil)))

  (it "takes no successor name as no name to show"
    (with-temp-buffer
      (eca-chat-handoff-lock "")
      (expect eca-chat-handoff-successor :to-be nil))))

(describe "eca-chat-handoff-reclaim"
  (it "gives back a chat the lock already closed"
    (with-temp-buffer
      (eca-chat-handoff-lock "eca-chat - handoff: port-callers")
      (eca-chat-handoff-reclaim)
      (expect eca-chat-handoff-lock-mode :to-be nil)
      (expect (handoff-test-typing-refused-p) :to-be nil)))

  ;; Reclaiming mid-turn has to disarm too, or the end of the turn would
  ;; lock the chat the reader just took back.
  (it "disarms a lock the turn has not closed yet"
    (with-temp-buffer
      (setq-local eca-chat--chat-loading t)
      (eca-chat-handoff-lock "eca-chat - handoff: port-callers")
      (eca-chat-handoff-reclaim)
      (run-hooks 'eca-chat-finished-hook)
      (expect eca-chat-handoff-lock-mode :to-be nil))))
