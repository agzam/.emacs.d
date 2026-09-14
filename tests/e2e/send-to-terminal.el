;;; tests/e2e/send-to-terminal.el --- code snippets reaching a terminal -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs.
;;
;; Everything this flow depends on is invisible to a buttercup spec: the
;; markdown branch needs markdown-mode installed, inline code needs the
;; buffer fontified, the target has to reach `embark-code-snippet-map'
;; through `embark-keymap-alist', "T" has to resolve there, the region has
;; to survive into the action embark runs, and the terminal picker has to
;; survive embark's target injection - embark types the target into the
;; first minibuffer an action opens and exits the read itself, which fed the
;; picker a snippet where it wanted a buffer name.  Nothing but a real
;; keypress against a real minibuffer shows that, so no case here fakes
;; `completing-read'.
;;
;; The terminals are buffers carrying the major mode: starting a shell would
;; test the shell, and ghostel needs a native module CI does not build.
;; `ghostel-paste-string' is the one stub, and only so the ghostel branch can
;; report what it was handed.

(require 'cl-lib)

(defvar send-to-terminal-e2e-cases
  '((:label "org inline code"
     :ext "org" :text "run ~ls -la~ now\npick this whole line\n"
     :search "ls -" :type code-snippet :want "ls -la")
    (:label "org verbatim"
     :ext "org" :text "say =echo hi= now\n"
     :search "echo" :type code-snippet :want "echo hi")
    (:label "markdown inline code"
     :ext "md" :text "run `ls -la` now\n"
     :search "ls -" :type code-snippet :want "ls -la")
    (:label "markdown fenced block"
     :ext "md" :text "intro\n\n```sh\necho hi\nls -l\n```\n\noutro\n"
     :search "echo" :type code-snippet :want "echo hi\nls -l")
    ;; the mode an eca chat buffer derives from, where this was first tried
    (:label "gfm inline code"
     :ext "md" :mode gfm-mode :text "run `ls -la` now\n"
     :search "ls -" :type code-snippet :want "ls -la")
    (:label "selected region"
     :ext "org" :text "run ~ls -la~ now\npick this whole line\n"
     :search "pick" :region t :type region :want "pick this whole line")
    (:label "ghostel is pasted into, never typed at"
     :ext "org" :terminal-mode ghostel-mode :text "run ~ls -la~ now\n"
     :search "ls -" :type code-snippet :want "ls -la" :want-pasted "ls -la")
    (:label "terminal already on screen is reused and takes focus"
     :ext "org" :text "run ~ls -la~ now\n"
     :search "ls -" :type code-snippet :want "ls -la"
     :show t :want-shown-before t :want-landed :terminal)
    (:label "terminal off screen is popped up and takes focus"
     :ext "org" :text "run ~ls -la~ now\n"
     :search "ls -" :type code-snippet :want "ls -la"
     :want-shown-before nil :want-landed :terminal)
    ;; embark's injection lands here: a picker that swallowed the target
    ;; would answer itself with the snippet and never reach a buffer
    (:label "two terminals: the picker opens and RET takes the recent one"
     :ext "org" :text "run ~ls -la~ now\n"
     :search "ls -" :type code-snippet :want "ls -la"
     :terminals 2 :keys "T RET" :want-prompts 1 :want-terminal 1)
    (:label "two terminals: the picker honours a moved selection"
     :ext "org" :text "run ~ls -la~ now\n"
     :search "ls -" :type code-snippet :want "ls -la"
     :terminals 2 :keys "T C-n RET" :want-prompts 1 :want-terminal 2)
    (:label "one terminal asks nothing"
     :ext "org" :text "run ~ls -la~ now\n"
     :search "ls -" :type code-snippet :want "ls -la" :want-prompts 0)
    ;; a real process, because RET reaching the shell is the point of the
    ;; state switch and no mode-carrying buffer can show that
    (:label "a live eshell ends in insert state with RET on its own submit"
     :ext "org" :real-eshell t :text "run ~ls -la~ now\n"
     :search "ls -" :type code-snippet :want "ls -la"
     :want-ret eshell-send-input :want-landed :terminal)
    ;; point is on the url, not on the snippet wrapping it, so the url is the
    ;; default and cycling once past it lands the send on the snippet
    (:label "cycling past a url inside a snippet still sends the snippet"
     :ext "md" :text "see `https://example.com/x` now\n"
     :search "example" :type url :cmd-type code-snippet
     :keys "C-; T" :want "https://example.com/x"))
  "One case per syntax, per terminal count and per placement.
:search puts point inside the snippet, :mode forces a major mode on the
fixture, :region selects the line instead, :show displays the terminal
beside the document, :terminals asks for more than one so the picker has
to run, :keys is the whole key sequence typed once embark is up,
:want-terminal is the 1-based terminal the text must land in and every
other must stay empty, :want-prompts is how many minibuffer reads the
act is allowed, :want-shown-before is whether a window already held the
terminal when the act started, :want-landed names the buffer focus must
end in, :real-eshell runs a live shell instead of a buffer carrying the
mode, :want-ret is the command RET must reach afterwards, :want-types
is the leading run of embark's target list in cycling order, and
:cmd-type names the target the keys act on when they cycle first.
Every case starts its terminals in normal state and every one must end
in insert state.")

(defun send-to-terminal-e2e--act (case)
  "Press CASE's keys through a real `embark-act', report what arrived."
  (let* ((file (expand-file-name (format "snippet.%s" (plist-get case :ext))
                                 e2e-work-dir))
         (buf (find-file-noselect file))
         (count (or (plist-get case :terminals) 1))
         (terminals (if (plist-get case :real-eshell)
                        (list (save-window-excursion (eshell t)))
                      (cl-loop repeat count
                               collect (generate-new-buffer " *e2e-terminal*"))))
         (target (nth (1- (or (plist-get case :want-terminal) 1)) terminals))
         (keys (or (plist-get case :keys) "T"))
         (prompts 0)
         (tally (lambda () (cl-incf prompts)))
         type types cmd mode pasted got others landed shown-before state ret err)
    (dolist (term terminals)
      (with-current-buffer term
        (unless (plist-get case :real-eshell)
          (setq major-mode (or (plist-get case :terminal-mode) 'eshell-mode)))
        ;; normal state is where reading output leaves you, and where RET is
        ;; evil's motion rather than the shell's submit
        (evil-normal-state)))
    (unwind-protect
        (cl-letf (((symbol-function 'terminal-buffers) (lambda () terminals))
                  ;; the real one talks to a pty; echo into the buffer so the
                  ;; case can check the text the same way as for eshell
                  ((symbol-function 'ghostel-paste-string)
                   (lambda (s) (setq pasted s) (insert s))))
          (add-hook 'minibuffer-setup-hook tally)
          (with-current-buffer buf
            (switch-to-buffer buf)
            (delete-other-windows)
            (erase-buffer)
            (insert (plist-get case :text))
            (when-let* ((m (plist-get case :mode))) (funcall m))
            (font-lock-ensure)
            (goto-char (point-min))
            (search-forward (plist-get case :search))
            (when (plist-get case :region)
              (push-mark (line-beginning-position) t t)
              (goto-char (line-end-position)))
            (when (plist-get case :show)
              (set-window-buffer (split-window-right) target))
            (setq mode major-mode)
            ;; no window here before the act is what makes the pop-up case
            ;; mean anything
            (setq shown-before (and (get-buffer-window target t) t))
            (setq types (mapcar (lambda (tg) (plist-get tg :type))
                                (embark--targets)))
            (setq type (car types))
            ;; the keys may cycle before acting, so the map that has to carry
            ;; "T" is the one of the target they land on
            (setq cmd (ignore-errors
                        (lookup-key
                         (embark--action-keymap
                          (or (plist-get case :cmd-type) type) nil)
                         (kbd "T"))))
            (condition-case e
                (let ((unread-command-events (listify-key-sequence (kbd keys))))
                  (embark-act))
              (error (setq err e))))
          (setq got (with-current-buffer target
                      (if (plist-get case :real-eshell)
                          ;; a live eshell holds its banner and prompt too
                          (buffer-substring-no-properties
                           eshell-last-output-end (point-max))
                        (buffer-string)))
                state (buffer-local-value 'evil-state target)
                ret (with-current-buffer target (key-binding (kbd "RET")))
                others (mapcar (lambda (b) (with-current-buffer b (buffer-string)))
                               (remq target terminals))
                landed (cond ((eq (window-buffer (selected-window)) target) :terminal)
                             ((eq (window-buffer (selected-window)) buf) :document)
                             (t (buffer-name (window-buffer (selected-window)))))))
      (remove-hook 'minibuffer-setup-hook tally)
      ;; a case whose keys did not all get consumed would feed the rest into
      ;; whichever scenario runs next
      (discard-input)
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf)
      (let ((kill-buffer-query-functions nil))
        (dolist (term terminals)
          (when-let* ((p (get-buffer-process term)))
            (set-process-query-on-exit-flag p nil))
          (kill-buffer term))))
    (list :label (plist-get case :label)
          :mode mode
          :keys keys :cmd cmd :type type :want-type (plist-get case :type)
          :probe (format "types=%S landed=%s state=%s RET=%s shown-before=%s prompts=%s others=%S pasted=%S"
                         types landed state ret shown-before prompts others pasted)
          :got got :want (plist-get case :want) :err err
          :ok (and (null err)
                   (eq type (plist-get case :type))
                   (eq cmd 'send-to-terminal)
                   (equal got (plist-get case :want))
                   ;; every terminal the case did not name stays untouched,
                   ;; so picking the wrong one cannot read as a pass
                   (cl-every #'string-empty-p others)
                   (equal pasted (plist-get case :want-pasted))
                   (or (null (plist-get case :want-prompts))
                       (eq prompts (plist-get case :want-prompts)))
                   (or (null (plist-get case :want-landed))
                       (eq landed (plist-get case :want-landed)))
                   (or (not (plist-member case :want-shown-before))
                       (eq shown-before (plist-get case :want-shown-before)))
                   ;; every case starts in normal state, so this is the switch
                   (eq state 'insert)
                   (or (null (plist-get case :want-ret))
                       (eq ret (plist-get case :want-ret)))))))

(defvar send-to-terminal-e2e-order-cases
  '((:label "a bug reference inside markdown code"
     :ext "md" :text "see `stitchdata/cloudcutter#1384` now\n"
     :search "cloudcut" :first bug-reference-link)
    (:label "a bug reference inside org code"
     :ext "org" :text "see ~stitchdata/cloudcutter#1384~ now\n"
     :search "cloudcut" :first bug-reference-link)
    (:label "a url inside markdown code"
     :ext "md" :text "see `https://example.com/x` now\n"
     :search "example" :first url)
    (:label "an RFC number inside org code"
     :ext "org" :text "see ~RFC 1234~ now\n"
     :search "1234" :first rfc-number)
    (:label "a snippet holding nothing narrower"
     :ext "md" :text "run `ls -la` now\n"
     :search "ls -" :first code-snippet)
    (:label "a fenced block holding nothing narrower"
     :ext "md" :text "a\n\n```sh\nls -la\n```\n\nb\n"
     :search "ls -" :first code-snippet))
  "Where point sits inside a snippet, and the target that must answer for it.
:first is the default target embark offers; unless it is the snippet
itself, the snippet has to come later in the list so a cycle reaches it.
The count of targets between them is embark's business and no case
pins it.")

(defun send-to-terminal-e2e--order (case)
  "Report the target order embark offers at CASE's point.
A snippet contains other targets and is contained by none, so it is the
default only when nothing narrower sits under point."
  (let* ((file (expand-file-name (format "order.%s" (plist-get case :ext))
                                 e2e-work-dir))
         (buf (find-file-noselect file))
         (first (plist-get case :first))
         types)
    (unwind-protect
        (with-current-buffer buf
          (erase-buffer)
          (insert (plist-get case :text))
          (font-lock-ensure)
          (goto-char (point-min))
          (search-forward (plist-get case :search))
          (setq types (mapcar (lambda (tg) (plist-get tg :type))
                              (embark--targets))))
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf))
    (list :label (format "target order: %s" (plist-get case :label))
          :got types
          :want (if (eq first 'code-snippet)
                    first
                  (format "%s first, code-snippet later" first))
          :ok (and (eq (car types) first)
                   (or (eq first 'code-snippet)
                       (and (memq 'code-snippet (cdr types)) t))))))

(defun send-to-terminal-e2e ()
  "Drive \"T\" over every snippet syntax, a region, and every terminal count."
  (require 'embark)
  (require 'which-key)
  (require 'markdown-mode)
  ;; resolve the autoload before the stubs go in: loading terminal.el from
  ;; inside a case would redefine the `terminal-buffers' each one fakes
  (dolist (fn '(send-to-terminal code-snippet-at-point))
    (when (autoloadp (symbol-function fn))
      (autoload-do-load (symbol-function fn) fn)))
  ;; one case blowing up must cost that case, not the ones beside it
  (let ((guarded (lambda (fn)
                   (lambda (case)
                     (condition-case e
                         (funcall fn case)
                       (error (list :label (plist-get case :label)
                                    :err e :ok nil)))))))
    (append
     (mapcar (funcall guarded #'send-to-terminal-e2e--order)
             send-to-terminal-e2e-order-cases)
     (mapcar (funcall guarded #'send-to-terminal-e2e--act)
             send-to-terminal-e2e-cases))))

(add-to-list 'e2e-scenarios #'send-to-terminal-e2e)
