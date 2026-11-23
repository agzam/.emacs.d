;;; tests/e2e/timestamp-actions.el --- timestamps shown across zones from embark -*- lexical-binding: t; -*-
;; Loaded by scripts/e2e-check.el inside a fully booted Emacs, not by
;; buttercup (the name stays clear of *-tests.el so discovery skips it).
;;
;; A spec cannot see the wiring: the timestamp target has to come first
;; among the finders, reach `embark-timestamp-map' through
;; `embark-keymap-alist', and the zone prompt a prefix argument opens has to
;; survive embark's injection, which types the target into the first
;; minibuffer an action opens.

(require 'cl-lib)

(defvar timestamp-actions-e2e-zones
  '(("UTC" "UTC") ("America/Chicago" "Central"))
  "A fixed `world-clock-list', so the expected tables stay put.")

(defun timestamp-actions-e2e-year ()
  "What the rows put after a 2026 date: nothing in 2026, the year after it."
  (if (equal (format-time-string "%Y") "2026") "" ", 2026"))

(defun timestamp-actions-e2e-table (header ms)
  "The table for 2026-11-23 14:05 UTC under HEADER, MS its epoch milliseconds."
  (let ((year (timestamp-actions-e2e-year)))
    (concat header "\n"
            (format "Central   Mon, Nov 23%s  08:05  CST  local\n" year)
            (format "UTC       Mon, Nov 23%s  14:05  UTC\n" year)
            "epoch s   1795442700\n"
            "epoch ms  " ms)))

(defun timestamp-actions-e2e-night (buffer)
  "Labels of the rows in BUFFER that carry the `timestamp-night' face."
  (with-current-buffer buffer
    (seq-keep (lambda (line)
                (when (text-property-any 0 (length line) 'face 'timestamp-night line)
                  (car (split-string line))))
              (split-string (buffer-string) "\n"))))

(defvar timestamp-actions-e2e-cases
  `((:label "t t shows an RFC 3339 stamp in a text file in every zone"
     :ext "txt" :text "deployed 2026-11-23T14:05:00Z ok\n" :search "T14"
     :keys "t t" :action "t t" :want-cmd show-timestamp-in-zones
     :want ,(timestamp-actions-e2e-table "2026-11-23T14:05:00Z" "1795442700000"))
    (:label "RET pops up a mail date in org"
     :ext "org" :text "Date: Mon, 23 Nov 2026 08:05:00 -0600\n" :search "Nov"
     :keys "RET" :action "RET" :want-cmd show-timestamp-in-zones
     :want ,(timestamp-actions-e2e-table "Mon, 23 Nov 2026 08:05:00 -0600"
                                         "1795442700000"))
    (:label "t b lists an epoch in elisp in a buffer of its own"
     :ext "el" :text "(setq ts 1795442700123)\n" :search "17954"
     :keys "t b" :action "t b" :want-cmd list-timestamp-in-zones :listed t
     :want ,(concat (timestamp-actions-e2e-table "1795442700123 (epoch ms)"
                                                 "1795442700123")
                    "\n"))
    (:label "t t dates a clock time in an org note today, UTC at night"
     :ext "org" :text "- first run tonight at 22:53 UTC, after the switch\n"
     :search "22:5" :keys "t t" :action "t t" :want-cmd show-timestamp-in-zones
     ;; the zone order puts Central between the header and this row
     :want ,(format-time-string "\nUTC       %a, %b %e  22:53  UTC\n" nil t)
     :want-night ("UTC"))
    (:label "t b keeps the night band in its buffer"
     :ext "txt" :text "deployed at 2026-11-23T03:30:00Z\n" :search "03:3"
     :keys "t b" :action "t b" :want-cmd list-timestamp-in-zones :listed t
     :want ,(concat "2026-11-23T03:30:00Z\n"
                    (format "Central   Sun, Nov 22%s  21:30  CST  local\n"
                            (timestamp-actions-e2e-year))
                    (format "UTC       Mon, Nov 23%s  03:30  UTC\n"
                            (timestamp-actions-e2e-year))
                    "epoch s   1795404600\n"
                    "epoch ms  1795404600000\n")
     :want-night ("Central" "UTC"))
    ;; org timestamps read as local time, so the zone is picked to pin it
    (:label "C-u t t shows an active org timestamp in the zone picked"
     :ext "org" :text "- call at <2026-11-23 Mon 08:05> sharp\n" :search "08:0"
     :type org-timestamp :keys "C-u t t Central RET" :action "t t"
     :want-cmd show-timestamp-in-zones
     :want ,(timestamp-actions-e2e-table "<2026-11-23 Mon 08:05> (read as Central)"
                                         "1795442700000"))
    (:label "C-u t b lists an inactive org timestamp in the zone picked"
     :ext "org" :text "- noted [2026-11-23 Mon 08:05] here\n" :search "08:0"
     :type org-timestamp :keys "C-u t b Central RET" :action "t b"
     :want-cmd list-timestamp-in-zones :listed t
     :want ,(concat (timestamp-actions-e2e-table
                     "[2026-11-23 Mon 08:05] (read as Central)" "1795442700000")
                    "\n"))
    ;; outside org nothing but this target answers for org syntax
    (:label "C-u t t shows an org timestamp in markdown in the zone picked"
     :ext "md" :text "call at `<2026-11-23 Mon 08:05>` sharp\n" :search "08:0"
     :keys "C-u t t Central RET" :action "t t" :want-cmd show-timestamp-in-zones
     :want ,(timestamp-actions-e2e-table "<2026-11-23 Mon 08:05> (read as Central)"
                                         "1795442700000"))
    (:label "C-u t t dates a bare clock time today in the zone picked"
     :ext "txt" :text "call at 16:37 today\n" :search "16:3"
     :keys "C-u t t Central RET" :action "t t" :want-cmd show-timestamp-in-zones
     :want "16:37 (read as Central, dated today)\n"
     ;; 21:37 or 22:37 in UTC, with or without daylight saving
     :want-night ("UTC"))
    ;; the injected target answers the timestamp prompt only, so the zone
    ;; prompt is still there for the keys
    (:label "C-u t t reads a zoneless stamp in the zone picked"
     :ext "md" :text "at 2026-11-23 08:05:00 sharp\n" :search "08:0"
     :keys "C-u t t Central RET" :action "t t" :want-cmd show-timestamp-in-zones
     :want ,(timestamp-actions-e2e-table "2026-11-23 08:05:00 (read as Central)"
                                         "1795442700000")))
  "One case per action key, file kind and stamp shape.
:type is the target expected under point (`timestamp' when absent),
:keys the whole key sequence typed once embark is up, :action the key
:want-cmd must sit on, :want the table that must show, :listed takes
it from the buffer the action opens, :want-night the banded rows.")

(defvar timestamp-actions-e2e-order-cases
  '((:label "a stamp inside a url"
     :ext "md" :text "see https://example.com/?from=2026-11-23T14:05:00Z now\n"
     :search "T14" :first timestamp :later url)
    (:label "a stamp inside markdown code"
     :ext "md" :text "run `2026-11-23 14:05:00` now\n"
     :search "14:0" :first timestamp :later code-snippet)
    (:label "a selected stamp"
     :ext "txt" :text "deployed 2026-11-23T14:05:00Z ok\n"
     :search "2026-11-23T14:05:00Z" :select t :first region)
    (:label "an org timestamp"
     :ext "org" :text "<2026-11-23 Mon 14:05>\n"
     :search "14:0" :first org-timestamp))
  "Where point sits, and the target that must answer for it.
:later names a target the cycle must still reach; without one the
timestamp target must not be offered at all.  :select puts the region
over the :search text.")

(defun timestamp-actions-e2e-in-case (case fn)
  "Call FN in a real file holding CASE's text, point after its :search.
The file is in its own mode, fontified and displayed, as in `e2e-act-case'."
  (let* ((file (expand-file-name (format "stamp.%s" (plist-get case :ext))
                                 e2e-work-dir))
         (buf (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buf
          (switch-to-buffer buf)
          (delete-other-windows)
          (erase-buffer)
          (insert (plist-get case :text))
          (font-lock-ensure)
          (goto-char (point-min))
          (search-forward (plist-get case :search))
          (when (plist-get case :select)
            (push-mark (match-beginning 0) t t))
          (funcall fn))
      ;; keys a case did not consume would reach the next scenario
      (discard-input)
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf))))

(defun timestamp-actions-e2e-act (case)
  "Press CASE's keys through a real `embark-act', report what showed.
A tty without child frames gets the echo area, one with them gets the
posframe, so the popup text is read from whichever holds it."
  (let ((world-clock-list timestamp-actions-e2e-zones)
        (start (with-current-buffer (messages-buffer) (point-max))))
    (unwind-protect
        (timestamp-actions-e2e-in-case
         case
         (lambda ()
           (let* ((mode major-mode)
                  (want-type (or (plist-get case :type) 'timestamp))
                  (type (plist-get (car (embark--targets)) :type))
                  (cmd (lookup-key (embark--action-keymap type nil)
                                   (kbd (plist-get case :action))))
                  (err (condition-case e
                           (let ((unread-command-events
                                  (listify-key-sequence (kbd (plist-get case :keys)))))
                             (embark-act)
                             nil)
                         (error e)))
                  (padded (when-let* ((buf (get-buffer timestamp-popup-buffer)))
                            (with-current-buffer buf
                              (buffer-substring-no-properties (point-min) (point-max)))))
                  ;; the popup pads every line, and the table is what is compared
                  (popup (when padded
                           (mapconcat #'string-trim (split-string padded "\n" t) "\n")))
                  (listed (when-let* ((buf (get-buffer "*timestamp*")))
                            (with-current-buffer buf
                              (buffer-substring-no-properties (point-min) (point-max)))))
                  (echoed (with-current-buffer (messages-buffer)
                            (buffer-substring-no-properties
                             (min start (point-max)) (point-max))))
                  (via (cond ((plist-get case :listed) "buffer")
                             ((and popup (not (string-empty-p popup))) "posframe")
                             (t "echo area")))
                  (got (pcase via
                         ("buffer" listed)
                         ("posframe" popup)
                         (_ echoed)))
                  ;; the echo log keeps no faces, so only a buffer shows the band
                  (night (pcase via
                           ("buffer" (timestamp-actions-e2e-night "*timestamp*"))
                           ("posframe" (timestamp-actions-e2e-night timestamp-popup-buffer))
                           (_ (plist-get case :want-night))))
                  (landed (buffer-name (window-buffer (selected-window)))))
             (list :label (plist-get case :label)
                   :mode mode :keys (plist-get case :keys) :cmd cmd
                   :type type :want-type want-type
                   :probe (format "via=%s landed=%s night=%S" via landed night)
                   :got got :want (plist-get case :want) :err err
                   :ok (and (null err)
                            (eq type want-type)
                            (eq cmd (plist-get case :want-cmd))
                            (stringp got)
                            (equal night (plist-get case :want-night))
                            (if (plist-get case :listed)
                                (and (equal got (plist-get case :want))
                                     (equal landed "*timestamp*"))
                              (and (string-search (plist-get case :want) got) t)))))))
      (when (memq 'hide-timestamp-popup pre-command-hook)
        (hide-timestamp-popup))
      (when (and (fboundp 'posframe-delete) (get-buffer timestamp-popup-buffer))
        (posframe-delete timestamp-popup-buffer))
      (when-let* ((buf (get-buffer "*timestamp*")))
        (kill-buffer buf)))))

(defun timestamp-actions-e2e-order (case)
  "Report the targets embark offers at CASE's point."
  (timestamp-actions-e2e-in-case
   case
   (lambda ()
     (let ((types (mapcar (lambda (tg) (plist-get tg :type)) (embark--targets)))
           (first (plist-get case :first))
           (later (plist-get case :later)))
       (deactivate-mark)
       (list :label (format "target order: %s" (plist-get case :label))
             :got types
             :want (if later
                       (format "%s first, %s later" first later)
                     (format "%s first, no timestamp" first))
             :ok (and (eq (car types) first)
                      (if later
                          (and (memq later (cdr types)) t)
                        (not (memq 'timestamp types)))))))))

(defun timestamp-actions-e2e ()
  "Drive the timestamp actions and check which target answers where."
  (require 'embark)
  (require 'which-key)
  (require 'markdown-mode)
  ;; loaded before any case binds `world-clock-list', or its `after!' would
  ;; set the case's binding instead of the global value
  (require 'time)
  (when (autoloadp (symbol-function 'show-timestamp-in-zones))
    (autoload-do-load (symbol-function 'show-timestamp-in-zones)
                      'show-timestamp-in-zones))
  ;; one case blowing up must cost that case, not the ones beside it
  (let ((guarded (lambda (fn)
                   (lambda (case)
                     (condition-case e
                         (funcall fn case)
                       (error (list :label (plist-get case :label)
                                    :err e :ok nil)))))))
    ;; the local zone decides which row is marked; the next scenario gets
    ;; the machine's own back
    (unwind-protect
        (progn
          (set-time-zone-rule "America/Chicago")
          (append
           (mapcar (funcall guarded #'timestamp-actions-e2e-order)
                   timestamp-actions-e2e-order-cases)
           (mapcar (funcall guarded #'timestamp-actions-e2e-act)
                   timestamp-actions-e2e-cases)))
      (set-time-zone-rule nil))))

(add-to-list 'e2e-scenarios #'timestamp-actions-e2e)
