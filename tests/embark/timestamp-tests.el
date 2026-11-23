;;; tests/embark/timestamp-tests.el --- embark/autoload/timestamp.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/embark/autoload/timestamp.el")

(defvar timestamp-tests-zones
  '(("UTC" "UTC") ("America/Chicago" "Central") ("Europe/Paris" "Paris"))
  "A fixed `world-clock-list', so the tables do not follow the real one.")

(defun timestamp-tests-utc (string &optional zone)
  "The instant STRING holds, as UTC ISO 8601 with milliseconds.
ZONE goes to `parse-timestamp'."
  (when-let* ((parsed (parse-timestamp string zone)))
    (format-time-string "%FT%T.%3NZ" (car parsed) t)))

(defmacro timestamp-tests-now (decoded &rest body)
  "Run BODY with `current-time' fixed at DECODED, a `decode-time' list."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'current-time)
              (lambda () (encode-time ',decoded))))
     ,@body))

(defun timestamp-tests-forms (file)
  "Every top-level form of FILE, relative to the config root."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file test-config-root))
    (goto-char (point-min))
    (let (forms form)
      (while (setq form (ignore-errors (read (current-buffer))))
        (push form forms))
      (nreverse forms))))

(defun timestamp-tests-find (pred form)
  "Depth-first search FORM for a subform satisfying PRED."
  (cond ((funcall pred form) form)
        ((proper-list-p form)
         (seq-some (lambda (f) (timestamp-tests-find pred f)) form))))

(describe "parse-timestamp"
  (dolist (case '(;; RFC 3339, as k8s and Kibana write it
                  ("2026-11-23T14:05:00Z" "2026-11-23T14:05:00.000Z" nil)
                  ("2026-11-23T14:05:00.123456789Z" "2026-11-23T14:05:00.123Z" nil)
                  ("2026-11-23T14:05:00+05:30" "2026-11-23T08:35:00.000Z" nil)
                  ("2026-11-23T14:05:00-08" "2026-11-23T22:05:00.000Z" nil)
                  ;; the Jira API
                  ("2026-11-23T14:05:00.000+0000" "2026-11-23T14:05:00.000Z" nil)
                  ;; Python logging, comma before the milliseconds
                  ("2026-11-23 14:05:00,123" "2026-11-23T14:05:00.123Z" "read as UTC")
                  ;; Postgres, an hour-only offset
                  ("2026-11-23 14:05:00.123456+00" "2026-11-23T14:05:00.123Z" nil)
                  ("2026-11-23 14:05:00 UTC" "2026-11-23T14:05:00.000Z" nil)
                  ("2026-11-23 14:05:00 PST" "2026-11-23T22:05:00.000Z" nil)
                  ("2026-11-23 14:05:00 CDT" "2026-11-23T19:05:00.000Z" nil)
                  ("2026/11/23 14:05" "2026-11-23T14:05:00.000Z" "read as UTC")
                  ;; a one-digit hour, as notes write it
                  ("2026-11-23 9:05 UTC" "2026-11-23T09:05:00.000Z" nil)
                  ;; Go's time.String(), whose trailing name stays out
                  ("2026-11-23 14:05:00.123456789 +0000 UTC" "2026-11-23T14:05:00.123Z" nil)
                  ;; RFC 2822, mail and HTTP headers
                  ("Mon, 23 Nov 2026 14:05:00 +0000" "2026-11-23T14:05:00.000Z" nil)
                  ("3 Nov 2026 08:05 -0600" "2026-11-03T14:05:00.000Z" nil)
                  ("1795442700" "2026-11-23T14:05:00.000Z" "epoch s")
                  ("1795442700123" "2026-11-23T14:05:00.123Z" "epoch ms")
                  ("1795442700123456" "2026-11-23T14:05:00.123Z" "epoch µs")
                  ("1795442700123456789" "2026-11-23T14:05:00.123Z" "epoch ns")))
    (pcase-let ((`(,string ,instant ,note) case))
      (it (format "reads %s" string)
        (expect (timestamp-tests-utc string) :to-equal instant)
        (expect (cdr (parse-timestamp string)) :to-equal note))))

  (it "takes no word for a zone name"
    (expect (parse-timestamp "2026-11-23 14:05:00 ESTIMATE")
            :to-equal (parse-timestamp "2026-11-23 14:05:00")))

  (dolist (string '("2026-11-23"              ; a date alone
                    "17954427001"             ; no epoch length
                    "3795442700"              ; an epoch past 2065
                    "2026-11-23 Mon"          ; a journal heading, no time
                    "nothing here"))
    (it (format "finds nothing in %S" string)
      (expect (parse-timestamp string) :to-be nil)))

  ;; 01:00 UTC on November 24 is still November 23 in the US
  (it "dates a clock time with a zone name today, in that zone"
    (timestamp-tests-now (0 0 1 24 11 2026 nil nil t)
      (expect (timestamp-tests-utc "22:53 UTC") :to-equal "2026-11-24T22:53:00.000Z")
      (expect (timestamp-tests-utc "22:53Z") :to-equal "2026-11-24T22:53:00.000Z")
      (expect (timestamp-tests-utc "22:53:10.250 GMT")
              :to-equal "2026-11-24T22:53:10.250Z")
      (expect (timestamp-tests-utc "9:05 PST") :to-equal "2026-11-23T17:05:00.000Z")
      (expect (timestamp-tests-utc "22:53 +0000") :to-equal "2026-11-24T22:53:00.000Z")
      (expect (cdr (parse-timestamp "22:53 UTC")) :to-equal "dated today")))

  (it "reads a bare clock time as local time today"
    (timestamp-tests-now (0 0 18 23 11 2026 nil nil t)
      (unwind-protect
          (progn
            (set-time-zone-rule "America/Chicago")
            (expect (timestamp-tests-utc "16:37") :to-equal "2026-11-23T22:37:00.000Z")
            (expect (cdr (parse-timestamp "16:37"))
                    :to-equal "read as local time, dated today"))
        (set-time-zone-rule nil))))

  (it "dates a clock time today in the zone it is given"
    (timestamp-tests-now (0 0 1 24 11 2026 nil nil t)
      (expect (timestamp-tests-utc "22:53 UTC" '("America/Chicago" "Central"))
              :to-equal "2026-11-24T04:53:00.000Z")
      (expect (cdr (parse-timestamp "22:53 UTC" '("America/Chicago" "Central")))
              :to-equal "read as Central, dated today")))

  ;; org writes local time; the local zone is pinned so the instant is known
  (it "reads an org timestamp, active or inactive, as local time"
    (unwind-protect
        (progn
          (set-time-zone-rule "America/Chicago")
          (expect (timestamp-tests-utc "<2026-11-23 Mon 14:05> ")
                  :to-equal "2026-11-23T20:05:00.000Z")
          (expect (timestamp-tests-utc "[2026-11-23 Mon 14:05]")
                  :to-equal "2026-11-23T20:05:00.000Z")
          ;; the same written without brackets, as a chat or a note has it
          (expect (timestamp-tests-utc "2026-11-23 Mon 14:05")
                  :to-equal "2026-11-23T20:05:00.000Z")
          (expect (cdr (parse-timestamp "[2026-11-23 Mon 14:05]"))
                  :to-equal "read as local time"))
      (set-time-zone-rule nil)))

  (it "reads an org timestamp in the zone it is given"
    (expect (timestamp-tests-utc "<2026-11-23 Mon 14:05>" '("Europe/Paris" "Paris"))
            :to-equal "2026-11-23T13:05:00.000Z")
    (expect (cdr (parse-timestamp "<2026-11-23 Mon 14:05>" '("Europe/Paris" "Paris")))
            :to-equal "read as Paris"))

  (it "starts an org timestamp without a time at midnight"
    (expect (timestamp-tests-utc "[2026-11-24 Tue]" '("UTC" "UTC"))
            :to-equal "2026-11-24T00:00:00.000Z")
    (expect (cdr (parse-timestamp "[2026-11-24 Tue]" '("UTC" "UTC")))
            :to-equal "read as UTC, at 00:00"))

  (it "takes the start of an org range, repeater or time span"
    (expect (timestamp-tests-utc "<2026-11-23 Mon>--<2026-11-24 Tue>" '("UTC" "UTC"))
            :to-equal "2026-11-23T00:00:00.000Z")
    (expect (timestamp-tests-utc "<2026-11-23 Mon 14:05-15:30 +1w>" '("UTC" "UTC"))
            :to-equal "2026-11-23T14:05:00.000Z"))

  (it "reads a zoneless clock time in the zone it is given, DST included"
    (expect (timestamp-tests-utc "2026-07-23 14:05:00" '("Europe/Paris" "Paris"))
            :to-equal "2026-07-23T12:05:00.000Z")
    (expect (cdr (parse-timestamp "2026-07-23 14:05:00" '("Europe/Paris" "Paris")))
            :to-equal "read as Paris"))

  (it "lets the given zone replace the written one"
    (expect (timestamp-tests-utc "2026-11-23T14:05:00Z" '("America/Chicago" "Central"))
            :to-equal "2026-11-23T20:05:00.000Z"))

  (it "keeps an epoch where it is, whatever zone it is given"
    (expect (parse-timestamp "1795442700" '("Europe/Paris" "Paris"))
            :to-equal (parse-timestamp "1795442700"))))

(describe "timestamp-at-point"
  (cl-flet ((at (text search)
              (with-temp-buffer
                (insert text)
                (goto-char (point-min))
                (search-forward search)
                (timestamp-at-point))))

    (it "returns the timestamp with its bounds"
      (with-temp-buffer
        (insert "deployed 2026-11-23T14:05:00Z ok")
        (goto-char (point-min))
        (search-forward "T14")
        (pcase-let ((`(,string ,beg . ,end) (timestamp-at-point)))
          (expect string :to-equal "2026-11-23T14:05:00Z")
          (expect (buffer-substring-no-properties beg end) :to-equal string))))

    (it "reaches back to an RFC 2822 weekday and on to its zone"
      (expect (car (at "Date: Mon, 23 Nov 2026 14:05:00 +0000\n" "Mo"))
              :to-equal "Mon, 23 Nov 2026 14:05:00 +0000")
      (expect (car (at "Date: Mon, 23 Nov 2026 14:05:00 +0000\n" "+00"))
              :to-equal "Mon, 23 Nov 2026 14:05:00 +0000"))

    (it "picks the timestamp under point when a line holds two"
      (expect (car (at "from 2026-11-23 10:00:00 to 2026-11-23 15:00:00" "to 2026-11-23 15"))
              :to-equal "2026-11-23 15:00:00"))

    (it "finds one inside a url"
      (expect (car (at "https://kibana/app?_g=(time:(from:'2026-11-23T14:05:00.000Z'))" "T14"))
              :to-equal "2026-11-23T14:05:00.000Z"))

    (it "takes a clock time with the zone name after it"
      (expect (car (at "first run tonight at 22:53 UTC, after the switch" "22:5"))
              :to-equal "22:53 UTC")
      (expect (car (at "first run tonight at 22:53 UTC, after the switch" "UT"))
              :to-equal "22:53 UTC"))

    (it "takes a bare clock time, and a range as two times"
      (expect (car (at "call at 16:37 today" "16:3")) :to-equal "16:37")
      (expect (car (at "window 14:00-16:00 UTC tonight" "14:0")) :to-equal "14:00")
      (expect (car (at "window 14:00-16:00 UTC tonight" "16:0"))
              :to-equal "16:00 UTC"))

    ;; brackets around a log stamp stay out, so it never reads as org's local time
    (it "takes a bracketed log stamp without its brackets"
      (expect (car (at "[2026-11-23 14:05:00,123] INFO started" "14:0"))
              :to-equal "2026-11-23 14:05:00,123"))

    (it "takes an org timestamp with its brackets, or written without them"
      (expect (car (at "call <2026-11-23 Mon 14:05> sharp" "14:0"))
              :to-equal "<2026-11-23 Mon 14:05>")
      (expect (car (at "noted [2026-11-24 Tue] here" "11-24"))
              :to-equal "[2026-11-24 Tue]")
      (expect (car (at "sent 2026-11-23 Mon 16:37 here" "16:3"))
              :to-equal "2026-11-23 Mon 16:37"))

    (it "takes an epoch number whole"
      (expect (car (at "{\"ts\":1795442700123,\"id\":7}" "17954"))
              :to-equal "1795442700123"))

    (it "leaves other numbers alone"
      (expect (at "call 5551234567 now" "5551") :to-be nil))

    (it "matches case-sensitively whatever the buffer says"
      (with-temp-buffer
        (setq-local case-fold-search t)
        (insert "at 2026-11-23 14:05:00 est")
        (goto-char (point-min))
        (search-forward "14:0")
        (expect (car (timestamp-at-point)) :to-equal "2026-11-23 14:05:00")))))

(describe "embark-target-timestamp-at-point"
  (it "targets the timestamp under point"
    (with-temp-buffer
      (insert "at 2026-11-23T14:05:00Z ok")
      (goto-char (point-min))
      (search-forward "14:0")
      (expect (embark-target-timestamp-at-point)
              :to-equal '(timestamp "2026-11-23T14:05:00Z" 4 . 24))))

  (it "leaves an active region to the region finder"
    (with-temp-buffer
      (let ((transient-mark-mode t))
        (insert "at 2026-11-23T14:05:00Z ok")
        (push-mark (point-min) t t)
        (goto-char (point-min))
        (search-forward "14:0")
        (expect (use-region-p) :to-be-truthy)
        (expect (embark-target-timestamp-at-point) :to-be nil))))

  (it "leaves a bracketed timestamp in org to embark-org's target"
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "- call <2026-11-23 Mon 14:05> or 2026-11-23 Mon 16:37")
      (goto-char (point-min))
      (search-forward "14:0")
      (expect (embark-target-timestamp-at-point) :to-be nil)
      (search-forward "16:3")
      (expect (cadr (embark-target-timestamp-at-point))
              :to-equal "2026-11-23 Mon 16:37")))

  ;; in the minibuffer embark keeps only the first finder that answers, so
  ;; an answer here would hide the completion candidates altogether
  (it "stays out of the minibuffer"
    (with-temp-buffer
      (insert "1795442700")
      (cl-letf (((symbol-function 'minibufferp) (lambda (&rest _) t)))
        (expect (embark-target-timestamp-at-point) :to-be nil)))))

(describe "timestamp-in-zones"
  ;; the local zone decides which row is marked, so it is pinned
  (before-each (set-time-zone-rule "America/Chicago"))
  (after-each (set-time-zone-rule nil))

  (it "lists every zone, then the epoch, under the timestamp"
    (timestamp-tests-now (0 0 12 23 11 2026 nil nil t)
      (let ((world-clock-list timestamp-tests-zones))
        (expect (substring-no-properties
                 (timestamp-in-zones "2026-11-23T08:05:00.250-06:00"))
                :to-equal
                (concat "2026-11-23T08:05:00.250-06:00\n"
                        "Central   Mon, Nov 23  08:05  CST  local\n"
                        "UTC       Mon, Nov 23  14:05  UTC\n"
                        "Paris     Mon, Nov 23  15:05  CET\n"
                        "epoch s   1795442700\n"
                        "epoch ms  1795442700250")))))

  (cl-flet ((labels-at (decoded zones)
              (let* ((world-clock-list zones)
                     (stamp (format-time-string "%FT%TZ" (encode-time decoded) t))
                     (rows (cdr (split-string (timestamp-in-zones stamp) "\n"))))
                (thread-last rows
                             (mapcar (lambda (row) (car (split-string row))))
                             (seq-take-while (lambda (label) (not (equal label "epoch"))))))))

    (it "runs the zones west to east, equal offsets in the list's order"
      (expect (labels-at '(0 0 12 23 11 2026 nil nil t)
                         '(("Europe/Paris" "Paris") ("Asia/Seoul" "Seoul")
                           ("America/Chicago" "Central") ("Europe/Copenhagen" "Copenhagen")
                           ("UTC" "UTC")))
              :to-equal '("Central" "UTC" "Paris" "Copenhagen" "Seoul")))

    ;; London matches Lagos in summer only, so the order follows the instant;
    ;; London is local here, so no Local row joins the two
    (it "takes the offsets at the instant shown, daylight saving included"
      (set-time-zone-rule "Europe/London")
      (let ((zones '(("Africa/Lagos" "Lagos") ("Europe/London" "London"))))
        (expect (labels-at '(0 0 12 15 1 2026 nil nil t) zones)
                :to-equal '("London" "Lagos"))
        (expect (labels-at '(0 0 12 15 7 2026 nil nil t) zones)
                :to-equal '("Lagos" "London")))))

  (it "carries the year only outside the current year"
    (timestamp-tests-now (0 0 12 5 1 2027 nil nil t)
      (let ((world-clock-list '(("UTC" "UTC"))))
        (expect (timestamp-in-zones "2026-11-23T14:05:00Z")
                :to-match "^UTC       Mon, Nov 23, 2026  14:05  UTC$"))))

  (it "carries the seconds only when they are not zero"
    (timestamp-tests-now (0 0 12 23 11 2026 nil nil t)
      (let ((world-clock-list '(("UTC" "UTC"))))
        (expect (timestamp-in-zones "2026-11-23T14:05:37Z")
                :to-match "^UTC       Mon, Nov 23  14:05:37  UTC$"))))

  ;; one-digit days and hours on either side of midnight are where a
  ;; variable-width field would show
  (it "keeps every zone row one width"
    (timestamp-tests-now (0 0 12 23 11 2026 nil nil t)
      (let* ((world-clock-list timestamp-tests-zones)
             (rows (thread-first (timestamp-in-zones "2026-11-03T04:05:00Z")
                                 (split-string "\n")
                                 (seq-subseq 1 4))))
        (expect rows :to-equal '("Central   Mon, Nov  2  22:05  CST  local"
                                 "UTC       Tue, Nov  3  04:05  UTC"
                                 "Paris     Tue, Nov  3  05:05  CET"))
        ;; the local marker trails the zone name, past the aligned columns
        (expect (thread-last rows
                             (mapcar (lambda (row) (length (string-remove-suffix "  local" row))))
                             (seq-uniq))
                :to-equal '(33)))))

  (it "marks the zone that shows local time"
    (timestamp-tests-now (0 0 12 23 11 2026 nil nil t)
      (let ((world-clock-list timestamp-tests-zones))
        (expect (seq-filter (lambda (row) (string-suffix-p "  local" row))
                            (split-string (timestamp-in-zones "2026-11-23T14:05:00Z") "\n"))
                :to-equal '("Central   Mon, Nov 23  08:05  CST  local")))))

  (it "adds a Local row, in its place, when no zone shows local time"
    (set-time-zone-rule "Asia/Tokyo")
    (timestamp-tests-now (0 0 12 23 11 2026 nil nil t)
      (let* ((world-clock-list timestamp-tests-zones)
             (rows (split-string (timestamp-in-zones "2026-11-23T14:05:00Z") "\n")))
        (expect (mapcar (lambda (row) (car (split-string row))) (seq-subseq rows 1 5))
                :to-equal '("Central" "UTC" "Paris" "Local"))
        (expect (nth 4 rows) :to-equal "Local     Mon, Nov 23  23:05  JST"))))

  (it "drops the whitespace an org target ends in from the header"
    (let ((world-clock-list '(("UTC" "UTC"))))
      (expect (car (split-string
                    (timestamp-in-zones "<2026-11-23 Mon 14:05> " '("UTC" "UTC"))
                    "\n"))
              :to-equal "<2026-11-23 Mon 14:05> (read as UTC)")))

  (it "says in the header how it read the zone"
    (let* ((world-clock-list timestamp-tests-zones)
           (text (timestamp-in-zones "2026-11-23 14:05:00")))
      (expect (car (split-string text "\n"))
              :to-equal "2026-11-23 14:05:00 (read as UTC)")
      (expect (get-text-property 0 'face text) :to-be 'bold)))

  (it "bands the zones where it is night, and nothing else"
    (timestamp-tests-now (0 0 12 23 11 2026 nil nil t)
      (let ((world-clock-list timestamp-tests-zones))
        ;; 22:00 UTC is 16:00 in Chicago and 23:00 in Paris
        (expect (mapcar (lambda (line) (get-text-property 0 'face line))
                        (split-string (timestamp-in-zones "2026-11-23T22:00:00Z") "\n"))
                :to-equal '(bold nil timestamp-night timestamp-night nil nil)))))

  (it "falls back to the zoneinfo list when `world-clock-list' is t"
    (let ((world-clock-list t))
      (expect (timestamp-in-zones "2026-11-23T14:05:00Z") :to-match "^Seattle ")))

  (it "refuses text without a timestamp"
    (expect (timestamp-in-zones "nothing here") :to-throw 'user-error)))

(describe "timestamp-night-p"
  (cl-flet ((night-at (hour &optional zone)
              (timestamp-night-p (encode-time `(0 30 ,hour 23 11 2026 nil nil t))
                                 (or zone t))))
    (it "counts 21:00 to 08:00 as night"
      (expect (mapcar #'night-at '(7 8 12 20 21 23 0))
              :to-equal '(t nil nil nil t t t)))

    (it "reads the hour on the zone's own clock"
      ;; 03:30 UTC is 21:30 in Chicago, 09:30 UTC is 03:30 there
      (expect (night-at 3 "America/Chicago") :to-be-truthy)
      (expect (night-at 9 "America/Chicago") :to-be-truthy)
      (expect (night-at 15 "America/Chicago") :to-be nil))

    (it "takes a night that does not wrap past midnight"
      (let ((timestamp-night-hours '(0 . 6)))
        (expect (mapcar #'night-at '(23 0 5 6))
                :to-equal '(nil t t nil))))))

(describe "read-timestamp-args"
  (it "offers the timestamp at point and asks for no zone"
    (with-temp-buffer
      (insert "at 2026-11-23T14:05:00Z")
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt _initial _history default) default)))
        (let ((current-prefix-arg nil))
          (expect (read-timestamp-args)
                  :to-equal '("2026-11-23T14:05:00Z" nil))))))

  (it "offers the current time when no timestamp is at point"
    (timestamp-tests-now (0 0 12 23 11 2026 nil nil t)
      (unwind-protect
          (with-temp-buffer
            (set-time-zone-rule "America/Chicago")
            (insert "nothing to read here")
            (cl-letf (((symbol-function 'read-string)
                       (lambda (_prompt _initial _history default) default)))
              (let* ((current-prefix-arg nil)
                     (args (read-timestamp-args)))
                (expect args :to-equal '("2026-11-23T06:00-0600" nil))
                ;; the default reads back as the instant it names
                (expect (timestamp-tests-utc (car args))
                        :to-equal "2026-11-23T12:00:00.000Z"))))
        (set-time-zone-rule nil))))

  (it "asks for the zone with a prefix argument"
    (let ((world-clock-list timestamp-tests-zones)
          (current-prefix-arg '(4))
          offered)
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "2026-11-23 14:05"))
                ((symbol-function 'completing-read)
                 (lambda (_prompt labels &rest _) (setq offered labels) "Central")))
        (expect (read-timestamp-args)
                :to-equal '("2026-11-23 14:05" ("America/Chicago" "Central")))
        (expect offered :to-equal '("UTC" "Central" "Paris"))))))

(describe "show-timestamp-in-zones"
  (before-each (set-time-zone-rule "America/Chicago"))
  (after-each (set-time-zone-rule nil))

  (it "pops the table up at point, padded, until the next command"
    (let ((world-clock-list timestamp-tests-zones)
          (pre-command-hook nil)
          shown hidden)
      (cl-letf (((symbol-function 'current-time)
                 (lambda () (encode-time '(0 0 12 23 11 2026 nil nil t))))
                ((symbol-function 'posframe-workable-p) (lambda () t))
                ((symbol-function 'posframe-show)
                 (lambda (buffer &rest args) (setq shown (cons buffer args))))
                ((symbol-function 'posframe-hide)
                 (lambda (buffer) (setq hidden buffer))))
        (show-timestamp-in-zones "2026-11-23T14:05:00Z")
        (expect (car shown) :to-equal timestamp-popup-buffer)
        (expect (plist-get (cdr shown) :string)
                :to-equal
                (concat "\n"
                        "  2026-11-23T14:05:00Z  \n"
                        "  Central   Mon, Nov 23  08:05  CST  local  \n"
                        "  UTC       Mon, Nov 23  14:05  UTC  \n"
                        "  Paris     Mon, Nov 23  15:05  CET  \n"
                        "  epoch s   1795442700  \n"
                        "  epoch ms  1795442700000  \n"))
        (expect pre-command-hook :to-equal '(hide-timestamp-popup))
        ;; a night row's padding and newline carry its face, so the band
        ;; reaches both edges of the popup
        (show-timestamp-in-zones "2026-11-23T22:00:00Z")
        (let* ((popup (plist-get (cdr shown) :string))
               (start (string-search "  UTC" popup))
               (end (string-search "\n" popup start)))
          (expect (get-text-property start 'face popup) :to-be 'timestamp-night)
          (expect (get-text-property end 'face popup) :to-be 'timestamp-night)
          (expect (get-text-property (string-search "  Central" popup) 'face popup)
                  :to-be nil))
        (run-hooks 'pre-command-hook)
        (expect hidden :to-equal timestamp-popup-buffer)
        (expect pre-command-hook :to-be nil))))

  (it "uses the echo area where no posframe can show"
    (let ((world-clock-list timestamp-tests-zones)
          (pre-command-hook nil)
          echoed)
      (cl-letf (((symbol-function 'posframe-workable-p) (lambda () nil))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
        (show-timestamp-in-zones "2026-11-23T14:05:00Z")
        (expect echoed :to-equal (timestamp-in-zones "2026-11-23T14:05:00Z"))
        (expect pre-command-hook :to-be nil)))))

(describe "list-timestamp-in-zones"
  (it "shows the table in a read-only buffer it reuses"
    (let ((world-clock-list timestamp-tests-zones))
      (unwind-protect
          (save-window-excursion
            (list-timestamp-in-zones "1795442700")
            (list-timestamp-in-zones "2026-11-23T14:05:00Z")
            (with-current-buffer "*timestamp*"
              (expect (buffer-string)
                      :to-equal
                      (concat (timestamp-in-zones "2026-11-23T14:05:00Z") "\n"))
              (expect major-mode :to-be 'special-mode)
              (expect buffer-read-only :to-be t)
              (expect (point) :to-equal (point-min)))
            (expect (buffer-name (window-buffer (selected-window)))
                    :to-equal "*timestamp*"))
        (when-let* ((buf (get-buffer "*timestamp*")))
          (kill-buffer buf))))))

;; config.el cannot load in batch (real embark keymaps at `map!' time), so
;; the wiring is read as data
(describe "timestamp wiring"
  :var* ((embark-forms (cons 'progn (timestamp-tests-forms "modules/embark/config.el")))
         (pairs (mapcan #'map-form-key-pairs
                        (map-form-groups embark-forms 'embark-timestamp-map)))
         (time-keys (map-form-prefix-pairs embark-forms 'embark-timestamp-map "t")))

  (it "registers the finder with the other text finders"
    (expect (timestamp-tests-find
             (lambda (f) (and (eq (car-safe f) 'quote)
                              (memq 'embark-target-timestamp-at-point
                                    (ensure-list (cadr f)))
                              (memq 'embark-target-RFC-number-at-point
                                    (ensure-list (cadr f)))))
             embark-forms)
            :to-be-truthy))

  (it "maps the timestamp type to its keymap"
    (expect (timestamp-tests-find
             (lambda (f) (equal f ''(timestamp . embark-timestamp-map)))
             embark-forms)
            :to-be-truthy))

  (it "gives embark-org's timestamps the same keymap"
    (expect (timestamp-tests-find
             (lambda (f) (equal f ''(org-timestamp . embark-timestamp-map)))
             embark-forms)
            :to-be-truthy))

  (it "pops up on RET and on t t, lists in a buffer on t b"
    (expect (cdr (assoc "RET" pairs)) :to-equal '(function show-timestamp-in-zones))
    (expect (cdr (assoc "t" time-keys)) :to-equal '(function show-timestamp-in-zones))
    (expect (cdr (assoc "b" time-keys)) :to-equal '(function list-timestamp-in-zones)))

  (it "lists UTC among the world-clock zones"
    (let ((zones (cadr (nth 2 (timestamp-tests-find
                               (lambda (f) (and (eq (car-safe f) 'setopt)
                                                (eq (cadr f) 'world-clock-list)))
                               (cons 'progn (timestamp-tests-forms "config.el")))))))
      (expect (assoc "UTC" zones) :to-equal '("UTC" "UTC")))))
