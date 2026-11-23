;;; modules/embark/autoload/timestamp.el --- a timestamp in every world-clock zone -*- lexical-binding: t; -*-

;; A timestamp written in text - org style, ISO 8601 or SQL, RFC 2822, an
;; epoch number or a clock time - is read once and shown in every
;; `world-clock-list' zone.

(require 'thingatpt)
(require 'time)

(defconst timestamp-zone-offsets
  '(("Z" . 0) ("UTC" . 0) ("UT" . 0) ("GMT" . 0)
    ("PST" . -28800) ("PDT" . -25200) ("MST" . -25200) ("MDT" . -21600)
    ("CST" . -21600) ("CDT" . -18000) ("EST" . -18000) ("EDT" . -14400))
  "Zone names a timestamp may end with, as seconds east of UTC.")

(defconst timestamp-months
  '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "Month names of RFC 2822 dates, in calendar order.")

(defconst timestamp-regexp
  (let* ((names `(or ,@(mapcar #'car timestamp-zone-offsets)))
         (offset '(seq (any "+-") (= 2 digit) (? (? ":") (= 2 digit))))
         (zone `(or ,offset (seq ,names word-end)))
         (clock '(seq (group-n 4 (** 1 2 digit)) ":" (group-n 5 (= 2 digit))
                      (? ":" (group-n 6 (= 2 digit))
                         (? (any ".,") (group-n 7 (+ digit))))))
         (org-date '(seq (group-n 1 (= 4 digit)) "-" (group-n 2 (= 2 digit))
                         "-" (group-n 3 (= 2 digit)) " " (group-n 11 (+ alpha)) (? ".")))
         (org-clock '(seq (group-n 4 (** 1 2 digit)) ":" (group-n 5 (= 2 digit)))))
    (rx-to-string
     `(or ;; org's own, active or inactive, with repeaters and spans inside
          (seq (any "<[") ,org-date (? " " ,org-clock) (* (not (any "]>\n"))) (any "]>"))
          ;; the same without brackets needs a time, or a journal heading counts
          (seq ,org-date " " ,org-clock)
          (seq (group-n 1 (= 4 digit)) (any "-/") (group-n 2 (= 2 digit))
               (any "-/") (group-n 3 (= 2 digit)) (any "T ") ,clock
               (? (? " ") (group-n 8 ,zone)))
          (seq (? (= 3 alpha) ", ")
               (group-n 3 (** 1 2 digit)) " "
               (group-n 9 (or ,@timestamp-months)) " "
               (group-n 1 (= 4 digit)) " "
               (group-n 4 (= 2 digit)) ":" (group-n 5 (= 2 digit))
               (? ":" (group-n 6 (= 2 digit)))
               (? " " (group-n 8 ,zone)))
          ;; a clock time alone takes an offset only after a space: in
          ;; 14:00-16:00 UTC an attached one would read -16:00 as a zone
          (seq word-boundary ,clock
               (? (or (seq (? " ") (group-n 8 ,names) word-end)
                      (seq " " (group-n 8 ,offset)))))
          ;; seconds to nanoseconds, 2001 to 2065
          (seq word-boundary
               (group-n 10 (any "12") (= 9 digit)
                        (? (or (= 9 digit) (= 6 digit) (= 3 digit))))
               word-boundary))
     t))
  "Match a timestamp: org style, ISO 8601 or SQL, RFC 2822, clock time, epoch.
Groups 1 to 6 hold the year, month, day, hour, minute and second, 7 the
fraction of a second, 8 the zone, 9 a month name, 10 the epoch digits and
11 the weekday of an org-style date.")

(defconst timestamp-popup-buffer " *timestamp-popup*"
  "Buffer `show-timestamp-in-zones' displays its posframe from.")

(defvar timestamp-night-hours '(21 . 8)
  "Night on a zone's clock, from the hour in the car until the hour in the cdr.")

(defface timestamp-night
  '((((background light)) :background "#e6e1ea" :extend t)
    (((background dark)) :background "#14161f" :extend t))
  "Face of the `timestamp-in-zones' rows that fall at night."
  :group 'display-time)

(defun timestamp-night-p (time zone)
  "Non-nil when TIME falls in `timestamp-night-hours' on ZONE's clock."
  (pcase-let ((`(,start . ,end) timestamp-night-hours)
              (hour (string-to-number (format-time-string "%H" time zone))))
    (if (< start end)
        (and (<= start hour) (< hour end))
      (or (<= start hour) (< hour end)))))

(defun timestamp-at-point ()
  "Timestamp written at point as (STRING BEG . END), nil when there is none."
  (let ((case-fold-search nil))
    (when (thing-at-point-looking-at timestamp-regexp 40)
      `(,(match-string-no-properties 0) ,(match-beginning 0) . ,(match-end 0)))))

(defun timestamp-offset (zone)
  "Seconds east of UTC for ZONE, the zone written after a timestamp."
  (or (cdr (assoc zone timestamp-zone-offsets))
      (let* ((digits (string-replace ":" "" (substring zone 1)))
             (hours (string-to-number (substring digits 0 2)))
             (minutes (string-to-number (substring digits 2)))
             (seconds (+ (* 3600 hours) (* 60 minutes))))
        (if (string-prefix-p "-" zone) (- seconds) seconds))))

(defun parse-timestamp (string &optional zone)
  "Read the first timestamp in STRING as (TIME . NOTE), nil when there is none.
ZONE, a `world-clock-list' entry, replaces the zone STRING is written in.
Without either, machine formats read as UTC, org style and a bare clock as
local time, and a time with no date as today.  NOTE says what was assumed."
  (let ((case-fold-search nil))
    (when (string-match timestamp-regexp string)
      (pcase-let ((`(,year ,month ,day ,hour ,minute ,second ,fraction
                           ,written ,month-name ,epoch ,weekday)
                   (mapcar (lambda (n) (match-string n string))
                           (number-sequence 1 11))))
        (if epoch
            (let* ((unit (thread-first (length epoch) (- 10) (/ 3)))
                   (time (cons (string-to-number epoch) (expt 1000 unit))))
              (cons time (nth unit '("epoch s" "epoch ms" "epoch µs" "epoch ns"))))
          (let* ((ticks (string-to-number (concat second fraction)))
                 (hz (expt 10 (length fraction)))
                 ;; a weekday or a missing date marks what a person wrote, as
                 ;; org does: in local time
                 (local (or weekday (null day)))
                 (tz (cond (zone (car zone))
                           (written (timestamp-offset written))
                           (local nil)
                           (t t)))
                 (date (cond ((null day)
                              (seq-subseq (decode-time (current-time) tz) 3 6))
                             (month-name
                              (list (string-to-number day)
                                    (1+ (seq-position timestamp-months month-name))
                                    (string-to-number year)))
                             (t (mapcar #'string-to-number (list day month year)))))
                 (time (encode-time
                        `(,(cons ticks hz) ,(string-to-number (or minute "0"))
                          ,(string-to-number (or hour "0")) ,@date nil -1 ,tz)))
                 (notes (delq nil (list (cond (zone (format "read as %s" (cadr zone)))
                                              (written nil)
                                              (local "read as local time")
                                              (t "read as UTC"))
                                        (unless day "dated today")
                                        (unless hour "at 00:00")))))
            (cons time (when notes (string-join notes ", ")))))))))

(defun timestamp-in-zones (string &optional zone)
  "Lines showing the timestamp in STRING in each `world-clock-list' zone.
The zones run west to east, the one in local time marked, or added as
Local.  ZONE goes to `parse-timestamp'.  Year and seconds show only when
they say something: outside the current year, and when not zero."
  (pcase-let* ((`(,time . ,note) (or (parse-timestamp string zone)
                                    (user-error "No timestamp in %S" string)))
               ;; an org target ends in the whitespace after the timestamp
               (shown (string-trim string))
               (header (if note (format "%s (%s)" shown note) shown))
               (year (unless (equal (format-time-string "%Y" time)
                                    (format-time-string "%Y" (current-time)))
                       ", %Y"))
               (seconds (unless (equal (format-time-string "%S" time) "00") ":%S"))
               ;; fixed-width fields only (%e pads the day), so every row lines up
               (layout (concat "%a, %b %e" year "  %H:%M" seconds "  %Z"))
               (local (current-time-zone time))
               (local-p (lambda (tz) (equal (current-time-zone time tz) local)))
               (listed (time--display-world-list))
               ;; by offset at this instant, so daylight saving counts; equal
               ;; offsets keep the list's order
               (zones (seq-sort-by (lambda (zone) (car (current-time-zone time (car zone))))
                                   #'<
                                   (if (seq-some (lambda (zone) (funcall local-p (car zone)))
                                                 listed)
                                       listed
                                     (cons '(nil "Local") listed))))
               (rows (append
                      (mapcar (pcase-lambda (`(,tz ,label))
                                (list label
                                      (concat (format-time-string layout time tz)
                                              (when (and tz (funcall local-p tz))
                                                "  local"))
                                      (when (timestamp-night-p time tz) 'timestamp-night)))
                              zones)
                      `(("epoch s" ,(format-time-string "%s" time) nil)
                        ("epoch ms" ,(thread-first (time-convert time 1000)
                                                   car
                                                   number-to-string)
                         nil))))
               (width (thread-last rows
                                   (mapcar (lambda (row) (string-width (car row))))
                                   (apply #'max))))
    (concat (propertize header 'face 'bold)
            "\n"
            (mapconcat (pcase-lambda (`(,label ,text ,face))
                         (propertize (concat (truncate-string-to-width label width nil ?\s)
                                             "  " text)
                                     'face face))
                       rows "\n"))))

(defun read-timestamp-args ()
  "Read a timestamp, and with a prefix argument the zone it is written in."
  (let* ((default (car (timestamp-at-point)))
         (string (read-string (format-prompt "Timestamp" default) nil nil default))
         (zones (time--display-world-list)))
    (list string
          (when current-prefix-arg
            (rassoc (list (completing-read "Timestamp zone: "
                                           (mapcar #'cadr zones) nil t))
                    zones)))))

(defun hide-timestamp-popup ()
  "Hide the popup `show-timestamp-in-zones' put up."
  (remove-hook 'pre-command-hook #'hide-timestamp-popup)
  (posframe-hide timestamp-popup-buffer))

;;;###autoload
(defun show-timestamp-in-zones (string &optional zone)
  "Show the timestamp in STRING in every `world-clock-list' zone, at point.
With a prefix argument, ask for the ZONE STRING is written in.  The
popup goes away on the next command; a terminal frame gets the echo area."
  (interactive (read-timestamp-args))
  (let ((text (timestamp-in-zones string zone)))
    (if (and (fboundp 'posframe-workable-p) (posframe-workable-p))
        ;; the padding and the newline take the row's face, so a night
        ;; band runs the whole width of the popup
        (let ((lines (mapcar (lambda (line)
                               (propertize (concat "  " line "  \n")
                                           'face (get-text-property 0 'face line)))
                             (split-string text "\n"))))
          (posframe-show timestamp-popup-buffer
                         :string (apply #'concat "\n" lines)
                         :poshandler #'posframe-poshandler-point-bottom-left-corner
                         :border-width 1
                         :border-color (face-foreground 'shadow nil t)
                         :left-fringe 8
                         :right-fringe 8)
          (add-hook 'pre-command-hook #'hide-timestamp-popup))
      (message "%s" text))))

;;;###autoload
(defun list-timestamp-in-zones (string &optional zone)
  "List the timestamp in STRING in every `world-clock-list' zone in a buffer.
With a prefix argument, ask for the ZONE STRING is written in."
  (interactive (read-timestamp-args))
  (let ((text (timestamp-in-zones string zone)))
    (with-current-buffer (get-buffer-create "*timestamp*")
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text "\n"))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun embark-target-timestamp-at-point ()
  "Target the timestamp at point.
Off in the minibuffer, over an active region and on a bracketed timestamp
in org, where the candidate, the selection or embark-org's target answers."
  (unless (or (minibufferp) (use-region-p))
    (when-let* ((found (timestamp-at-point))
                ((not (and (derived-mode-p 'org-mode)
                           (memq (aref (car found) 0) '(?< ?\[))))))
      (cons 'timestamp found))))
