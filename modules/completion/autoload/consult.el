;;; modules/completion/autoload/consult.el -*- lexical-binding: t; -*-

;;; Narrowing a package's own picker by candidate prefix

;; The model pickers of gptel and eca offer one flat list where every
;; entry reads BACKEND<separator>MODEL, and the backend is the axis to cut
;; the list down by.  Consult narrowing does that in one key, but only
;; inside `consult--read'.  These route a `completing-read' some package
;; wrote through it, with one narrowing key per distinct prefix, and leave
;; the package's list building and result handling alone.

(defvar consult--narrow)

(defun candidate-prefix (candidate separator)
  "The text before the first SEPARATOR in CANDIDATE, nil without one.
An alist entry counts by its key: that is what the completion predicate
receives for one."
  (let ((string (if (consp candidate) (car candidate) candidate)))
    (when-let* ((end (string-search separator string)))
      (substring string 0 end))))

(defun narrowing-keys (groups &optional pins)
  "Alist of narrowing key to group, one entry per name in GROUPS.
PINS, an alist of key to group, fixes the key of the groups it names.
Every other group takes the first letter of its own name not yet in
use, so two groups sharing an initial still get distinct keys.  A group
whose letters are all taken gets no key."
  (let ((taken (mapcar #'car pins)))
    (delq nil
          (mapcar (lambda (group)
                    (or (rassoc group pins)
                        (when-let* ((key (seq-find (lambda (char)
                                                     (and (<= ?a char ?z)
                                                          (not (memq char taken))))
                                                   (downcase group))))
                          (push key taken)
                          (cons key group))))
                  groups))))

(defun prefix-narrowing (candidates separator &optional pins)
  "Consult narrowing configuration grouping CANDIDATES by prefix.
Each distinct text before SEPARATOR is one group, keyed the way
`narrowing-keys' does with PINS.  The value is for the :narrow argument
of `consult--read'."
  (let ((keys (narrowing-keys
               (seq-uniq (delq nil (mapcar (lambda (candidate)
                                             (candidate-prefix candidate separator))
                                           candidates)))
               pins)))
    (list :predicate (lambda (candidate)
                       (equal (candidate-prefix candidate separator)
                              (alist-get consult--narrow keys)))
          :keys keys)))

(defun prefix-group (separator keys)
  "A completion group function titling candidates by their prefix.
Each title names the group and, after it, the key from KEYS that
narrows to it, so the list itself shows what `consult-narrow-key'
followed by that letter does.  SEPARATOR ends the prefix."
  (let ((narrow-key (and consult-narrow-key
                         (key-description (consult--key-parse consult-narrow-key)))))
    (lambda (candidate transform)
      (if transform
          candidate
        (when-let* ((prefix (candidate-prefix candidate separator)))
          (if-let* ((key (and narrow-key (car (rassoc prefix keys)))))
              (format "%s  (%s %c)" prefix narrow-key key)
            prefix))))))

(defun default-first (collection default)
  "COLLECTION with the entry completing to DEFAULT moved to the front.
Nil when COLLECTION is not a list or holds no such entry."
  (when-let* (((listp collection))
              ((stringp default))
              (entry (assoc-string default collection)))
    (cons entry (remq entry collection))))

;;;###autoload
(defun call-with-prefix-narrowing (separator pins function &rest arguments)
  "Apply FUNCTION to ARGUMENTS with its `completing-read' narrowable by prefix.
Each `completing-read' FUNCTION performs runs through `consult--read'
with the narrowing `prefix-narrowing' derives from SEPARATOR and PINS,
so `consult-narrow-key' works in a picker some package wrote, without
touching how that package builds its list or handles the choice.

The list is shown in the order the package built it, which groups by
prefix already, under one header per group that spells out its key,
with the default entry leading it.  Handing the default to
`completing-read' instead makes vertico select the prompt as soon as
narrowing hides that entry, and RET then yields the default rather than
the first candidate in view."
  (require 'consult)
  (let* ((read completing-read-function)
         (completing-read-function
          (lambda (prompt collection &optional predicate require-match
                          initial hist default inherit-input-method)
            ;; `consult--read' reads through `completing-read' itself; that
            ;; call must reach the outer reader, not come back here.
            (let* ((completing-read-function read)
                   (ordered (default-first collection default))
                   (narrow (prefix-narrowing
                            (all-completions "" collection predicate)
                            separator pins)))
              (consult--read (or ordered collection)
                             :prompt prompt
                             :predicate predicate
                             :require-match require-match
                             :initial initial
                             :history (if (consp hist) (car hist) hist)
                             :default (unless ordered default)
                             :sort nil
                             :inherit-input-method inherit-input-method
                             :narrow narrow
                             :group (prefix-group separator
                                                  (plist-get narrow :keys)))))))
    (apply function arguments)))
