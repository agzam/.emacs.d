;;; tests/chat/config-tests.el --- chat/config.el binding specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

;; The batch tier has neither telega nor general, so `modules/chat/config.el'
;; can't be loaded; its `map!' forms are read and walked as data instead.
(defvar chat-tests--forms
  (with-temp-buffer
    (insert-file-contents (expand-file-name "modules/chat/config.el"
                                            test-config-root))
    (goto-char (point-min))
    (let (forms form)
      (while (setq form (ignore-errors (read (current-buffer))))
        (push form forms))
      (nreverse forms)))
  "Every top-level form of the chat module config.")

(defun chat-tests--map-forms (form)
  "Collect every `map!' subform of FORM."
  (when (proper-list-p form)
    (append (when (eq (car form) 'map!) (list form))
            (mapcan #'chat-tests--map-forms form))))

(defun chat-tests--rows (body path)
  "Walk a `map!' BODY, returning (PATH KEY DESC DEF) rows under PATH."
  (let (rows desc)
    (while body
      (let ((head (pop body)))
        (cond
         ((eq head :desc)
          (setq desc (pop body)))
         ((and (consp head) (eq (car head) :localleader))
          (setq rows (nconc rows (chat-tests--rows (cdr head) path))))
         ((and (consp head) (eq (car head) :prefix))
          (setq rows (nconc rows (chat-tests--rows (cddr head)
                                                   (append path (list (caadr head)))))))
         ((stringp head)
          (setq rows (nconc rows (list (list path head desc (pop body))))
                desc nil)))))
    rows))

(defun chat-tests--localleader (keymap)
  "Rows of the `:localleader' tree bound on KEYMAP."
  (mapcan
   (lambda (form)
     (when (eq (plist-get (cdr form) :map) keymap)
       (chat-tests--rows
        (seq-filter (lambda (x) (and (consp x) (eq (car x) :localleader)))
                    (cdr form))
        nil)))
   (mapcan #'chat-tests--map-forms chat-tests--forms)))

(defun chat-tests--unbindings (keymap)
  "Keys explicitly unbound (bound to nil) on KEYMAP."
  (mapcan
   (lambda (form)
     (when (eq (plist-get (cdr form) :map) keymap)
       (let ((body (cdr form)) keys)
         (while body
           (let ((head (pop body)))
             (when (and (stringp head) (null (car body)))
               (push head keys)))
           (pop body))
         (nreverse keys))))
   (mapcan #'chat-tests--map-forms chat-tests--forms)))

(describe "telega leader keys on message buttons"
  ;; A message button's keymap is a text property at point, which outranks
  ;; evil's state maps: whatever it binds, the leaders never see.
  (it "frees SPC and the localleader key in telega-msg-button-map"
    (expect (chat-tests--unbindings 'telega-msg-button-map)
            :to-have-same-items-as '("SPC" ","))))

(describe "telega localleader trees"
  (dolist (keymap '(telega-root-mode-map telega-chat-mode-map))
    (describe (symbol-name keymap)
      :var* ((rows (chat-tests--localleader keymap)))

      (it "binds something"
        (expect (length rows) :to-be-greater-than 10))

      (it "labels every binding for which-key"
        (expect (seq-remove (lambda (row) (stringp (nth 2 row))) rows)
                :to-equal nil))

      (it "has no key bound twice within a prefix"
        (let ((seen (make-hash-table :test #'equal))
              dupes)
          (dolist (row rows)
            (let ((path (append (nth 0 row) (list (nth 1 row)))))
              (when (gethash path seen)
                (push path dupes))
              (puthash path t seen)))
          (expect dupes :to-equal nil)))

      (it "binds telega commands as functions and keymaps as variables"
        (dolist (row rows)
          (let ((def (nth 3 row)))
            (if (eq (car-safe def) 'function)
                ;; #'telega-foo-map would look up a void function cell
                (expect (symbol-name (cadr def)) :not :to-match "-map\\'")
              ;; a bare symbol is evaluated: only keymap variables qualify
              (expect (symbol-name def) :to-match "\\`telega-.*-map\\'"))))))))
