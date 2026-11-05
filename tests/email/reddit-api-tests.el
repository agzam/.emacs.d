;;; tests/email/reddit-api-tests.el --- Reddit write path specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/email/autoload/reddit-api.el")

(defun reddit-api-tests--stand-in (dir output &optional exit)
  "An osascript under DIR printing OUTPUT and exiting EXIT.
It writes its arguments to DIR/args, each ended by a NUL byte."
  (let ((program (expand-file-name "osascript" dir)))
    (with-temp-file program
      (insert "#!/bin/sh\n"
              "for arg; do printf '%s\\0' \"$arg\"; done > '" (expand-file-name "args" dir) "'\n"
              "cat <<'EOF'\n" output "\nEOF\n"
              (format "exit %d\n" (or exit 0))))
    (set-file-modes program #o755)
    program))

(defun reddit-api-tests--args (dir)
  "Arguments the stand-in under DIR ran with."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "args" dir))
    (split-string (buffer-string) "\0" t)))

(defun reddit-api-tests--post (output &optional exit)
  "What the callback of a post gets while osascript says OUTPUT and exits EXIT.
The value is (JSON ERROR ARGS), ARGS the stand-in's arguments."
  (let* ((dir (make-temp-file "reddit-api-tests" t))
         (reddit-api-program (reddit-api-tests--stand-in dir output exit))
         got)
    (unwind-protect
        (progn
          (reddit-api-post "/api/comment" '(("thing_id" . "t1_c2") ("text" . "my reply"))
                           (lambda (json error) (setq got (list json error))))
          (with-timeout (10)
            (while (not got)
              (accept-process-output nil 0.05)))
          (append got (list (reddit-api-tests--args dir))))
      (delete-directory dir t))))

(describe "reddit-thing-id"
  (it "names a comment t1_ and a post t3_, as Reddit's fullnames do"
    (expect (reddit-thing-id "<c2.p1@reddit.com>") :to-equal "t1_c2")
    (expect (reddit-thing-id "<p1@reddit.com>") :to-equal "t3_p1"))
  (it "names no other mail"
    (expect (reddit-thing-id "<1@x>") :to-be nil)
    (expect (reddit-thing-id "<c2.p1@reddit.com.example>") :to-be nil)
    (expect (reddit-thing-id nil) :to-be nil)))

(describe "reddit-api-form"
  (it "encodes every name and value, and asks for JSON"
    (expect (reddit-api-form '(("thing_id" . "t1_c2") ("text" . "a & b = c\nnext ü")))
            :to-equal
            "thing_id=t1_c2&text=a%20%26%20b%20%3D%20c%0Anext%20%C3%BC&api_type=json")))

(describe "reddit-api-command"
  (it "runs the script in the browser on Reddit's origin, the form last"
    (let ((reddit-api-program "osascript")
          (reddit-api-browser "Brave Browser")
          (reddit-api-origin "https://www.reddit.com"))
      (expect (reddit-api-command "/api/vote" '(("id" . "t1_c2") ("dir" . "1")))
              :to-equal
              (list "osascript" "-l" "JavaScript" "-e" reddit-api-script
                    "Brave Browser" "https://www.reddit.com" "/api/vote"
                    "id=t1_c2&dir=1&api_type=json"))))
  (it "quotes the modhash /api/me.json gives in the post, which goes from the tab"
    (dolist (text '("send('GET', '/api/me.json', null)"
                    "send('POST', path, body + '&uh=' + encodeURIComponent(modhash))"
                    "tab.execute({javascript: '(' + inTab + ')('"))
      (expect reddit-api-script :to-match (regexp-quote text)))))

(describe "reddit-api-post"
  (it "hands the callback Reddit's answer and no error once Reddit took the post"
    (pcase-let ((`(,json ,error ,args)
                 (reddit-api-tests--post
                  "HTTP 200\n{\"json\": {\"errors\": [], \"data\": {\"things\": [{\"kind\": \"t1\"}]}}}")))
      (expect error :to-be nil)
      (expect (gethash "kind" (aref (gethash "things" (gethash "data" (gethash "json" json))) 0))
              :to-equal "t1")
      (expect (last args 4)
              :to-equal (list reddit-api-browser reddit-api-origin "/api/comment"
                              "thing_id=t1_c2&text=my%20reply&api_type=json"))))
  (it "says what Reddit's own errors say, its answer along"
    (pcase-let ((`(,json ,error)
                 (reddit-api-tests--post
                  (concat "HTTP 200\n{\"json\": {\"errors\": [[\"RATELIMIT\","
                          " \"Take a break for 9 minutes.\", \"ratelimit\"]]}}"))))
      (expect error :to-equal "RATELIMIT: Take a break for 9 minutes.")
      (expect (hash-table-p json) :to-be t)))
  (it "says the HTTP status of a refused post and the message it came with"
    (expect (cadr (reddit-api-tests--post "HTTP 403\n{\"message\": \"Forbidden\", \"error\": 403}"))
            :to-equal "HTTP 403: Forbidden")
    (expect (cadr (reddit-api-tests--post "HTTP 403\n<!DOCTYPE html><html>blocked</html>"))
            :to-equal "HTTP 403: a web page instead of JSON")
    (expect (cadr (reddit-api-tests--post "HTTP 401\nno Reddit login in the browser"))
            :to-equal "HTTP 401: no Reddit login in the browser"))
  (it "takes an empty answer to a vote for a yes"
    (expect (cadr (reddit-api-tests--post "HTTP 200\n{}")) :to-be nil))
  (it "says why osascript failed"
    (pcase-let ((`(,json ,error)
                 (reddit-api-tests--post
                  "execution error: Error: Brave Browser is not running (-2700)" 1)))
      (expect json :to-be nil)
      (expect error :to-match "failed: execution error: Error: Brave Browser is not running")))
  (it "says there was no answer when the script printed none"
    (expect (cadr (reddit-api-tests--post ""))
            :to-equal (format "no answer from %s" reddit-api-browser)))
  (it "calls back with the reason when osascript is missing, and leaves no buffer"
    (let ((reddit-api-program "/nonexistent/osascript")
          (before (buffer-list))
          got)
      (reddit-api-post "/api/vote" '(("id" . "t1_c2") ("dir" . "1"))
                       (lambda (json error) (setq got (list json error))))
      (expect (car got) :to-be nil)
      (expect (cadr got) :to-match "osascript")
      (expect (seq-difference (buffer-list) before) :to-equal nil)))
  (it "kills the buffer of its output once the callback has it"
    (let ((before (buffer-list)))
      (reddit-api-tests--post "HTTP 200\n{}")
      (expect (seq-filter (lambda (buffer) (string-prefix-p " *reddit-api" (buffer-name buffer)))
                          (seq-difference (buffer-list) before))
              :to-equal nil))))

;;; reddit-api-tests.el ends here
