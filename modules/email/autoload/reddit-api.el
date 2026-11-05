;;; modules/email/autoload/reddit-api.el -*- lexical-binding: t; -*-
;;; Commentary:
;; Writes to Reddit as the user.  osascript runs each request in a
;; www.reddit.com tab of the running Brave, so it carries the login's
;; cookies, and reads the modhash Reddit wants quoted in a write first.
;;; Code:

(require 'subr-x)
(require 'url-util)

(defvar reddit-api-program "osascript"
  "Program that runs `reddit-api-script'.")

(defvar reddit-api-browser "Brave Browser"
  "Browser whose Reddit login writes.
It must run, and allow JavaScript from Apple Events.")

(defvar reddit-api-origin "https://www.reddit.com"
  "Origin of the tab a request goes from.")

(defconst reddit-api-script
  "function run(argv) {
  var name = argv[0], origin = argv[1], path = argv[2], body = argv[3];
  var app = Application(name);
  if (!app.running()) { throw new Error(name + ' is not running'); }
  var wins = app.windows();
  if (wins.length === 0) { throw new Error(name + ' has no window open'); }
  var tab = null, opened = null;
  for (var i = 0; i < wins.length && !tab; i++) {
    var tabs = wins[i].tabs();
    for (var j = 0; j < tabs.length && !tab; j++) {
      if ((tabs[j].url() || '').indexOf(origin + '/') === 0) { tab = tabs[j]; }
    }
  }
  if (!tab) {
    var win = wins[0], active = win.activeTabIndex();
    win.tabs.push(app.Tab({url: origin + '/robots.txt'}));
    var all = win.tabs();
    tab = opened = all[all.length - 1];
    win.activeTabIndex = active;
    for (var k = 0; k < 100 && tab.loading(); k++) { delay(0.1); }
  }
  try {
    return tab.execute({javascript: '(' + inTab + ')(' + JSON.stringify(path)
                        + ',' + JSON.stringify(body) + ')'});
  } finally {
    if (opened) { opened.close(); }
  }
}

function inTab(path, body) {
  var newline = String.fromCharCode(10);
  function send(method, url, data) {
    var request = new XMLHttpRequest();
    request.open(method, url, false);
    request.setRequestHeader('Accept', 'application/json');
    if (data !== null) {
      request.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
    }
    request.send(data);
    return request;
  }
  try {
    var me = send('GET', '/api/me.json', null);
    if (me.status !== 200) { return 'HTTP ' + me.status + newline + '/api/me.json failed'; }
    var modhash = (JSON.parse(me.responseText).data || {}).modhash;
    if (!modhash) { return 'HTTP 401' + newline + 'no Reddit login in the browser'; }
    var answer = send('POST', path, body + '&uh=' + encodeURIComponent(modhash));
    return 'HTTP ' + answer.status + newline + answer.responseText;
  } catch (error) {
    return 'HTTP 0' + newline + error;
  }
}"
  "JavaScript for Automation posting a form from a tab of a browser.
Its arguments are the browser, the origin, the path and the form; it
prints the status line \"HTTP N\" and the answer.  Without a tab on the
origin it opens one behind the active tab, and closes it after.")

;;;###autoload
(defun reddit-thing-id (message-id)
  "Reddit's fullname of the post or comment with MESSAGE-ID, or nil.
A comment's is t1_ID, a post's t3_ID."
  (when (and message-id
             (string-match "\\`<\\(?:\\([^.@<>]+\\)\\.\\)?\\([^.@<>]+\\)@reddit\\.com>\\'"
                           message-id))
    (if (match-beginning 1)
        (concat "t1_" (match-string 1 message-id))
      (concat "t3_" (match-string 2 message-id)))))

(defun reddit-api-form (params)
  "PARAMS, an alist of strings, as a form asking Reddit to answer in JSON."
  (mapconcat (pcase-lambda (`(,name . ,value))
               (concat (url-hexify-string name) "=" (url-hexify-string value)))
             (append params '(("api_type" . "json")))
             "&"))

(defun reddit-api-command (path params)
  "Command line that posts PARAMS to Reddit's PATH from `reddit-api-browser'."
  (list reddit-api-program "-l" "JavaScript" "-e" reddit-api-script
        reddit-api-browser reddit-api-origin path (reddit-api-form params)))

(defun reddit-api-reason (json text)
  "What a refused request's answer says, JSON when it parsed, else its TEXT."
  (or (and (hash-table-p json) (gethash "message" json))
      (if (string-prefix-p "<" (string-trim-left text))
          "a web page instead of JSON"
        (truncate-string-to-width (string-trim text) 200 nil nil t))))

(defun reddit-api-answer (output)
  "The OUTPUT of `reddit-api-script' as (JSON . ERROR).
JSON is Reddit's answer parsed, or nil.  ERROR is nil when Reddit took
the request, else why not, Reddit's own json.errors among the reasons."
  (if (not (string-match "^HTTP \\([0-9]+\\)\n" output))
      (cons nil (string-join (delete "" (list (format "no answer from %s" reddit-api-browser)
                                              (string-trim output)))
                             ": "))
    (let* ((status (string-to-number (match-string 1 output)))
           (text (substring output (match-end 0)))
           (json (ignore-errors
                   (json-parse-string text :object-type 'hash-table
                                      :null-object nil :false-object nil)))
           (errors (and (hash-table-p json)
                        (hash-table-p (gethash "json" json))
                        (gethash "errors" (gethash "json" json)))))
      (cons json
            (cond ((/= status 200)
                   (format "HTTP %d: %s" status (reddit-api-reason json text)))
                  ((not json)
                   (format "an answer that is no JSON: %s" (reddit-api-reason nil text)))
                  ((< 0 (length errors))
                   ;; each is [CODE MESSAGE FIELD]
                   (mapconcat (lambda (problem)
                                (format "%s: %s" (aref problem 0) (aref problem 1)))
                              errors "; ")))))))

;;;###autoload
(defun reddit-api-post (path params callback)
  "Post PARAMS, an alist of strings, to Reddit's PATH as the user.
CALLBACK gets Reddit's answer parsed, or nil, and nil when Reddit took
the request, else why not."
  (let ((buffer (generate-new-buffer " *reddit-api*")))
    (condition-case err
        (make-process
         :name "reddit-api" :buffer buffer :command (reddit-api-command path params)
         :connection-type 'pipe :coding 'utf-8 :noquery t
         :sentinel (lambda (process _event)
                     (unless (process-live-p process)
                       (let* ((output (with-current-buffer buffer (buffer-string)))
                              (answer (if (zerop (process-exit-status process))
                                          (reddit-api-answer output)
                                        (cons nil (format "%s failed: %s" reddit-api-program
                                                          (string-trim output))))))
                         (kill-buffer buffer)
                         (funcall callback (car answer) (cdr answer))))))
      (error (kill-buffer buffer)
             (funcall callback nil (format "cannot run %s: %s" reddit-api-program
                                           (error-message-string err)))))))

;;; reddit-api.el ends here
