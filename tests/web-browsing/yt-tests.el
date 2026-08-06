;;; tests/web-browsing/yt-tests.el --- web-browsing/autoload/yt.el specs -*- lexical-binding: t; -*-

(require 'test-helper
         (expand-file-name
          "helper.el"
          (locate-dominating-file (or load-file-name buffer-file-name)
                                  "helper.el")))
(require 'buttercup)

(load-module-file "modules/web-browsing/autoload/yt.el")

(describe "yt-dlp-command"
  (it "names the gnome keyring and impersonates a browser on Linux"
    ;; yt-dlp's keyring auto-detection knows only GNOME/KDE sessions;
    ;; under other DEs it falls back to plaintext and decrypts zero
    ;; Brave cookies, so every authenticated extractor breaks.  And
    ;; without impersonation, Reddit's edge rejects the system python's
    ;; TLS fingerprint no matter which request handler carries it.
    (let ((system-type 'gnu/linux))
      (expect (yt-dlp-command "https://youtu.be/xyz")
              :to-equal
              (concat "yt-dlp --verbose --restrict-filenames"
                      " --cookies-from-browser brave+gnomekeyring"
                      " --impersonate chrome"
                      " 'https://youtu.be/xyz'"))))

  (it "keeps the pre-keyring command byte-identical on macOS"
    ;; macOS reads the key from the Keychain and its yt-dlp lacks the
    ;; curl_cffi extra: an --impersonate flag there would hard-error
    (let ((system-type 'darwin))
      (expect (yt-dlp-command "https://youtu.be/xyz")
              :to-equal
              (concat "yt-dlp --verbose --restrict-filenames"
                      " --cookies-from-browser brave"
                      " 'https://youtu.be/xyz'")))))

(describe "yt-extract-video-y-entonces"
  (it "shells out with the platform-aware command"
    (spy-on 'async-shell-command)
    (spy-on 'get-buffer-process)
    (spy-on 'set-process-sentinel)
    (yt-extract-video-y-entonces "https://youtu.be/xyz")
    (expect 'async-shell-command :to-have-been-called-with
            (yt-dlp-command "https://youtu.be/xyz") "*yt-dlp*")))
