;; Verify/repair the macOS TCC grants Emacs needs (screen capture,
;; accessibility, full disk, developer tool).
;;
;; emacs-plus is ad-hoc signed, so every rebuild mints a new cdhash, and a
;; TCC row recorded against the old build stops validating: tccd denies
;; screenshots and UI scripting while System Settings still shows the
;; toggles on.  A bundle-id row whose code requirement is the hash-free
;; `identifier "org.gnu.Emacs"` validates any build, so one write survives
;; all future rebuilds.  Writing the system TCC.db takes sudo plus SIP
;; disabled; probes route through emacsclient so tccd attributes them to
;; Emacs the same way it attributes the MCP tools this protects.

(require '[babashka.fs :as fs]
         '[babashka.process :as p]
         '[clojure.string :as str])

(when-not (str/includes? (str/lower-case (System/getProperty "os.name")) "mac")
  (println "tcc: macOS-only task; nothing to do here")
  (System/exit 0))

(def db "/Library/Application Support/com.apple.TCC/TCC.db")

(def client "org.gnu.Emacs")

;; `identifier "org.gnu.Emacs"` compiled by csreq; regenerate with:
;; printf 'identifier "org.gnu.Emacs"\n' > r.txt && csreq -r r.txt -b r.bin && xxd -p r.bin
(def wanted-csreq
  "FADE0C000000002400000001000000020000000D6F72672E676E752E456D616373000000")

;; service -> the System Settings > Privacy & Security pane that seeds its row
(def services
  {"kTCCServiceAccessibility"        "Accessibility"
   "kTCCServiceScreenCapture"        "Screen & System Audio Recording"
   "kTCCServiceDeveloperTool"        "Developer Tools"
   "kTCCServiceSystemPolicyAllFiles" "Full Disk Access"})

(defn read-rows []
  (let [{:keys [exit out err]}
        (p/shell {:continue true :out :string :err :string}
                 "sqlite3" db
                 (format "select service, auth_value, hex(csreq) from access where client='%s';"
                         client))]
    (when-not (zero? exit)
      (println "tcc: cannot read" db "-" (str/trim (str err)))
      (System/exit 1))
    (into {}
          (for [line (remove str/blank? (str/split-lines out))
                :let [[svc auth csreq] (str/split line #"\|" -1)]]
            [svc {:auth auth :csreq (str/upper-case (or csreq ""))}]))))

(defn repair! [svcs]
  (let [sql (format "update access set csreq=X'%s', auth_value=2, auth_reason=4, last_modified=strftime('%%s','now') where client='%s' and service in (%s);"
                    wanted-csreq client
                    (str/join "," (map #(format "'%s'" %) svcs)))]
    (when-not (zero? (:exit (p/shell {:continue true} "sudo" "sqlite3" db sql)))
      (println "tcc: sqlite update failed - sudo denied, or SIP guards the system TCC.db")
      (System/exit 1))
    ;; `killall tccd` silently no-ops; kickstart is the reload that works
    (when-not (zero? (:exit (p/shell {:continue true}
                                     "sudo" "launchctl" "kickstart" "-k"
                                     "system/com.apple.tccd.system")))
      (println "tcc: tccd kickstart failed; the rewritten rows may apply only after reboot"))))

(defn emacs-eval
  "FORM's printed value via emacsclient, or nil when no server answers in time."
  [form timeout-ms]
  (try
    (let [proc (p/process {:out :string :err :string} "emacsclient" "--eval" form)
          res (deref proc timeout-ms :hung)]
      (cond (= res :hung) (do (p/destroy proc) nil)
            (zero? (:exit res)) (str/trim (str (:out res)))))
    (catch Exception _ nil)))

(defn screenshot-allowed? []
  (let [png "/tmp/emacs-tcc-probe.png"]
    (fs/delete-if-exists png)
    (let [res (emacs-eval (format "(call-process \"screencapture\" nil nil nil \"-x\" \"-R\" \"0,0,40,40\" %s)"
                                  (pr-str png))
                          15000)
          ok? (and (= "0" res) (fs/exists? png) (pos? (fs/size png)))]
      (fs/delete-if-exists png)
      ok?)))

(defn accessibility-allowed? []
  ;; Finder because it always runs; a menu-bar count proves the AX read,
  ;; exit 1 is the -25211 assistive-access denial
  (= "0" (emacs-eval "(call-process \"osascript\" nil nil nil \"-l\" \"JavaScript\" \"-e\" \"Application('System Events').processes.byName('Finder').menuBars().length\")"
                     15000)))

(let [rows (read-rows)
      missing (remove #(contains? rows %) (keys services))
      stale (filter (fn [svc]
                      (when-let [{:keys [auth csreq]} (rows svc)]
                        (or (not= "2" auth) (not= wanted-csreq csreq))))
                    (keys services))
      probe-fail?
      (do (doseq [svc missing]
            (println (format "tcc: no row for %s - toggle Emacs on once under System Settings > Privacy & Security > %s, then rerun `bb tcc` to make it rebuild-proof"
                             svc (services svc))))
          (if (seq stale)
            (do (println "tcc: rewriting" (str/join ", " stale)
                         "to the rebuild-proof requirement (sudo)...")
                (repair! stale))
            (println "tcc: all rows already carry the rebuild-proof requirement"))
          (if (nil? (emacs-eval "t" 3000))
            (do (println "tcc: Emacs server not answering; live probes skipped (rows verified in the DB only)")
                false)
            (let [sc? (screenshot-allowed?)
                  ax? (accessibility-allowed?)]
              (println "tcc: screenshot from Emacs:" (if sc? "allowed" "DENIED"))
              (println "tcc: accessibility from Emacs:" (if ax? "allowed" "DENIED"))
              (when (and (seq stale) (not (and sc? ax?)))
                (println "tcc: rows were just rewritten - restart Emacs and rerun `bb tcc`"))
              (not (and sc? ax?)))))]
  (System/exit (if (or (seq missing) probe-fail?) 1 0)))
