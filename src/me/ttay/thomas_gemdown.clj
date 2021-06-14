(ns me.ttay.thomas-gemdown
  (:require
   [clojure.string :as str]
   [clojure.tools.cli :refer [parse-opts]]
   [me.ttay.thomas-gemdown.emit :as emit]
   [me.ttay.thomas-gemdown.watcher :as watcher])
  (:import (java.nio.file Files LinkOption)
           (java.nio.file.attribute FileAttribute))
  (:gen-class))

(def cli-options
  [;; First three strings describe a short-option, long-option with optional
   ;; example argument description, and a description. All three are optional
   ;; and positional.
   ["-w" "--watch DIRs" "Directory(s) to watch"
    :multi true
    :default []
    :update-fn conj]
   ["-o" "--out OUTDIR" "Directory(s) to place the output files"
    :default "gemtext"]
;; TODO: add logging levels
;;  ["-v" nil "Verbosity level; may be specified multiple times to increase value"
;;   ;; If no long-option is specified, an option :id must be given
;;   :id :verbosity
;;   :default 0
;;   ;; Use :update-fn to create non-idempotent options (:default is applied first)
;;   :update-fn inc]
;;  ;; A boolean option that can explicitly be set to false
;;  ["-d" "--[no-]daemon" "Daemonize the process" :default true]
   ;; Version string
   ;; If no required argument description is given, the option is assumed to
   ;; be a boolean option defaulting to nil
   [nil "--version" "Version"]
   ["-h" "--help"]])


;; The :default values are applied first to options. Sometimes you might want
;; to apply default values after parsing is complete, or specifically to
;; compute a default value based on other option values in the map. For those
;; situations, you can use :default-fn to specify a function that is called
;; for any options that do not have a value after parsing is complete, and
;; which is passed the complete, parsed option map as it's single argument.
;; :default-fn (constantly 42) is effectively the same as :default 42 unless
;; you have a non-idempotent option (with :update-fn or :assoc-fn) -- in which
;; case any :default value is used as the initial option value rather than nil,
;; and :default-fn will be called to compute the final option value if none was
;; given on the command-line (thus, :default-fn can override :default)

(defn usage [options-summary]
  (->> ["Thomas' Markdown to Gemini Text converter and directory watcher."
        ""
        "Usage: ./thomas_gemdown --watch DIRs --out OUTDIR"
        ""
        "Options:"
        options-summary
        ""
        "Actions:"
        "  start    Start a new server"
        "  stop     Stop an existing server"
        "  status   Print a server's status"
        ""
        "Please refer to the manual page for more information."]
       (str/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (str/join \newline errors)))

(def version "0.1.0")

(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with a error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) ; help => exit OK with usage summary
      {:exit-message (usage summary) :ok? true}
      (:version options) ; version => exit OK with version string
      {:exit-message version :ok? true} ; TODOn
      errors ; errors => exit with description of errors
      {:exit-message (error-msg errors)}
      :else
      (if (seq (:watch options))
        {:action :start-watcher :options options :arguments arguments}
        {:action :start-watcher
         :options (assoc options :watch ["content/"])
         :arguments arguments}))))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn make-output-dir-if-not-exist [out-dir]
  (let [out-path (watcher/path-of "." out-dir)]
    (when (Files/notExists out-path (into-array LinkOption []))
      (println "Output directory" out-dir "not found, creating it now.")
      (Files/createDirectory out-path (into-array FileAttribute [])))))

(defn -main [& args]
  (let [{:keys [options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (let [watch-dirs (:watch options)
            out-dir (:out options)]
        (apply println "Watching directories: " watch-dirs)
        (make-output-dir-if-not-exist out-dir)
        (run! #(watcher/parse-and-emit-dir % out-dir) watch-dirs)
        (watcher/make-watcher watch-dirs (:out options))))))

(comment
  (spit "resources/test2.gmi" (emit/file->gemtext "resources/test2.md"))
  (validate-args ["-w" "resources"])
  (validate-args ["-w" "content contetnt2" "--out" "asd"])
  (validate-args ["-w" "content contetnt2" "--out" "asd" "--version"])
  (-main "-w" "resources")
  ;;)
  )
