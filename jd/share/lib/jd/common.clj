#_(comment
    ;; in your jd-script:
    (babashka.classpath/add-classpath (str (System/getenv "_JD_ROOT") "/lib"))
    ;; in other scripts in this repo
    (babashka.classpath/add-classpath (str (babashka.fs/parent (babashka.fs/parent *file*)) "/..//etc/lib/bb"))
    (require '[ps.common :as common :refer [log logf]]))

#_{:clj-kondo/ignore [:namespace-name-mismatch]}
(ns jd.common
  (:require
   clojure.pprint
   [babashka.http-client :as http]
   [babashka.fs :as fs]
   [babashka.process :as p]
   [selmer.parser :as tmpl]
   [clj-yaml.core :as yaml]
   [cheshire.core :as json]
   [clojure.string :as str]
   [clojure.tools.cli :refer [parse-opts]])
  (:import [java.util Base64]))

;; 'logging'
;; some code adapted from https://github.com/ams-clj/clansi/blob/master/src/clansi/core.clj (MIT license)

(def ANSI-CODES
  {:reset "[0m"
   :bold "[1m"
   :default "[39m"
   :white "[37m"
   :black "[30m"
   :red "[31m"
   :green "[32m"
   :blue "[34m"
   :yellow "[33m"
   :magenta "[35m"
   :cyan "[36m"})

(defn colorize [color s]
  (if (System/getenv "NO_COLOR")
    s
    (if-let [color (get ANSI-CODES color)]
      (str "\u001b" color
           s
           "\u001b" (:reset ANSI-CODES))
      s)))

(def log-level->color-str
  {:error #(colorize :red "ERROR")
   :warn #(colorize :yellow "WARN")
   :info #(colorize :blue "INFO")
   :debug #(colorize :cyan "DEBUG")
   :other #(colorize :magenta "?????")})

(defn script-name []
  (-> *file*
      fs/normalize
      fs/file-name))

;; declaring here so that we can use these in the log function
(declare debug?)
(declare quiet?)

(defn log
  "Simple logger with nice formatting of log levels. Respects DEBUG env var for :debug
  Will always print all other log levels"
  [level msg]
  (when-not quiet?
    (when (or (not= :debug level)
              (and debug? (= :debug level)))
      (locking *err*
        (binding [*out* *err*]
          (println (str (script-name) ": [" ((get log-level->color-str level)) "] ") msg))))))

(def debug?
  (when (System/getenv "DEBUG")
    (log :warn "Debug mode enabled")
    true))

(def quiet?
  (when (System/getenv "QUIET")
    true))

(defn logf
  "Simple logger with nice formatting of log levels and printf style formatting"
  [level msg & args]
  (log level (apply format msg (map str args))))

(defn dump [level thing]
  (log level (with-out-str
               (clojure.pprint/pprint thing))))

(defn pp [thing]
  (clojure.pprint/pprint thing)
  thing)

;; execution helpers
(defn current-file-dir []
  (-> *file*
      fs/normalize
      fs/parent))

(defn run-main?
  "Check if this file is being run as a script"
  []
  (= *file* (System/getProperty "babashka.file")))

;; CLI args handling
(defn build-cli-args-handler
  "Wrapper around parse-opts to handle CLI args.
  Returns a function that will handle the CLI args - the function accepts vector of args
  When invoked it will:
  - parse the args
  - in case `:help` is set, args are empty or there are errors, print help and exit with an error code

  Otherwise, it will return a map of `{:options :sumamry :errors}.
  You don't need to specify `-h` option - it will do it for you

  Builder args::
  - cli-options - the options to parse, has to follow format of clojure.tools.cli/pare-opts
  - description - string, what does this script do?
  "
  [{:keys [cli-options
           allow-no-args?
           description]}]

  (let [has-help-flag? (some (fn [[short long & _rest]]
                               (or (= short "-h")
                                   (= long "--help")))
                             cli-options)
        cli-options (concat
                     (when-not has-help-flag?
                       [["-h" "--help" "Help!"]])
                     cli-options)]
    (fn args-handler [command-line-args]
      ;; render completions before anything else happens
      (when (= "--complete" (first command-line-args))
        (->> cli-options
             (map (fn [[short-opt long-opt description & _rest]]
                    (let [long-opt-name (str/replace (str long-opt) #"\s.*" "")]
                      (when long-opt
                        (println (format "%s[%s]" long-opt-name description)))
                      (when short-opt
                        (println (format "%s[%s]" short-opt description))))))
             doall)
        (System/exit 0))

      ;; allow for run-time control of how the CLI args are handled
      (let [[handler-opts command-line-args] (if (map? (first command-line-args))
                                               [(first command-line-args)
                                                (rest command-line-args)]
                                               [{} command-line-args])
            {:keys [exit-on-help?] :or {exit-on-help? true}} handler-opts

            {:keys [options errors summary arguments] :as _res} (parse-opts command-line-args cli-options)
            print-help? (or (:help options)
                            (when-not allow-no-args?
                              (not (seq command-line-args)))
                            (seq errors))]

        (when print-help?
          (println description)
          (println "Usage:")
          (println summary)
          (when (seq errors)
            (println "Errors:")
            (doseq [err errors]
              (println "  " err))
            (System/exit 1))
          ;; in some situations we don't want to always exit when help flag is handled
          (when exit-on-help?
            (System/exit 0)))
        (when (and (seq arguments) (not= (count arguments) 1)) ; allow a single subcommand
          (println "Error: Only one subcommand is allowed")
          (println summary)
          (System/exit 1))

        {:options options
         :subcommand (some-> arguments seq first)
         :summary summary
         :errors errors}))))

;; Process helpers

(defn process-exec
  "Like exec 3 - runs a command which will replace current process
  Can optionally accept {:extra-env ENV-MAP} as first argument to add environment variables"
  [& args]
  (let [[opts cmd] (if (map? (first args))
                     [(first args) (second args)]
                     [{} (first args)])
        extra-env (:extra-env opts)
        cmd-vec (cond
                  ;; If it's a string, run through shell
                  (string? cmd) ["/bin/bash" "-c" cmd]
                  ;; If it's already a vector, use as-is
                  (vector? cmd) cmd
                  ;; Otherwise join and run through shell
                  :else ["/bin/bash" "-c" (str/join " " cmd)])
        exec-opts (cond-> {}
                    extra-env (assoc :extra-env extra-env))]
    (p/exec cmd-vec exec-opts)))

(defn process-run-sync
  "Run a command, return a map of `{:exit :out :err}`"
  [cmd & [{:keys [dir] :as _opts}]]
  (let [cmd (if (string? cmd)
              cmd
              (str/join " " cmd))
        _ (when debug?
            (log :debug (str "Running: " cmd
                             (when dir (str " [in directory " dir "]")))))
        bb-shell-opts (cond-> {:out :string :err :string :continue true}
                        (and (string? dir) (not-empty dir)) (assoc :dir dir))
        {:keys [exit out err]} (babashka.process/shell bb-shell-opts cmd)]
    {:cmd cmd
     :exit exit
     :success? (zero? exit)
     :out out
     :err err}))

(defn return-output-or-fail!
  "Takes a map of `{:exit :out :err}` and checks if the command was successful.
  If it was return a map of `{:success? true :exit :out :err}`
  otherwise print the error and exit with the exit code of the command.
  Designed to be threaded with `process-run-sync`"
  [{:keys [cmd exit out err success?]}]
  (if (pos? exit)
    (do
      (log :error "Command failed: ")
      (log :error (str "\t" cmd))
      (println out)
      (println err)
      (System/exit exit))
    {:success? success?
     :exit exit
     :out out
     :err err}))

(defn print-output-and-continue
  "Prints out status code, stdout and stderr of a command, doesn't fail execution
  Designed to be threaded with `process-run-sync`"
  [{:keys [cmd out err exit] :as result}]
  (log :info (str "\t" cmd "\t[" exit "]\nstdout:\n" out "\nstderr:\n" err "\n"))
  result)

(defn terminate-with-exception!
  [message & [exit-code]]
  (throw (ex-info message {:babashka/exit (or exit-code 1)})))

;; Dealing with various data formats and encoding

(defn parse-boolean [thing]
  (java.lang.Boolean/parseBoolean (str thing)))

;; b64
(defn base64-decode [str-val]
  (String. ^"[B"
           (.decode (Base64/getDecoder) (.getBytes ^String str-val))))

(defn base64-encode [str-val]
  (.encodeToString (Base64/getEncoder) (.getBytes ^String str-val)))

(defn parse-env-file-content [envf]
  (->> envf
       (str/split-lines)
       (map-indexed (fn [idx line]
                      {:line (inc idx)
                       :content line}))
       (remove #(str/starts-with? (:content %) "#"))
       (remove #(str/blank? (:content %)))
       (map (fn [{:keys [line content]}]
              ;; split only on 1st occurence of '='
              (let [[k v] (str/split content #"=" 2)
                    val (when v
                          ;; strip " and " from value
                          (str/replace v #"^\"|\"$" ""))]
                (if val
                  (hash-map k val)
                  (logf :error "invalid line in env file: [%s] %s" line content)))))
       (remove nil?)
       (reduce merge)))

(defn read-yaml
  "Reads yaml file or content, map arg keys:
  - `path` or  `content` - where to read the YAML from
  - `all-docs?` - if true, return all documents in the file, rather than the first one"
  [{:keys [path content all-docs?]}]
  (yaml/parse-string (if path
                       (slurp path)
                       content)
                     :unknown-tag-fn :value
                     :unsafe false
                     :load-all (boolean all-docs?)))

(defn write-yaml
  "Serializes `:data` to YAML and writes it to a given file `:path`, if supplied in the arg map,"
  [{:keys [path data]}]
  (let [content (yaml/generate-string data :dumper-options {:flow-style :block})]
    (if path
      (spit path content)
      content)))

(defn read-json
  "Reads JSON content."
  [content]
  (json/parse-string content true))

(defn write-json
  "Serializes `:data` to JSON and writes it to a given file `:path` if supplied in the arg map,
  otherwise returns the JSON string."
  [{:keys [path data]}]
  (let [content (json/generate-string data)]
    (if path
      (spit path content)
      content)))

(defn render-template
  [{:keys [template-string data]}]
  (tmpl/render template-string data))

;; Simple HTTP client helper

(defn http-req
  [{:keys [method url body headers auth-token as-json? lax-body-parsing?]
    :or {as-json? true lax-body-parsing? false}}]
  {:pre [(not (str/blank? url))
         (#{:get :post :put :delete} method)]}
  (-> (http/request (cond-> {:method method
                             :uri url
                             :headers (merge headers (when as-json?
                                                       {"Accept" "application/json"
                                                        "Content-Type" "application/json"}))
                             :throw false}
                      body (assoc :body (if as-json?
                                          (json/generate-string body)
                                          body))
                      auth-token (assoc-in [:headers "Authorization"] (str "Bearer " (base64-encode auth-token)))))
      (update :body (fn [body]
                      (if as-json?
                        (try
                          (json/parse-string body true)
                          (catch Exception err
                            (if lax-body-parsing?
                              body
                              {:raw-body body
                               :error "Failed to decode json body"
                               :error-message (ex-message err)})))
                        body)))))
