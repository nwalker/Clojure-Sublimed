(ns clojure-sublimed.socket-repl
  (:require
    [clojure.string :as str]
    [clojure-sublimed.exception :as exception])
  (:import
    [System.IO StringReader]))

(def ^:dynamic *out-fn*
  prn)

(def ^:dynamic *context*
  nil)

(defonce *evals
  (atom {}))

(defn stop! []
  (throw (ex-info "Stop" {::stop true})))

(defn debug> [x]
  (tap> x)
  x)

(defn read-command [in]
  (let [[form s] (read+string {:eof ::eof, :read-cond :allow} in)]
    (when (= ::eof form)
      (stop!))
    
    (vswap! *context* assoc :form s)
    
    (when-not (map? form)
      (throw (Exception. "Unexpected form")))
    
    form))

(defn report-throwable [^Exception t]
  (let [root  ^Exception (exception/root-cause t)
        {:clojure.error/keys [source line column]} (ex-data root)
        cause ^Exception (or (some-> root .InnerException) root)
        data  (ex-data cause)
        class (.Name (class cause))
        msg   (.Message cause)
        val   (cond-> (str class ": " msg)
                data
                (str " " (exception/bounded-pr-str data)))
        trace (exception/trace-str root {:location? false})]
    (*out-fn*
      (merge
        {:tag    :ex
         :val    val
         :trace  trace
         :source source
         :line   line
         :column column}
        @*context*))))

(defn reader [code line column]
  (let [reader (clojure.lang.LineNumberingTextReader. (StringReader. code))]
    (when line
      (.set_LineNumber reader (int line)))
    ; TODO: reimplement
    #_(when column
      (when-some [field (->> clojure.lang.LineNumberingTextReader
                          (.getDeclaredFields)
                          (filter #(= "_columnNumber" (.getName ^java.lang.reflect.Field %)))
                          first)]
        (doto ^java.lang.reflect.Field field
          (.setAccessible true)
          (.set reader (int column)))))
    reader))

(defn eval-code [form]
  (let [{:keys [id op code ns line column file]} form
        ; code' (binding [*read-eval* false]
        ;         (read-string {:read-cond :preserve} code))
        start  (clojure.lang.RT/StartStopwatch)
        ns     (or ns 'user)
        ns-obj (or
                 (find-ns ns)
                 (do
                   (require ns)
                   (find-ns ns)))
        name   (some-> file (str/split #"[/\\]") last)
        ; ret   (eval `(do (in-ns '~(or ns 'user)) ~code'))
        ret    (binding [*read-eval* false
                         *ns*        ns-obj]
                 (clojure.lang.Compiler/load (reader code line column) file name nil))
        time   (clojure.lang.RT/StopStopwatch)]
    (*out-fn*
      {:tag  :ret
       :id   id
       :val  (exception/bounded-pr-str ret)
       :time time})))

(defn fork-eval [{:keys [id] :as form}]
  (let [f (future
            (try
              (eval-code form)
              (catch Exception t
                (try
                  (report-throwable t)
                  (catch Exception t
                    :ignore))))
            (swap! *evals dissoc id))]
    (swap! *evals assoc id f)))

(defn interrupt [{:keys [id]}]
  (when-some [f (@*evals id)]
    (future-cancel f)))

(def safe-meta?
  #{:ns :name :doc :file :arglists :forms :macro :special-form :protocol :line :column :added :deprecated :resource})

(defn lookup-symbol [form]
  (let [{:keys [id op symbol ns] :or {ns 'user}} form
        ns     (clojure.core/symbol ns)
        symbol (clojure.core/symbol symbol)
        meta   (if (special-symbol? symbol)
                 (assoc ((requiring-resolve 'clojure.repl/special-doc) symbol)
                   :ns 'clojure.core
                   :file "clojure/core.clj"
                   :special-form true)
                 (meta (ns-resolve ns symbol)))]
    (*out-fn*
      (if meta
        (let [meta' (reduce-kv
                      (fn [m k v]
                        (if (safe-meta? k)
                          (assoc m (name k) (str v)) ; stringify to match nREPL
                          m))
                      nil
                      meta)]
          {:tag :lookup
           :id  id
           :val meta'})
        {:tag :ex
         :id  id
         :val (str "Symbol '" symbol " not found in ns '" ns)}))))

(defn out-fn [out]
  (let [lock (Object.)]
    #(locking lock
       (binding [*out* out]
         (prn %)))))

(defn repl []
  (binding [*out-fn* (out-fn *out*)
            *out*    (.getRawRoot #'*out*)
            *err*    (.getRawRoot #'*err*)]
    (*out-fn* {:tag :started})
    (loop []
      (when
        (binding [*context* (volatile! {})]
          (try
            (let [form (read-command *in*)]
              (when-some [id (:id form)]
                (vswap! *context* assoc :id id))
              (case (:op form)
                :close     (stop!)
                :eval      (fork-eval form)
                :interrupt (interrupt form)
                :lookup    (lookup-symbol form)
                (throw (Exception. (str "Unknown op: " (:op form)))))
              true)
            (catch Exception t
              (when-not (-> t ex-data ::stop)
                (report-throwable t)
                true))))
        (recur)))
    (doseq [[id f] @*evals]
      (future-cancel f))))

