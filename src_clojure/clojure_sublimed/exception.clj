(ns clojure-sublimed.exception
  (:require
    [clojure.string :as str])
  (:import
    [clojure.lang Compiler Compiler+CompilerException ExceptionInfo LispReader+ReaderException]
    [System.IO TextWriter]
    [System.Diagnostics StackTrace StackFrame]))

(def ^:dynamic *print-quota*
  1024)

(defn debug> [x]
  (tap> x)
  x)

#_(defn- to-char-array ^chars [x]
    (cond
      (string? x)  (.toCharArray ^String x)
      (integer? x) (char-array [(char x)])
      :else        x))

;; modified from nrepl.middleware.print/with-quota-writer
(defn bounded-writer
  "java.io.Writer that wraps throws once it has written more than `quota` bytes"
  ^TextWriter [^TextWriter writer quota]
  writer
  ; TODO: reimplement 
  #_(let [total (volatile! 0)]
      (proxy [Writer] []
        (toString []
          (.toString writer))
        (write
          ([x]
           (let [cbuf (to-char-array x)]
             (.write ^Writer this cbuf (int 0) (count cbuf))))
          ([x off len]
           (locking total
             (let [cbuf (to-char-array x)
                   rem (- quota @total)]
               (vswap! total + len)
               (.write writer cbuf ^int off ^int (min len rem))
               (when (neg? (- rem len))
                 (throw (ex-info "Quota exceeded" {})))))))
        (flush []
          (.flush writer))
        (close []
          (.close writer)))))

(declare trace-str)

(defn bounded-pr-str [x]
  (let [writer (bounded-writer (System.IO.StringWriter.) *print-quota*)]
    (try
      (binding [*out* writer]
        ; TODO: lazy things may throw right inside `pr` and this code as it is now will replace exception trace with `...`
        ; I tried and it is pretty confusing 
        (pr x))
      (str writer)
      (catch Exception e
        ; TODO: remove possible(???) infinite loop
        (str writer (trace-str e))))))

;; CompilerException has location info, but its cause RuntimeException has the message ¯\_(ツ)_/¯
(defn root-cause [^Exception t]
  (loop [t t
         data nil]
    (if (and
          (nil? data)
          (or
            (instance? Compiler+CompilerException t)
            (instance? LispReader+ReaderException t))
          (not= [0 0] ((juxt :clojure.error/line :clojure.error/column) (ex-data t))))
      (recur t (ex-data t))
      (if-some [cause (some-> t .InnerException)]
        (recur cause data)
        (if data
          (ExceptionInfo. "Wrapper to pass CompilerException ex-data" data t)
          t)))))

(defn get-class-name [^StackFrame frame]
  (when-let [mi (.GetMethod frame)]
    (if-let [typ (or (.ReflectedType mi) (.DeclaringType mi))]
      (.Name typ)
      ; example: CallSite.Target stack frame
      "UNKNOWN")))

(defn duplicate? [^StackFrame prev-el ^StackFrame el]
  (and
    (= (get-class-name prev-el) (get-class-name el))
    (= (.GetFileName prev-el) (.GetFileName el))
    (= "invokeStatic" (.Name (.GetMethod prev-el)))
    (#{"invoke" "doInvoke"} (.Name (.GetMethod el)))))

(defn clear-duplicates [els]
  (for [[prev-el el] (map vector (cons nil els) els)
        :when (or (nil? prev-el) (not (duplicate? prev-el el)))]
    el))

(defn trace-element [^StackFrame el]
  (let [file     (.GetFileName el)
        clojure? (or (nil? file)
                   (= file "NO_SOURCE_FILE")
                   (.EndsWith file ".clj")
                   (.EndsWith file ".cljc"))
        classname (get-class-name el)]
    {:method (if (and clojure? (not (= classname "UNKNOWN")))
               (Compiler/demunge classname)
               (str classname "." (some-> (.GetMethod el) (.Name))))
     :file   (or (.GetFileName el) "NO_SOURCE_FILE")
     :line   (.GetFileLineNumber el)}))

(defn as-table [table]
  (let [[method file] (for [col [:method :file]]
                        (->> table
                          (map #(get % col))
                          (map str)
                          (map count)
                          (reduce max (count "null"))))
        format-str (str "\t%-" method "s\t%-" file "s\t:%d")]
    (->> table
      (map #(format format-str (:method %) (:file %) (:line %)))
      (str/join "\n"))))

(defn trace-str
  ([t]
   (trace-str t nil))
  ([^Exception t opts]
   (let [{:clojure.error/keys [source line column]} (ex-data t)
         cause t #_(or (.InnerException t) t)]
     (str
       (->> (StackTrace. cause true)
         (.GetFrames)
         (take-while #(not (#{"clojure.lang.Compiler" "clojure.lang.LispReader"}
                            (get-class-name ^StackFrame %))))
         (remove #(#{"clojure.lang.RestFn" "clojure.lang.AFn"} (get-class-name ^StackFrame %)))
         (clear-duplicates)
         (map trace-element)
         (reverse)
         (as-table))
       "\n>>> "
       (symbol (.Name (class cause)))
       ": "
       (.Message cause)
       (when (:location? opts true)
         (when (or source line column)
           (str " (" source ":" line ":" column ")")))
       (when-some [data (ex-data cause)]
         (str " " (bounded-pr-str data)))))))

(comment
  (require '[clojure-sublimed.exception :as exception] :reload-all)
  (try (/ 1 0) (catch Exception t (println (exception/root-cause t))))
  (try (/ 1 0) (catch Exception t (println (trace-str t))))
  (try (/ 1 0) (catch Exception t (Throwable->map t)))
  ,)
