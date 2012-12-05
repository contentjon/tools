(ns com.contentjon.debug.log
  "Tools for good-old printf-style debugging"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:dynamic *debug-out* *out*)

(defn expand-home [filename]
  (if (.startsWith filename "~")
    (str (System/getProperty "user.home")
         (subs filename 1))
    filename))

(defn- to-writer [x]
  (if (string? x)
    (-> x
        (expand-home)
        (io/writer))
    x))

(defn debug-to
  "Sets *debug-out* globally. Valid arguments are:
   - nil for suppressing all debug prints
   - a filename (a leading ~ is replaced by user.home)
   - any java.io.Writer, e.g. *out*"
  [x]
  (->> (to-writer x)
       (constantly)
       (alter-var-root #'*debug-out*)))

(defn dont-debug
  "Suppresses debug prints globally by setting *debug-out* to nil."
  []
  (debug-to nil))

(defmacro with-debug-to
  "Dynamically binds *debug-out* and executes body.
   See debug-to for more information."
  [x & body]
  `(binding [*debug-out* (to-writer ~x)]
     ~@body))

(defmacro without-debug
  "Binds *debug-out* to nil and executes body."
  [& body]
  `(with-debug-to nil
     ~@body))

(defn log
  "Logs args to *debug-out*, formatted using println-str"
  [& args]
  (when *debug-out*
    (->> args
         (apply println-str)
         (.write *debug-out*))
    (.flush *debug-out*)))

(defmacro log-call
  "Logs a function call, printing both the arguments and the result,
   then returns the result. Example: (log-call + 1 2 3)"
  [f & args]
  `(let [args# [~@args]]
     (log "Calling" ~(str f) "with args:" (str/join " " args#))
     (let [ret# (apply ~f args#)]
       (log ~(str f) "returned" ret#)
       ret#)))