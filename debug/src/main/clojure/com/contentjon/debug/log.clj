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

(defn- expanded [x]
  (if (string? x)
    (expand-home x)
    x))

(defn debug-to
  "Sets *debug-out* globally. Valid arguments are:
   - nil for suppressing all debug prints
   - a filename (a leading ~ is replaced by user.home)
   - any java.io.Writer, e.g. *out*"
  [x]
  (->> (expanded x)
       (constantly)
       (alter-var-root #'*debug-out*)))

(defn dont-debug
  "Suppresses debug prints globally by setting *debug-out* to nil."
  []
  (debug-to nil))

(defn debugging-to [f x]
  (fn [& args]
    (binding [*debug-out* (expanded x)]
      (apply f args))))

(defmacro with-debug-to
  "Dynamically binds *debug-out* and executes body.
   See debug-to for more information."
  [x & body]
  `(let [f# (debugging-to (fn []
                            ~@body)
                          ~x)]
     (f#)))

(defn log
  "Logs args to *debug-out*, formatted using println-str"
  [& args]
  (let [msg (binding [*print-length* 20
                      *print-level* 4]
              (apply println-str args))]
    (when *debug-out*
      (if (string? *debug-out*)
        (spit *debug-out* msg :append true)
        (doto *debug-out*
          (.write msg)
          (.flush)))
      nil)))

(defn logging
  "Returns a function that behaves like f, but logs the arguments
   and the return value. The second argument is a function name
   to be printed along."
  [f fname]
  (fn [& args]
    (log "Calling" fname "with args:" args)
    (let [ret (apply f args)]
       (log fname "returned" ret)
       ret)))

(defmacro with-logging
  "Logs a function call, printing both the arguments and the result,
   then returns the result. Example: (with-logging + 1 2 3)"
  [f & args]
  `(let [fname# '~f
         f#     (logging ~f fname#)]
     (f# ~@args)))