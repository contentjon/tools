(ns com.contentjon.fn.algo)

(defn applier
  "Returns a function that applies f to
   an arbitrary number of arguments. Optionally
   any number of arguments can be bound beforehand"
  [f & bound]
  (apply partial apply f bound))

(defn mapper
  "Returns a function that maps f to its arguments."
  [f]
  (partial map f))

(defn reducer
  "Returns a function that reduces f on the given arguments.
   Optionally, the init arg can be bound."
  ([f]      (partial reduce f))
  ([f init] (partial reduce f init)))

(def composer (applier comp))
