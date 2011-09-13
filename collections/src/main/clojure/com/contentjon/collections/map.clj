(ns com.contentjon.collections.map)

(defn update-with
  "Takes a map and a second map of keys to functions. Returns a new map in which
   each value is updated whose key is present in the second map. The updated values
   are the result of applying the function stored at the respective key in the second map
   to the original value"
  [map updates]
  (reduce (fn [m [k update-fn]]
            (assoc m k (update-fn (k m))))
          map
          updates))

(defn update-when
  "Takes a map, a predicate and an update function.
   Returns a new map with each key of the original map associated
   with the result of applying the update function to the value at that key.
   This update only happens when the predicate returns a logically true
   value for the value at a specific key."
  [m pred update-fn]
  (reduce  (fn [m [k v]]
                (if (pred v)
                  (assoc m k (update-fn v))
                  m))
           m m))

(defn update-all
  "Update all values in a map m with update-fn"
  [m update-fn]
  (zipmap (keys m)
          (map update-fn (vals m))))

(defn select-when
  "Takes a map and returns a new map which contains only keys for which
   pred returns true"
  [m pred]
  (into {} (filter (fn [[k v]] (pred v)) m)))
