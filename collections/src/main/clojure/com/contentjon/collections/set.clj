(ns com.contentjon.collections.set
  (:refer-clojure :exclude [conj disj map])
  (:require [clojure.core :as clj]))

(defn as-set
  "Turns the given collection into a set, if necessary"
  [coll]
  (if (set? coll)
    coll
    (set coll)))

(def conj
  "Like clojure's conj, but always returns a set"
  (comp as-set clj/conj))

(def disj
  "Like clojure's disj, but always returns a set"
  (comp clj/disj as-set))

(def map
  "Like clojure's map, but always returns a set"
  (comp set clj/map))