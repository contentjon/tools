(ns com.contentjon.collections.vector
  (:refer-clojure :exclude [concat map])
  (:require [clojure.core :as clj]))

(def map
  "Like clojure's map, but always returns a vector"
  (comp vec clj/map))

(def concat
  "Like clojure's concat, but always returns a vector"
  (comp vec clj/concat))