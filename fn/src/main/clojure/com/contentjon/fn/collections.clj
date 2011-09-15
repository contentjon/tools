(ns com.contentjon.logic.collections
  "Clojure collection related predicate generators"
  (:refer-clojure :exclude (every? map-of?))
  (:use [com.contentjon.logic.core :only (and? is? or?)]))

(defn every?
  "Take a predicate and returns a predicate that returns true
   when an input value returns true for elements of a sequence"
  [pred]
  (partial clojure.core/every? pred))

(defn count?
  "Takes an integer and returns a predicate
   that returns true when that integer equals the result of
   applying count to its input"
  [n]
  (is? count n))

(defn sequence-of?
  "Takes a number of predicates. Returns a predicate that checks that
   at least one of those predicates applies to every element of an
   input sequence"
  [& preds]
  (every? (apply or? preds)))

(defn property?
  "Takes a key and a predicate. Returns a function that first reads
   the value at key from a map and applies the predicate to that value"
  [key pred]
  (comp pred key))

(defn map-of?
  "Takes an arbitrary number of parameter specs where each
   spec is of the form :key pred.
   Returns a predicate takes a map and checks the following:
   - Only keys from the input spec are present in the input map
   - All keys from the input spec are present in the input map
   - For each key in the input map the corresponding predicate from
     the parameter spec applies"
  [& props]
  (let [spec (partition 2 props)]
    (apply and? (map (partial apply property?) spec))))
