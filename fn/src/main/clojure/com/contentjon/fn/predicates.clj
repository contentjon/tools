(ns com.contentjon.fn.predicates
  (:refer-clojure :exclude [class? every? contains?]))

(defn and?
  "Takes n predicates and returns a predicate that returns
   true when all those predicates return true for a given input value"
  [& preds]
  (if (empty? preds)
    (fn [x] true)
    (fn [x]
      (loop [[pred & rest] preds]
        (let [result (pred x)]
          (if (and result rest)
            (recur rest)
            result))))))

(defn or?
  "Take n predicates and returns a predicate that returns true when
   any of those predicates return true for a given input value"
  [& preds]
  (fn [x] (some #(% x) preds)))

(defn eq?
  "Takes a value and returns a predicate that returns true when
   it's input is equal to value"
  [val]
  #(= % val))

(defn is?
  "Takes a function f and a value val and returns returns a predicate
   that returns true when the result of applying f to an input is equal
   to val"
  [f val]
  (comp (eq? val) f))

(defn in?
  "Returns a predicate that checks if an input value is in a set of values"
  [& vals]
  (set vals))

(defn class?
  "Takes a parameter clazz and returns a predicate that checks if apllying
   class to an input value equals clazz"
  [clazz]
  (is? class clazz))

(defn interval?
  "Takes a upper and lower bound for an interval and returns
   a predicate that checks that an input value is within those boundaries.
   The comparison is performed with the <= operator unless :type :open
   is passed in which case < is used"
  [min max & { :keys [type] :or { type :closed }}]
  (let [pred  (fn [op x] (op min x max))
        types { :open < :closed <= }]
    (and? number?
          (partial pred (get types type)))))

(def not? complement)

(defn every?
  "Take a predicate and returns a predicate that returns true
   when an input value returns true for elements of a sequence"
  [pred]
  (partial clojure.core/every? pred))

(defn contains?
  "Take a key and returns a predicates that checks whether the
   key exists in its input parameter"
  [key]
  #(clojure.core/contains? % key))

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
  (and? sequential?
        (every? (apply or? preds))))

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

(defn identity-when
  "Takes a predicate and an element x. Returns the identity of x
   when (pred x) evaluates to logically true"
  [pred]
  (and? pred identity))
