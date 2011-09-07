(ns com.contentjon.logic.core
  (:refer-clojure :exclude (class?)))

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

(defn class?
  "Takes a parameter clazz and returns a predicate that checks if apllying
   class to an input value equals clazz"
  [clazz]
  (is? class clazz))

(defn interval?
  "Takes a upper and lower bound for an interval and returns
   a predicate that check that an input value is within those boundaries.
   The comparison is performed with the <= operator unless :type :open
   is passed in which case < is used"
  [min max & { :keys [type] :or { type :closed }}]
  (let [pred  (fn [op x] (op min x max))
        types { :open < :closed <= }]
    (partial pred (get types type))))
