(ns com.contentjon.gen.core
  "Contains general parser functions and the definition of the parser monad, which are used
   in more specialized parsers."
  (:refer-clojure :exclude [+ * not or])
  (:use [clojure.contrib.monads]))

(def #^{:doc "Defines the parser monad. Currently this is (state-t maybe-m).
              This means that a parser returns either nil if it fails or
              a vector containing the parser return value and the remaining
              input that was not matched"}
     parser-m (state-t maybe-m))

(def #^{:doc "Takes the result of a parser and checks if it failed"}
  parser-fail? nil?)

(defn result
  "Return the result of a parse process"
  [in]
  (first in))

(defn parser-finished?
  "Checks if a parser has successfully consumed all of it's input"
  [in]
  (and (clojure.core/not (parser-fail? in))
       (empty? (second in))))

(defn parse
  "Uses a parser to parse the input and returns the result of the parsing process"
  [rule in]
  (rule in))

(defprotocol AsParser
  "this section defines what can be turned into a parser function
    and how this is done"
  (as-parser [_]))

;;; helpers for defining or running parsers

(defn- skip-transform-bindings [skip-parser-sym bindings]
  (let [skip-binding `[_# (* ~skip-parser-sym)]
        bindings (->> bindings
                      (partition 2)
                      (interpose skip-binding)
                      (apply concat))]
    (concat skip-binding bindings skip-binding)))

(defn- bindings->parser [bindings]
  (->> bindings
       (partition 2)
       (map (fn [[s p]]
              (if (some #{s} [:when :let])
                [s p]
                (vector s `(as-parser ~p)))))
       (apply concat)))

(defmacro parser
  "Creates a new monadic parser with the passed in bindings and body.
   It optionally accepts :skip skip-parser as first arguments, in which
   case skip-parser is run before and after each subparsers. This is useful
   for dealing with whitespace."
  ([bindings body]
     `(domonad parser-m
               [~@(bindings->parser bindings)]
               ~body))
  ([skip-keyword skip bindings body]
     (when-not (= skip-keyword :skip)
       (throw (IllegalArgumentException. "use :skip")))
     (let [skip-parser-sym (gensym "skip-parser")
           bindings (skip-transform-bindings skip-parser-sym bindings)]
       `(let [~skip-parser-sym ~skip]
          (parser ~bindings
                  ~body)))))

(defmacro defparser
  "Defines a new monadic parser with the passed in name, bindings and body.
   Can deal with :skip skip-parser like the parser macro."
  ([name bindings body]
     `(def ~name (parser ~bindings
                         ~body)))
  ([name skip-keyword skip bindings body]
     `(def ~name (parser ~skip-keyword
                         -skip
                         ~bindings
                         ~body))))

;;; public matching functions

(defn lambda
  "The empty word. Returns a parser that matches nothing and returns the entire input state."
  []
  (fn [in] [nil in]))

(defn any
  "Returns a parser that matches the first input symbol with no further inspection
   and returns the input without it's first symbol as the new state"
  []
  (fn [in]
    (when-not (empty? in)
      [(first in) (rest in)])))

(defn of
  "Takes a predicate and returns a parser that matches the first symbol of the input if the
   predicate evaluates to a value that is not logically false"
  [predicate]
  (fn [in]
    (when (and (clojure.core/not (empty? in))
               (predicate (first in)))
      [(first in) (rest in)])))

(defn not
  "A parser that fails when p succeeds and succeeds when p fails"
  [p]
  (fn [in]
    (if (p in)
      nil
      [nil in])))

(def #^{:doc "Returns a parser that matches one of multiple alternatives"
	:arglists '([& rules]) }
     or (with-monad parser-m m-plus))

(defn ?
  "The parser may apply another parser or not"
  [rule]
  (or rule (lambda)))

(declare +)

(defn *
  "Returns a parser that applies a subparser 0 or more times to it's input"
  [rule]
  (? (+ rule)))

(defn +
  "Returns a parser that applies a subparser 1 or more times to it's input"
  [rule]
  (parser [first rule
           rest  (* rule)]

    (concat [first] rest)))

(defn times
  "A parser that applies p between min and max times.
   If only min is given, the parser is applied exactly min times"
  ([p min]
     (times p min min))
  ([p min max]
     { :pre [(<= min max)]}
     (letfn [(next-parser [n]
               (if (= n max)
                 (lambda)
                 (let [next (parser [f p
                                     r (next-parser (inc n))]
                              (cons f r))]
                   (if (< n min)
                     next
                     (? next)))))]
       (next-parser 0))))

;;; helper functions that can be used to verify the state of a parser but don't consume
;;; any input

(defn assert-state
  "Returns a parser that matches nothing. It returns a nil value and its input as its state.
   Before it does this it applies a predicate to its input state and lets the parser fail
   if it doesn't match"
  [predicate]
  (parser [state (fetch-state)
	   _     #(if (predicate state) [% state] nil)]
	  nil))

;;; some useful parsers

(defn surround
  "A parser that first runs before, then p, then after and returns the result of p when all succeed"
  ([p around]
     (surround p around around))
  ([p before after]
     (parser [_ before
              res p
              _ after]
             res)))

(defn surrounder
  "Returns a function that transforms a parser with the surround function"
  ([around]
     (surrounder around around))
  ([before after]
     #(surround % before after)))

(defn log
  "Wraps a parser and logs its input"
  [msg rule]
  (fn [in]
    (println msg in)
    (rule in)))

;;; parser coercsions

(defn- without-match [match in]
  (.substring in (.length match) (.length in)))

(defn descend [rule]
  (parser [stream (fetch-state) :when (clojure.core/not (empty? stream))
           :let  [f (first stream)]
           _      (set-state (if (string? f)
                               f
                               (seq (first stream))))
           result rule
           _      (assert-state empty?)
           _      (set-state (rest stream))]
          result))

(defn maybe-descend [rule]
  (fn [in]
    (if (sequential? in)
      (parse (descend rule) in)
      (parse rule in))))

(extend-protocol AsParser
  java.lang.Character
  (as-parser [this]
    (fn [in]
      (let [length (.length in)]
        (when (and (> length 0)
                   (= (.charAt in 0) this))
          [this (.substring in 1 length)]))))
  java.lang.String
  (as-parser [this]
    (maybe-descend
     (fn [in]
        (when (.startsWith in this)
          [this (without-match this in)]))))
  java.util.regex.Pattern
  (as-parser [this]
    (maybe-descend
     (fn [in]
       (let [pat (java.util.regex.Pattern/compile (str "^" this ""))
             m   (.matcher pat in)]
         (when (.find m)
           (let [match (.group m)]
             [match (without-match match in)]))))))
  clojure.lang.IPersistentVector
  (as-parser [this]
    (let [rule (* (apply or (map as-parser this)))]
      (descend rule)))
  clojure.lang.IPersistentMap
  (as-parser [this]
    (descend
     (parser [pairs (+ (descend
                        (parser [k (of this)
                                 v (get this k)]
                                [k v])))]
             (into {} pairs))))
  clojure.lang.Fn
  (as-parser [this] this)
  java.lang.Object
  (as-parser [this]
    (throw (java.lang.IllegalArgumentException. (str "Not a supported parser type: " this))))
  nil
  (as-parser [_]
    (throw (java.lang.IllegalArgumentException. "Not a supported parser type: nil"))))
