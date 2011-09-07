(ns com.contentjon.gen.core
  "Contains general parser functions and the definition of the parser monad, which are used
   in more specialized parsers."
  (:use [clojure.contrib.monads]))

(def #^{:doc "Defines the parser monad. Currently this is (state-t maybe-m).
              This means that a parser returns either nil if it fails or
              a vector containing the parser return value and the remaining
              input that was not matched"}
     parser-m (state-t maybe-m))

(def #^{:doc "Takes the result of a parser and checks if it failed"}
  parser-fail? nil?)

(defn parser-finished?
  "Checks if a parser has successfully consumed all of it's input"
  [in]
  (and (not (parser-fail? in))
       (empty? (second in))))

;;; helpers for defining or running parsers

(defmacro parser
  "Creates a new mondaic parser with the passed in bindings and body"
  [bindings body]
  `(domonad parser-m
            ~@(if bindings [bindings])
            ~body))

(defmacro defparser
  "Defines a new monadic parser with the passed in name, bindings and body"
  ([name bindings body]
     `(def ~name (parser ~bindings
                          ~body)))
  ([name skip-keyword skip bindings body]
     (when-not (= skip-keyword :skip)
       (throw (IllegalArgumentException. "use :skip")))
     (let [skip-parser-sym (gensym "skip-parser")
           skip-binding `[_# (many* ~skip-parser-sym)]
           bindings (->> bindings
                         (partition 2)
                         (interpose skip-binding)
                         (apply concat))
           bindings (concat skip-binding bindings skip-binding)]
       `(let [~skip-parser-sym ~skip]
          (defparser ~name ~bindings
            ~body)))))

(defn parse
  "Uses a parser to parse the input and returns the result of the parsing process"
  [rule in]
  (first (rule in)))

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
    (if-not (empty? in)
      [(first in) (rest in)])))

(defn of
  "Takes a predicate and returns a parser that matches the first symbol of the input if the
   predicate evaluates to a value that is not logically false"
  [predicate]
  (fn [in]
    (if (and (not (empty? in))
	     (predicate (first in)))
      [(first in) (rest in)])))

(defn literal
  "Takes a literal expression and returns a parser that matches the first input symbol if
   if comparing it for equality with the literal returns true"
  [lit]
  (of #(= % lit)))

(defn literals
  "Takes a collection of literals and matches them in sequence"
  [col]
  (if (empty? col)
    (lambda)
    (parser [first (literal (first col))
	     rest  (literals (rest col))]
	    (concat [first] rest))))

(def #^{:doc "Returns a parser that matches one of multiple alternatives"
	:arglists '([& rules]) }
     one (with-monad parser-m m-plus))

(defn maybe
  "The parser may apply another parser or not"
  [rule]
  (one rule (lambda)))

(declare many+)

(defn many*
  "Returns a parser that applies a subparser 0 or more times to it's input"
  [rule]
  (maybe (many+ rule)))

(defn many+
  "Returns a parser that applies a subparser 1 or more times to it's input"
  [rule]
  (parser [first rule
           rest  (many* rule)]
          (concat [first] rest)))

;;; a few additional composite rules

(defn one*
  "Match zero or more of a choice of several subrules"
  [& rules]
  (many* (apply one rules)))

(defn one+
  "Match one or more of a choice of several subrules"
  [& rules]
  (many+ (apply one rules)))

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
