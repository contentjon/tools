(ns com.contentjon.gen.lang.curly
  (:refer-clojure :exclude (and cast for or ==))
  (:require [com.contentjon.gen.lang.text :as txt])
  (:use [com.contentjon.fn.algo :only [applier]]))

(def block
  (comp txt/braces txt/block))

(defn statement [expr]
  (txt/append expr ";"))

(defn return [expr]
  (txt/words* "return" expr))

(defn if [pred cmds]
  (txt/words*
    "if"
    (txt/parens pred)
    (block cmds)))

(defn else [cmds]
  (txt/words* "else" 
              (block cmds)))

(defn else-if [cmds]
  (txt/words* "else if"
              (block cmds)))

(defn for [init condition step cmds]
  (txt/words* "for"
              (txt/parens (txt/semicolons* init condition step))
              (block cmds)))

(defn call* [name & args]
  (str name (txt/parens (txt/commas args))))

(def call (applier call*))

(defn var-def
  ([type name]
     (txt/words* type name))
  ([type name init]
     (txt/words* type name "=" init)))

(defn op [o & args]
  (txt/parens
    (txt/words (interpose o args))))

(def or*   (partial op "||"))
(def and*  (partial op "&&"))
(def eq*   (partial op "=="))
(def neq*  (partial op "!="))

(def or  (applier or*))
(def and (applier and*))
(def eq  (applier eq*))

(def && (partial op "&&"))
(def == (partial op "=="))
(def != (partial op "!="))

(defn bool-op [o args]
  (if (= (count args)
         2)
    (op o args)          
    (let [arg-pairs (partition 2 1 args)]
      (op "&&"
          (map #(op o %)
               arg-pairs)))))

(defn ? [expr a b]
  (txt/words* expr "?" a ":" b))

(defn postfix-op [op x]
  (txt/parens (str x op)))

(defn cast [type expr]
  (txt/words (txt/parens type)
             expr))
