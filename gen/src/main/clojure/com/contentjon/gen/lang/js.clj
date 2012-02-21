(ns com.contentjon.gen.lang.js
  (:require [com.contentjon.gen.lang.curly :as crly]
            [com.contentjon.gen.lang.text  :as txt]))

(defn each [x xs cmds]
  (txt/words* "for"
              (txt/parens (txt/words* x "in" xs))
              (crly/block cmds)))

(defn var-def
  ([type init]
     (txt/words* type "=" init)))

(defn lambda [args body]
  (txt/words*
    (crly/call "function" args)
    (crly/block body)))
