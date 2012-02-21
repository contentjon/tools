(ns com.contentjon.gen.lang.java
  (:refer-clojure :exclude (cast class for))
  (:require [com.contentjon.gen.lang.curly :as crly]
            [com.contentjon.gen.lang.text  :as txt]))

(defn ctor [name args cmds]
  (txt/words* (crly/call name args)
              (crly/block cmds)))

(defn method [type name args cmds]
  (txt/words* type
              (crly/call name args)
              (crly/block cmds)))

(defn class [name cmds]
  (txt/words* "class"
              name
              (crly/block cmds)))
