(ns com.contentjon.gen.lang.text
  "Generic text generation utilities functions"
  (:require [clojure.string :as str])
  (:use [com.contentjon.fn.algo :only [applier]]))

(def ^:dynamic *endl*   "\n")
(def ^:dynamic *indent* "  ")

(def indent
  #(.replace % *endl* (str *endl* *indent*)))

(defn embracer
  ([open]       (embracer open open))
  ([open close] #(str open % close)))

(defn joiner [separator]
  (partial str/join separator))

(defn caller [f]
  (comp f list))

(def prepend #(str %2 %1))
(def append  str)

(def parens   (embracer "(" ")"))
(def braces   (embracer "{" "}"))
(def brackets (embracer "[" "]"))

(def words      (joiner " "))
(def lines      (joiner "\n"))
(def paragraphs (joiner "\n\n"))
(def commas     (joiner ","))
(def semicolons (joiner ";"))

(def words*      (caller words))
(def lines*      (caller lines))
(def paragraphs* (caller paragraphs))
(def commas*     (caller commas))
(def semicolons* (caller semicolons))
