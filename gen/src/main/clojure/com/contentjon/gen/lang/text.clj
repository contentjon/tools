(ns com.contentjon.gen.lang.text
  "Generic text generation utilities functions"
  (:require [clojure.string :as str])
  (:use [com.contentjon.fn.algo :only [applier]]))

(def ^:dynamic *endl*   "\n")
(def ^:dynamic *indent* "  ")

(def render  (partial apply str))

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

(defn prepender [s]
  #(prepend % s))

(defn appender [s]
  #(append % s))

(def endl    (appender "\n"))
(def ownline (embracer "\n"))

(defn nendl [txt n]
  (first (drop n (iterate endl txt))))

(def parens   (embracer "(" ")"))
(def braces   (embracer "{" "}"))
(def brackets (embracer "[" "]"))
(def angles   (embracer "<" ">"))
(def quotes   (embracer "\""))

(def words       (joiner " "))
(def lines       (joiner "\n"))
(def paragraphs  (joiner "\n\n"))
(def points      (joiner "."))
(def commas      (joiner ","))
(def colons      (joiner ":"))
(def semicolons  (joiner ";"))
(def underscores (joiner "_"))
(def slashes     (joiner "/"))
(def ands        (joiner "&"))

(def words*      (caller words))
(def lines*      (caller lines))
(def paragraphs* (caller paragraphs))
(def points*     (caller points))
(def commas*     (caller commas))
(def semicolons* (caller semicolons))
(def slashes*    (caller slashes))

(defn block [cmds]
  (-> cmds
      lines*
      (prepend *endl*)
      indent
      (append *endl*)))
