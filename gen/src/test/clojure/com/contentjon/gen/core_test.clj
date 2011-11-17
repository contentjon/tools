(ns com.contentjon.gen.core-test
  (:refer-clojure :exclude [+ * not or])
  (:use [com.contentjon.gen.core]
        [clojure.test :only (deftest)]
        [midje.sweet]))

(deftest lambda-test
  (facts
    (parse (lambda) [1]) => falsey
    (parse (lambda) nil) => falsey
   
    (parse-partial (lambda) [])    => [nil []]
    (parse-partial (lambda) [1])   => [nil [1]]
    (parse-partial (lambda) "123") => [nil "123"]))

(deftest any-test
  (facts
    (parse (any) [])  => falsey
    (parse (any) [1]) => 1
    
    (parse-partial (any) [1 2 3]) => [1 [2 3]]
    (parse-partial (any) "abc")   => [\a [\b \c]]))

(deftest of-test
  (facts
    (parse (of integer?) [1])      => 1
    (parse (of map?)     [{:a 1}]) => {:a 1}
    (parse (of string?)  [1])      => nil
    
    (parse-partial (of string?) ["1" "ab"]) => ["1" ["ab"]]

    ; parser generation time failures
    (parse (of "xyz") [ ...xs...]) => (throws ClassCastException)
    (parse (of nil) [ ...xs...])   => (throws NullPointerException)))

(deftest not-test
  (facts
    (parse-partial (not "xyz") "abc") => [nil "abc"]
    (parse-partial (not "xyz") "xyz") => parser-fail?

    ; parser generation time failures
    (parse (not nil) [...xs...]) => (throws IllegalArgumentException)))

(deftest or-test
  (facts
    (parse (or (of integer?) "abc") "abc") => "abc"
    (parse (or (of integer?) "abc") [1])   => 1
    (parse (or (of integer?) "abc") [1.0]) => parser-fail?

    (parse-partial (* (or "xyz" (of integer?))) []) => [nil []]
    (parse (* (or "xyz" (of integer?))) [1]) => [1]
    (parse (* (or "xyz" (of integer?))) [1 "xyz"]) => [1 "xyz"]
    (parse (* (or "xyz" (of integer?))) [1 "xyz" "xyz" 1 1]) => [1 "xyz" "xyz" 1 1]
    
    (parse (or) [1]) => parser-fail?

    ; parser generation time failures
    (or nil) => (throws RuntimeException))) ; wrapped IllegalArgumentException

(deftest ?-test
  (facts

    (parse (? (of integer?)) [1]) => 1
    (parse (? "foo") ["foo"])     => "foo"
    (parse (? "foo") "foo")       => "foo"

    (parse-partial (? (any)) [])              => [nil []]
    (parse-partial (? (of integer?)) ["abc"]) => [nil ["abc"]]

    ; parser generation time failures
    (? nil) => (throws RuntimeException))) ; wrapped IllegalArgumentException

(deftest *-test
  (facts
    (parse (* "bar") ["bar"])       => ["bar"]
    (parse (* "bar") ["bar" "bar"]) => ["bar" "bar"]
    (parse (* "bar") "bar")         => ["bar"]
    (parse (* "bar") "barbar")      => ["bar" "bar"]
    
    (parse-partial (* "bar") "")     => [nil ""]
    (parse-partial (? (* "bar")) "") => [nil ""]
    
    ; * should be able to handle this case without
    ; blowing the stack
    (parse-partial (* (? "bar")) "")         => [nil ""]
    (parse-partial (+ (? (of integer?))) []) => [nil []]
    
    (parse (* nil) ...xs...) => (throws RuntimeException))) ; wrapped IllegalArgumentException

(deftest +-test
  (facts
    (parse (+ "bar") ["bar"])       => ["bar"]
    (parse (+ "bar") ["bar" "bar"]) => ["bar" "bar"]
    (parse (+ "bar") "bar")         => ["bar"]
    (parse (+ "bar") "barbar")      => ["bar" "bar"]
    
    (parse (+ "bar") "")             => parser-fail?
    (parse-partial (? (+ "bar")) "") => [nil ""]
    
    ; + should be able to handle this case without
    ; blowing the stack
    (parse-partial (+ (? "bar")) "")         => [nil ""]
    (parse-partial (+ (? (of integer?))) []) => [nil []]
    
    ; parser generation time failures
    (parse (+ nil) ...xs...) => (throws IllegalArgumentException)))

(deftest test-times
  (facts

    ; single argument
    (parse (times (of integer?) 1) [11])     => [11]
    (parse (times (of integer?) 3) [5 4 11]) => [5 4 11]
    (parse (times (of integer?) 1) [])       => parser-fail?
    (parse (times (of integer?) 1) [8 7])    => parser-fail?
    

    ; with boundaries
    (parse (times (of integer?) 3 3) [1 2 3])       => [1 2 3]
    (parse (times (of integer?) 1 3) [1 2 3])       => [1 2 3]
    (parse (times (of integer?) 1 3) [24])          => [24]
    (parse (times (of integer?) 2 5) [1 2 3 4 8 3]) => parser-fail?
    (parse (times (of integer?) 1 3) [])            => parser-fail?
    (parse (times (of integer?) 1 3) nil)           => parser-fail?
    (parse (times (of integer?) 2 3) [8])           => parser-fail?
    (parse (times (of integer?) 2 3) [8 8 "xy"])    => parser-fail?
    (parse (times (of integer?) 3 3) [8 "xy" 4])    => parser-fail?
    (parse (times (of integer?) 1 1) ["xy"])        => parser-fail?
    (parse (times (of integer?) 0 1) ["xy"])        => parser-fail?

    (parse-partial (times (of integer?) 0 3) []) => [nil []]
    (parse-partial (times (of integer?) 0) [])   => [nil []]

    (parse-partial (times (* "baz") 0 1) "")    => [nil ""]
    (parse-partial (times (* "baz") 0 1) "baz") => [[["baz"]] ""]
    (parse-partial (times (* "baz") 0 1) "bazbaz") => [[["baz" "baz"]] ""]

    (parse-partial (times (* "baz") 2) "bazbaz") => [[["baz" "baz"]] ""]
    (parse (times (+ "baz") 2) "bazbaz")         => parser-fail?

    (parse (times (? (of integer?)) 3) [1 2 3]) => [1 2 3]
    (parse (times (? (of integer?)) 3) [1 2])   => [1 2]
    
    ; parse time exceptions
    (parse (times nil 1 4) []) => (throws IllegalArgumentException)

    ; parser generation exceptions
    (times (of integer?) 3 2)   => (throws AssertionError)
    (times nil 4 1)             => (throws AssertionError)
    (times (of integer?) "x" 2) => (throws ClassCastException)
    (times (of integer?) 1 "x") => (throws ClassCastException)
    (times nil "x" "y")         => (throws ClassCastException)    
    (times (of integer?) nil 6) => (throws NullPointerException)
    (times (of integer?) 3 nil) => (throws NullPointerException)))
