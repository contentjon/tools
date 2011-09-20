(ns com.contentjon.gen.core-test
  (:use [com.contentjon.gen.core]
        [clojure.test :only (deftest)]
        [midje.sweet]))

(def parse-success (comp parser-finished? parse))

(deftest test-times
  (facts

    ; single argument
    (parse-success (times (of integer?) 1) [11])     => true
    (parse-success (times (of integer?) 3) [5 4 11]) => true
    (parse-success (times (of integer?) 1) [])       => false
    (parse-success (times (of integer?) 1) [8 7])    => false

    ; with boundaries
    (parse-success (times (of integer?) 3 3) [1 2 3])       => true
    (parse-success (times (of integer?) 1 3) [1 2 3])       => true
    (parse-success (times (of integer?) 1 3) [24])          => true
    (parse-success (times (of integer?) 0 3) [])            => true
    (parse-success (times (of integer?) 2 5) [1 2 3 4 8 3]) => false
    (parse-success (times (of integer?) 1 3) [])            => false
    (parse-success (times (of integer?) 1 3) nil)           => false
    (parse-success (times (of integer?) 2 3) [8])           => false
    (parse-success (times (of integer?) 2 3) [8 8 "xy"])    => false
    (parse-success (times (of integer?) 3 3) [8 "xy" 4])    => false
    (parse-success (times (of integer?) 1 1) ["xy"])        => false
    (parse-success (times (of integer?) 0 1) ["xy"])        => false

    ; parse time exceptions
    (parse-success (times "x" 1 8) []) => (throws IllegalArgumentException)
    (parse-success (times nil 1 4) []) => (throws IllegalArgumentException)

    ; parser generation exceptions
    (times (of integer?) 3 2)   => (throws AssertionError)
    (times nil 4 1)             => (throws AssertionError)
    (times (of integer?) "x" 2) => (throws ClassCastException)
    (times (of integer?) 1 "x") => (throws ClassCastException)
    (times nil "x" "y")         => (throws ClassCastException)    
    (times (of integer?) nil 6) => (throws NullPointerException)
    (times (of integer?) 3 nil) => (throws NullPointerException)))
