(ns com.contentjon.fn.algo-test
  (:use [clojure.test :only [deftest]]
        midje.sweet
        [com.contentjon.fn.algo :reload true]))

(deftest test-applier
  (facts ((applier +) [1 2])           => 3
         ((applier +) 1 [1 2])         => 4
         ((applier + 3) 4 [])          => 7
         ((applier + 3) 4 [1 2])       => 10
         ((applier + 3 4) 5 [1 2])     => 15
         ((applier + 3 4 [1 2]))       => 10
         ((applier +) 4)               => (throws IllegalArgumentException)
         ((applier 1 2 3) [])          => (throws ClassCastException)
         ((applier 1) 4)               => (throws ClassCastException)
         ((applier nil ...many...) []) => (throws NullPointerException)))

(deftest test-mapper
  (facts ((mapper inc) [])            => []
         ((mapper inc) [1 2 3])       => [2 3 4]
         ((mapper +) [1 2 3] [3 2 1]) => [4 4 4]
         ((mapper nil) [])            => []
         ((mapper 1) [...xs...])      => (throws ClassCastException)))

(deftest test-reducer
  (facts ((reducer +) [1 2])                   => 3
         ((reducer +) 1 [1 2])                 => 4
         ((reducer + 3) [1 2])                 => 6
         ((reducer (constantly ...any...)) []) => ...any... ; reduce special case
         ((reducer ...fn... ...any...) [])     => ...any...
         ((reducer +) 4)                       => (throws IllegalArgumentException)
         ((reducer + 3) 4 [])                  => (throws clojure.lang.ArityException)
         ((reducer 1) [])                      => (throws ClassCastException)
         ((reducer 1) ...any...)               => (throws ClassCastException)
         ((reducer nil ...any...) [...xs...])  => (throws NullPointerException)))
