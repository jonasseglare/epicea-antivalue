(ns epicea.antivalue.core-test
  (:require [clojure.test :refer :all]
            [epicea.antivalue.core :refer :all]))

(defmacro compiles-identically [x]
  `(is (= ~x (compile-sub #{} ~x))))

(deftest a-test
  (testing "FIXME, I fail."
    (compiles-identically 1)
    (compiles-identically [1 2 3])
    (compiles-identically :a)
    (compiles-identically 'b)))
