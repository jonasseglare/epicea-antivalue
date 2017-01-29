(ns epicea.antivalue.core-test
  (:import [epicea.antivalue AntivalueException])
  (:require [clojure.test :refer :all]
            [epicea.tag.core :as tag]
            [epicea.antivalue.core :refer :all]))

(defmacro compiles-identically [x]
  `(is (= ~x (compile-sub #{} ~x))))

(deftest a-test
  (testing "FIXME, I fail."
    (compiles-identically 1)
    (compiles-identically [1 2 3])
    (compiles-identically :a)
    (compiles-identically 'b)
    (compiles-identically #{1 :a 3})
    (compiles-identically {:a 3 :b 4})
    (compiles-identically '(1 :a 3))
    (is (= 9 (unwrap-dep-value (tag/tag-success 9))))
    (try
      (unwrap-dep-value (tag/tag-failure 9))
      (is false)
      (catch AntivalueException e
        (is (= 9 (.state e)))))
    (is (compile-sub #{'a} 'a))
    (is (= 3 (either 3 9)))
    (is (= 4 (either (make false 9 3) 4)))
    (is (= 9 (either (make true 9 3) 4)))))
