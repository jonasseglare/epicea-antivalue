(ns epicea.antivalue.core-test
  (:import [epicea.antivalue AntivalueException])
  (:require [clojure.test :refer :all]
            [epicea.tag.core :as tag]
            [epicea.antivalue.core :as av]))

(deftest contextual-make
  ;; The call to 'av/make' should only work inside the av/either macro
  (is (= 4 (av/either (av/make false 9 3) 4)))
  (is (= 9 (av/either (av/make true 9 3) 4)))

  ;; But outside the av/either macro, 'av/make' should be undefined.
  (is (try
        av/make
        false

        (catch Throwable x
          true))))



(defmacro compiles-identically [x]
  `(is (= ~x (av/compile-sub #{} ~x))))

(deftest a-test
  (testing "FIXME, I fail."
    (compiles-identically 1)
    (compiles-identically [1 2 3])
    (compiles-identically :a)
    (compiles-identically 'b)
    (compiles-identically #{1 :a 3})
    (compiles-identically {:a 3 :b 4})
    (compiles-identically '(1 :a 3))
    (is (= 9 (av/unwrap-dep-value (tag/tag-success 9))))
    (try
      (av/unwrap-dep-value (tag/tag-failure 9))
      (is false)
      (catch AntivalueException e
        (is (= 9 (.state e)))))
    (is (av/compile-sub #{'a} 'a))

    (is (= 3 (av/either 3 9)))

    ;; The call to 'av/make' should only work inside the av/either macro
    (is (= 4 (av/either (av/make false 9 3) 4)))
    (is (= 9 (av/either (av/make true 9 3) 4)))
))

