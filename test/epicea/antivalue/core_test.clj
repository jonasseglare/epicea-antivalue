(ns epicea.antivalue.core-test
  (:import [epicea.antivalue AntivalueException])
  (:require [clojure.test :refer :all]
            [epicea.tag.core :as tag]
            [epicea.antivalue.core :as av]))

(defmacro compiles-identically [x]
  `(is (= ~x (tag/value (av/compile-sub #{} ~x)))))

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

    (let [k (av/fn-with-compiled 
              (fn [vals]
                `(:mjao ~@vals))
              [(av/defined 3) (av/defined 4)])]
      (is (= [:defined '(:mjao 3 4)] k)))

    (is (= [:undefined '(:mjao 3 4)]
           (av/fn-with-compiled
             (fn [vals]
               `(:mjao ~@vals))
             [(av/undefined 3) (av/defined 4)])))
))

