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
    (is (= 9 (av/unwrap (tag/tag-success 9))))
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
    (is (= 18 (av/either (let [a 9] (+ a a)))))
    (is (= 4 (av/either (let [a (av/make false 3 9)] a) 4)))
    (is (= 9 (av/either (let [a (av/make true 3 1234) b (* a a)] b) 4)))

    ;; Loops are currently not compiled
    (is (= 10 (av/either (loop [sum 0
                                i 4]
                           (if (= i 0)
                             sum
                             (recur (+ sum i) (- i 1)))))))

    (is (= 129 (av/either (if (av/make false true true) 9 19) 129)))
    (is (= 129 (av/either (if true (av/make false true true) 19) 129)))
    (is (= 9 (av/either (if true 9 (av/make false true true)) 129)))
    (is (= '(av/make 3) (av/either '(av/make 3) 9)))
    (is (= 120 (av/either (throw (av/make false 3 4))
                          120)))

    (is (= 119 (av/either (try (assert false) (catch Throwable a 119)) 120)))
    (is (= 7 (av/either (av/anti (av/make false 9 7)) 8)))
    (is (= 8 (av/either (av/make false 9 7) 8)))

    (is (= (av/either (let [a (av/make false 9 7)]
                        (av/either [:failure (av/anti a)]
                                   [:success a]))
                      nil)
           [:failure 7]))

    (is (= (av/either (let [a (av/make true 9 7)]
                        (av/either [:failure (av/anti a)]
                                   [:success a]))
                      nil)
           [:success 9]))
        

))

