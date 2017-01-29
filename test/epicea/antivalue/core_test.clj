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
      (av/unwrap (tag/tag-failure 9))
      (is false)
      (catch AntivalueException e
        (is (= 9 (.state e)))))
    (is (av/compile-sub #{'a} 'a))

    (is (= 3 (av/either 3 9)))

    ;; The call to 'av/condanti' should only work inside the av/either macro
    (is (= 4 (av/either (av/condanti false 9 3) 4)))
    (is (= 9 (av/either (av/condanti true 9 3) 4)))

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
    (is (= 4 (av/either (let [a (av/condanti false 3 9)] a) 4)))
    (is (= 9 (av/either (let [a (av/condanti true 3 1234) b (* a a)] b) 4)))

    ;; Loops are currently not compiled
    (is (= 10 (av/either (loop [sum 0
                                i 4]
                           (if (= i 0)
                             sum
                             (recur (+ sum i) (- i 1)))))))

    (is (= 129 (av/either (if (av/condanti false true true) 9 19) 129)))
    (is (= 129 (av/either (if true (av/condanti false true true) 19) 129)))
    (is (= 9 (av/either (if true 9 (av/condanti false true true)) 129)))
    (is (= '(av/condanti 3) (av/either '(av/condanti 3) 9)))
    (is (= 120 (av/either (throw (av/condanti false 3 4))
                          120)))

    (is (= 119 (av/either (try (assert false) (catch Throwable a 119)) 120)))
    (is (= 7 (av/either (av/anti (av/condanti false 9 7)) 8)))
    (is (= 8 (av/either (av/condanti false 9 7) 8)))

    (is (= 9 (av/either (let [a (av/condanti false 7 9)]
                          (av/either (av/anti a))))))

    (is (= (av/either (let [a (av/condanti false 9 7)]
                        (av/either [:failure (av/anti a)]
                                   [:success a]))
                      nil)
           [:failure 7]))

    (is (= (av/either (let [a (av/condanti true 9 7)]
                        (av/either [:failure (av/anti a)]
                                   [:success a]))
                      nil)
           [:success 9]))

    (is (= 3 (av/either (av/expect number? 3) :a)))
    (is (= :a (av/either (av/expect number? :b) :a)))
    (is (= :a (av/either (av/expect number? :b (fn [k] [:not-a-number k])) :a)))
    (is (= [:not-a-number :b]
           (av/either
            (let [a (av/expect number? :b (fn [k] [:not-a-number k]))]
              (av/either a
                         (av/anti a))))))

    (is (= [:not-a-number :b]
           (av/either
            (let [a (av/expect number? :b)]
              (av/either a
                         [:not-a-number (av/anti a)])))))
              
        

))

(defn add-safe-0 [a b]
  (av/either (+ (av/expect number? a)
                (av/expect number? b))
             :bad-input))

(defn add-safe-1 [a b]
  (av/either
   (let [anum (av/expect number? a)
         bnum (av/expect number? b)]
     (av/either (+ anum bnum)
                [:bad-a (av/anti anum)]
                [:bad-b (av/anti bnum)]))))

(defn add-safe-2 [a b]
  (av/either
   (let [anum (av/expect number? a)
         bnum (av/expect number? b)]
     (av/either [:bad-a (av/anti anum)]
                [:bad-b (av/anti bnum)]
                (+ anum bnum)))))
    

(deftest add-safe-test
  (is (= 9 (add-safe-0 4 5)))
  (is (= :bad-input (add-safe-0 4 nil)))
  (is (= :bad-input (add-safe-0 :kattskit 5)))

  (is (= 9 (add-safe-1 4 5)))
  (is (= [:bad-a :a] (add-safe-1 :a 5)))
  (is (= [:bad-b :b] (add-safe-1 4 :b)))

  (is (= 9 (add-safe-2 4 5)))
  (is (= [:bad-a :a] (add-safe-2 :a 5)))
  (is (= [:bad-b :b] (add-safe-2 4 :b)))

)
