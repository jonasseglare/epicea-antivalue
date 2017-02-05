(ns epicea.antivalue.core-test
  (:import [epicea.antivalue AntivalueException])
  (:require [clojure.test :refer :all]
            [epicea.tag.core :as tag]
            [epicea.antivalue.core :refer :all]))

(defmacro compiles-identically [x]
  `(is (= ~x (tag/value (compile-sub init-state ~x)))))

(deftest primitives
  (is (antivalue? (antivalue 3)))
  (is (not (antivalue? 3)))
  (compiles-identically 3)
  (compiles-identically 'a)
  (is (antivalue? (anti 3)))
  (is (= 3 (anti (anti 3))))
  (is (antivalue? (export (anti 3))))
  (is (not (antivalue? (export 3))))
  (is (= 3 (export (either 3 4))))
  (is (= 4 (export (either (anti 3) 4))))
  (is (= 3 (export (either (anti (anti 3)) 4))))
  (is (contains? (prepare-arg (undefined 3)) :sym))
  (is (not (contains? (prepare-arg (defined 3)) :sym)))
  (let [[a b] (make-farg-binding (prepare-arg (undefined 3)))]
    (is (symbol? a))
    (is (= 3 b)))

  (is (= [] (make-farg-binding (prepare-arg (defined 3)))))
  (is (= 6 (export (+ 1 2 3))))
  (let [x (export (+ (anti 1) 2 3))]
    (is (= 1 (:data x)))
    (is (antivalue? x)))
  (is (antivalue? (export (import (antivalue 3)))))
  (is (= 3 (export (if true 3 4))))
  (is (= 4 (export (if false 3 4))))
  (is (antivalue? (export (if (anti 9) 3 4))))
  (is (antivalue? (export (if true (anti 3) 4))))
  (is (= 3 (export (if true 3 (anti 4)))))
  (is (antivalue? (export (if false 3 (anti 4)))))
  (is (= 4 (export (if false (anti 3) 4))))
  (is (= 4 (export (if true 4))))
  (is (antivalue? (export (if true (anti 4)))))
  (is (nil? (export (if false (anti 3)))))
  (is (= '(4 1 (anti 2) 3) (export (conj '(1 (anti 2) 3) 4))))
  (is (= 3 (export (let [] 3))))
  (is (= 3 (export (let [a 3] a))))
  (is (= 4 (export (let [a 3 b 1] (+ a b)))))
  (is (= 4 (export (let [a (anti 3) b 2] (* b b)))))
  (is (antivalue? (export (let [a (anti 3) b 2] (+ a b)))))
  (is (= 5 (export (let [a (anti (anti 3)) b 2] (+ a b)))))
  (is (= 5 (export (let [a (anti (anti 3)) b (anti (anti 2))] (+ a b)))))
  (is (= 3 (export (do 1 2 3))))
  (let [y (export (do 1 (anti 2) 3))]
    (is (antivalue? y))
    (is (= 2 (anti y))))
  (is (= #{1 2 3} (export #{1 2 (either 3 4)})))
  (is (= {:a 2 :b 6} (export {:a (either 2 3) :b (either (anti 5) 6)})))
  (is (= (compile-try init-state '(try 1 2 3 (catch RuntimeError x (+ x 4)) (finally 5)))
         [:defined '(try (do 1 (do 2 3)) (catch RuntimeError x (+ x 4)) (finally 5))]))
  (is (= 1 (export (try 1))))
  (is (= 3 (export (let [a (anti (anti 3))]
                     (let [a (anti (anti 4))]
                       a)
                     a))))
                               

)




(defn factorial [x] (export (if (= 0 x) 1 
                                (* (expect number? x) 
                                   (factorial (- (expect number? x) 1))))))

(defn factorial-2 [x0]
  (top
   (let [x (expect number? x0)]
     (either
      (if (<= x 0) 
        1
        (* x (factorial-2 (- x 1))))
      [:bad-input (anti x)]
      nil))))
         
(defn factorial-loop [x]
  (top
   (loop [i x result 1]
     (if (= 0 i)
       result
       (recur (- i 1) (* result i))))))

(deftest factorial-test
  (is (= 1 (factorial 0)))
  (is (= (* 4 3 2) (factorial 4)))
  (is (= 24 (factorial-2 4)))
  (is (= [:bad-input :a] (factorial-2 :a))))
  
  
;; NOT YET GOOD!
;; 
