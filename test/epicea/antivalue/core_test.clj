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

)

;; NOT YET GOOD!
;; 
