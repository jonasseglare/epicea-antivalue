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
  (let [x (export (anti 3))]
    (is (undefined? x))
    (is (antivalue? (tag/value x))))
  (let [x (export 3)]
    (is (defined? x))
    (is (= 3 (tag/value x)))))
