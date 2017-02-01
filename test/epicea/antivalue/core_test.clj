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
  (compiles-identically 'a))
