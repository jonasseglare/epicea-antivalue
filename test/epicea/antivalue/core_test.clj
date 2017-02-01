(ns epicea.antivalue.core-test
  (:import [epicea.antivalue AntivalueException])
  (:require [clojure.test :refer :all]
            [epicea.tag.core :as tag]
            [epicea.antivalue.core :as av]))

;(defmacro compiles-identically [x]
;  `(is (= ~x (tag/value (av/compile-sub #{} ~x)))))

(deftest primitives
  (is true))
