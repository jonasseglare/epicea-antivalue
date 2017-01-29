(ns epicea.antivalue.spec
  (:require [epicea.antivalue.core :as av]
            [clojure.spec :as spec]))

(defmacro conform [sp x]
  `(av/expect
    (fn [x#] (not (= ::spec/invalid x#)))
    (spec/conform ~sp ~x)))

