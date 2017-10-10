(ns bluebell.antivalue.spec
  (:require [bluebell.antivalue.core :as av]
            [clojure.spec.alpha :as spec]))

(defmacro conform [sp x]
  `(av/expect
    (fn [x#] (not (= ::spec/invalid x#)))
    (spec/conform ~sp ~x)))
