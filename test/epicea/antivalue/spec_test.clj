(ns epicea.antivalue.spec
  (:require [clojure.test :refer :all]
            [epicea.antivalue.spec :as avspec]
            [clojure.spec :as spec]
            [epicea.antivalue.core :as av]))

(spec/def ::re number?)
(spec/def ::im number?)
(spec/def ::complex {:req-un [::re ::im]})
(spec/def ::complex2 (spec/cat :re ::re
                               :im ::im))

(deftest basic-test
  (is (spec/valid? ::complex {:re 9 :im 20}))
  (is (not (spec/valid? ::complex {:re 9})))
  (is (= {:re 9 :im 20} (av/either (avspec/conform ::complex2 [9 20])
                                   :bs)))
  (is (= :bs (av/either (avspec/conform ::complex2 [9])
                        :bs))))


