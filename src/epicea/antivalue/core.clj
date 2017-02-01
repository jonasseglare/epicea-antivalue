(ns epicea.antivalue.core
  (:import [epicea.antivalue AntivalueException])
  (:require [epicea.tag.core :as tag]
            [clojure.spec :as spec]
            [epicea.utils.macro :refer :all :as macro]
            [epicea.utils.debug :refer [dout]]))

(declare compile-sub)
(def defined (tag/tag :defined))
(def undefined (tag/tag :undefined))

(def defined? (tag/tagged? :defined))
(def undefined? (tag/tagged? :undefined))

(def init-state {:undefined #{} :debug? false})

(defrecord Antivalue [data])

(defn antivalue [x]
  (->Antivalue x))

(defn antivalue? [x]
  (= (class x) Antivalue))

(defn has-undefined? [state x]
  (contains? (:undefined state) x))

(defn compile-primitive [state x] 
  ((if (has-undefined? state x)
     undefined
     defined) x))

(defn compile-sub [state x]
  (cond
    :default (compile-primitive state x)))
