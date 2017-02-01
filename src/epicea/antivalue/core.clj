(ns epicea.antivalue.core
  (:import [epicea.antivalue AntivalueException])
  (:require [epicea.tag.core :as tag]
            [clojure.spec :as spec]
            [epicea.utils.macro :as macro]
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

(defn anti [x]
  (if (antivalue? x)
    (:data x)
    (antivalue x)))

(defn compile-anti [state x]
  (dout x)
  (assert (= 2 (count x)))
  (undefined `(anti ~(compile-sub state (second x)))))

(defn compile-either [state x]
  x)

(defn compile-seq [state x]
  (dout x)
  (let [f (first x)]
    (cond
      (compare-symbols `anti f) (compile-anti state x)
      (compare-symbols `either f) (compile-either state x)
      :default (compile-seq state (macroexpand x)))))

(defn compile-sub [state x]
  (cond
    (seq? x) (compile-seq state x)
    :default (compile-primitive state x)))

(defmacro export [x]
  (compile-sub init-state x))
