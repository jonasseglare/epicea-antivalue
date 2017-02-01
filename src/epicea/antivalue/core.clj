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
  (assert (= 2 (count x)))
  (undefined `(anti ~(tag/value (compile-sub state (second x))))))

(defn compile-args [state args]
  (map (compile-sub state) args))

(defn compile-either [state args]
  (let [[f & r] args]
    (if (defined? f)
      f
      (if (empty? r)
        f
        (undefined 
         `(let [y# ~(tag/value f)]
            (if (antivalue? y#)
              ~(tag/value (compile-either state r))
              y#)))))))

(defn compile-seq-sub [state x]
  (macro/error "Not impl"))

(defn compile-seq [state x]
  (let [f (first x)]
    (cond
      (compare-symbols `anti f) (compile-anti state x)
      (compare-symbols `either f) (compile-either state (compile-args state (rest x)))
      :default (compile-seq-sub state (macroexpand x)))))

(defn compile-sub 
  ([state x]
   (cond
     (seq? x) (compile-seq state x)
     :default (compile-primitive state x)))
  ([state]
   (fn [x] (compile-sub state x))))

(defmacro export [x]
  (tag/value (compile-sub init-state x)))
