(ns epicea.antivalue.core
  (:import [epicea.antivalue AntivalueException])
  (:require [epicea.tag.core :as tag]))


(defn compile-seq [deps form] nil)

(defn compile-vector [deps form] nil)

(defn compile-map [deps form] nil)

(defn compile-set [deps form] nil)

(defn compile-dep-sym [deps form] nil)
  
(defn compile-primitive [deps form]
  (if (contains? deps form)
    (compile-dep-sym form)
    form))

(defn compile-sub [deps form]
  ((cond
    (seq? form) compile-seq
    (vector? form) compile-vector
    (map? form) compile-map
    (set? form) compile-set
    :default compile-primitive) deps form))

(defmacro either [& form]
  (compile-sub #{} `(either ~@form)))
