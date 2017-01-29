(ns epicea.antivalue.core
  (:import [epicea.antivalue AntivalueException])
  (:require [epicea.tag.core :as tag]))

(declare compile-sub)

(defn compile-seq [deps form] 
  (map (compile-sub deps) form))

(defn compile-vector [deps form]
  (into [] (map (compile-sub deps) form)))

(defn compile-pair [deps]
  (fn [pair]
    (vec (map (compile-sub deps) pair))))

(defn compile-map [deps form]
  (into {} (map (compile-pair deps) form)))

(defn compile-set [deps form]
  (into #{} (map (compile-sub deps) form)))

(defn compile-dep-sym [deps form] nil)
  
(defn compile-primitive [deps form]
  (if (contains? deps form)
    (compile-dep-sym form)
    form))

(defn compile-sub 
  ([deps form]
   ((cond
      (seq? form) compile-seq
      (vector? form) compile-vector
      (map? form) compile-map
      (set? form) compile-set
      :default compile-primitive) deps form))
  ([deps] #(compile-sub deps %)))

(defmacro either [& form]
  (compile-sub #{} `(either ~@form)))
