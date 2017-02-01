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

(defn antivalue-or-nil [x]
  (if (antivalue? x) x))

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

(defn prepare-arg [arg]
  (merge
   {:expr (tag/value arg)}
   (if (defined? arg)
     {:antivalue? false}
     {:antivalue? true
      :sym (gensym)})))

(defn first-antivalue [args]
  `(or ~@(map (fn [arg] `(antivalue-or-nil ~(:expr arg))) (filter :antivalue? args))))

(defn make-farg-binding [prepared]
  (if (contains? prepared :sym)
    [(:sym prepared) (:expr prepared)]
    []))

(defn prepare-args [state compiled-args cb]
  (let [prepared (map prepare-arg compiled-args)]
    `(let ~(reduce into [] (make-farg-binding prepared))
       `(if-let [av# ~(first-antivalue prepared)]
          av#
          ~(cb (map :expr prepared))))))

(defn compile-fun-call [state f args0]
  (let [args (map prepare-arg (compile-args state args0))]
    nil))

(defn compile-if [state x]
  nil)

(defn compile-seq-sub [state x]
  (let [[f & r] x
        sf (get macro/special-forms f)]
    (cond
      (= :if sf) (compile-if state x)
      :default (compile-fun-call state f x))))

(defn compile-seq [state x]
  (let [f (first x)]
    (cond
      (macro/compare-symbols `anti f) (compile-anti state x)
      (macro/compare-symbols `either f) (compile-either state (compile-args state (rest x)))
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
