(ns epicea.antivalue.core
  (:import [epicea.antivalue AntivalueException])
  (:require [epicea.tag.core :as tag]
            [clojure.spec :as spec]
            [epicea.utils.macro :as macro]
            [epicea.utils.debug :refer [dout]]))

(declare compile-sub)
(def defined (tag/tag :defined))
(def undefined (tag/tag :undefined))

(defn defined? [x]
  (assert (tag/pair? x))
  (tag/tagged? :defined x))

(defn undefined? [x]
  (assert (tag/pair? x))
  (tag/tagged? :undefined x))

(def init-state {:undefined #{} :debug? false})

(defn add-undefined [state x]
  (update-in state [:undefined]
             #(conj % x)))

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

(defn compile-anti [state args]
  (assert (= 1 (count args)))
  (undefined `(anti ~(tag/value (compile-sub state (first args))))))

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

(defn make-farg-bindings [prepared]
  (reduce into [] (map make-farg-binding prepared)))

(defn wrap-arg-binding [state prepared cb]
  (let [bindings (make-farg-bindings prepared)
        subexpr (cb (map :expr prepared))]
    (if (empty? bindings)
      subexpr
      `(let ~bindings
         (if-let [av# ~(first-antivalue prepared)]
           av#
           ~subexpr)))))

(defn prepare-args [state args]
  (map prepare-arg (compile-args state args)))

(defn prepared-has-undefined? [x]
  (first (filter :antivalue? x)))

(defn compile-fun-call [state f args0]
  (let [prepared (prepare-args state args0)]
    ((if (prepared-has-undefined? prepared)
       undefined defined)
     (wrap-arg-binding
      state prepared
      (fn [args]
        `(~f ~@args))))))

(defn compile-if [state x]
  (let [parsed (spec/conform ::macro/if-form x)]
    (assert (not (= parsed ::spec/invalid)))
    (let [c (compile-sub state (:test parsed))
          on-true (compile-sub state (:on-true parsed))
          on-false (compile-sub state (:on-false parsed))
          branches (map tag/value [on-true on-false])
          all-defined (and (defined? c) (defined? on-true) (defined? on-false))]
      ((if all-defined
         defined
         undefined)
       (if (defined? c)
         `(if ~(tag/value c)
            ~@branches)
         `(let [y# ~(tag/value c)]
            (if (antivalue? y#)
              y#
              (if y# ~@branches))))))))

(defn compile-binding [[state bindings] b]
  (let [c (compile-sub state (:expr b))]
    [(if (defined? c)
       state
       (add-undefined state (:symbol b)))
     (into bindings [(:symbol b) (tag/value c)])]))

(defn compile-bindings [state bindings]
  (reduce compile-binding [state []] bindings))

(defn compile-let [state0 x]
  (let [parsed (spec/conform ::macro/basic-let-form x)]
    (assert (not (= parsed ::macro/invalid)))
    (let [[state bindings] (compile-bindings state0 (:bindings parsed))
          body (compile-sub state `(do ~@(:forms parsed)))]
      (println "state = " state)
      (println "bindings = " bindings)
      (println "body = " body)
      (dout
       (tag/tag 
        (tag/get-tag body)
        `(let ~(vec bindings)
           ~(tag/value body)))))))


(defn compile-seq-sub [state x]
  (let [[f & args] x
        sf (get macro/special-forms f)]
    (cond
      (= :if sf) (compile-if state x)
      (= :quote sf) x
      (= :let sf) (compile-let state x)
      :default (compile-fun-call state f args))))

(defn compile-import [state args]
  (assert (= 1 (count args)))
  (undefined (tag/value (compile-sub state (first args)))))

(defn compile-seq [state x]
  (let [[f & args] x]
    (cond
      (macro/compare-symbols `import f) (compile-import state args)
      (macro/compare-symbols `anti f) (compile-anti state args)
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

(defmacro either [x]
  (let [c (compile-either init-state x)]
    (assert (defined? x))
    (tag/value c)))
