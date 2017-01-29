(ns epicea.antivalue.core
  (:import [epicea.antivalue AntivalueException])
  (:require [epicea.tag.core :as tag]))

(declare make)

(def make ::make)
(def katt ::katt)

(defn evals-to-keyword [kwd]
  (fn [x]
    (try
      (and (symbol? x)
           (= kwd (eval x)))
      (catch Throwable e
        false))))

(defmacro katt? [x]
  (= ::katt (eval x)))

(def make-sym? (evals-to-keyword ::make))

(def special-forms {'if :if ;; OK
                    'do :do ;; OK
                    'let* :let ;; OK
                    'loop* :loop ;; OK
                    'recur :recur ;; OK
                    'throw :throw ;; OK
                    'def :def ;; OK
                    'var :var ;; OK
                    'monitor-enter :monitor-enter ;; OK
                    'monitor-exit :monitor-exit ;; OK
                    'fn* :fn ;; OK
                    'try :try ;; OK
                    'catch :catch ;; OK
                    'quote :quote ;; OK
                    })

(defn error [& s]
  (throw (RuntimeException. (apply str s))))

(declare compile-sub)

(defn compile-basic-seq [deps form]
  (map (compile-sub deps) form))

(defn compile-either [deps args]
  (if (empty? args)
    nil
    `(try ~(compile-sub deps (first args))
          (catch AntivalueException ~(gensym)
            ~(compile-either deps (rest args))))))

(defn compile-make [deps [p on-true on-false]]
  `(if ~(compile-sub deps p) 
     ~(compile-sub deps on-true)
     (throw (AntivalueException. ~(compile-sub deps on-false)))))

(defn compile-seq [deps form] 
  (let [f (first form)
        args (rest form)]
    (cond
      (make-sym? f) (compile-make deps args)
      :default (compile-basic-seq deps form))))

(defn compile-vector [deps form]
  (into [] (map (compile-sub deps) form)))

(defn compile-pair [deps]
  (fn [pair]
    (vec (map (compile-sub deps) pair))))

(defn compile-map [deps form]
  (into {} (map (compile-pair deps) form)))

(defn compile-set [deps form]
  (into #{} (map (compile-sub deps) form)))

(defn unwrap-dep-value [x]  
  (let [y (tag/value x)]
    (if (tag/success? x)
      y
      (throw (AntivalueException. y)))))
  
(defn compile-primitive [deps form]
  (if (contains? deps form)
    `(unwrap-dep-value ~form)
    form))

(defn compile-sub 
  ([deps form]
   (println "Compile-sub on " form)
   ((cond
      (seq? form) compile-seq
      (vector? form) compile-vector
      (map? form) compile-map
      (set? form) compile-set
      :default compile-primitive) deps form))
  ([deps] #(compile-sub deps %)))

(defmacro either [& forms]
  (compile-either #{} forms))
