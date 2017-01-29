(ns epicea.antivalue.core
  (:import [epicea.antivalue AntivalueException])
  (:require [epicea.tag.core :as tag]))

(def defined (tag/tag :defined))
(def undefined (tag/tag :undefined))

(def defined? (tag/tagged? :defined))
(def undefined? (tag/tagged? :undefined))

(defn fn-with-compiled [f vals]
  ((if (every? defined? vals)
     defined undefined) 
   (f (map tag/value vals))))

(defmacro with-compiled [[sym expr] & body]
  `(fn-with-compiled 
    (fn [~sym] ~@body)
    ~expr))

(def make ::make)

(defn evals-to-keyword [kwd]
  (fn [x]
    (try
      (and (symbol? x)
           (= kwd (eval x)))
      (catch Throwable e
        false))))

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
  (with-compiled [cmp (map (compile-sub deps) form)]
    cmp))

(defn compile-either [deps args]
  (defined
    (if (empty? args) 
      nil
      `(try ~(tag/value (compile-sub deps (first args)))
            (catch AntivalueException ~(gensym)
              ~(tag/value (compile-either deps (rest args))))))))

(defn compile-make [deps args]
  (let [[p on-true on-false] (map (comp tag/value (compile-sub deps)) args)]
    (undefined
     `(if ~p
        ~on-true
        (throw (AntivalueException. ~on-false))))))

(defn compile-seq [deps form] 
  (let [f (first form)
        args (rest form)]
    (cond
      (make-sym? f) (compile-make deps args)
      :default (compile-basic-seq deps form))))

(defn compile-vector [deps form]
  (with-compiled [args (map (compile-sub deps) form)]
    (into [] args)))

(defn compile-pair [deps]
  (fn [pair]
    (with-compiled [args (map (compile-sub deps) pair)]
      (vec args))))

(defn compile-map [deps form]
  (with-compiled [args (map (compile-pair deps) form)]
    (into {} args)))

(defn compile-set [deps form]
  (with-compiled [args (map (compile-sub deps) form)]
    (into #{} args)))

(defn unwrap-dep-value [x]  
  (let [y (tag/value x)]
    (if (tag/success? x)
      y
      (throw (AntivalueException. y)))))
  
(defn compile-primitive [deps form]
  (if (contains? deps form)
    (undefined `(unwrap-dep-value ~form))
    (defined form)))

(defn compile-sub 
  ([deps form]
   ((cond
      (seq? form) compile-seq
      (vector? form) compile-vector
      (map? form) compile-map
      (set? form) compile-set
      :default compile-primitive) deps form))
  ([deps] #(compile-sub deps %)))

(defmacro either [& forms]
  (tag/value (compile-either #{} forms)))
