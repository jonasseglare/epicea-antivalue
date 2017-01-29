(ns epicea.antivalue.core
  (:import [epicea.antivalue AntivalueException])
  (:require [epicea.tag.core :as tag]
            [clojure.spec :as spec]))

(defmacro dout [x]
  `(let [x# ~x]
     (println "############## " ~(str x) "=" x#)
     x#))

(spec/def ::if-sym #(= 'if %))
(spec/def ::expr (constantly true))
(spec/def ::if-form (spec/cat :if-sym ::if-sym
                              :test ::expr
                              :on-true ::expr
                              :on-false (spec/? ::expr)))

(spec/def ::binding (spec/cat :symbol symbol?
                              :expr ::expr))
(spec/def ::bindings (spec/spec (spec/* ::binding)))
(spec/def ::form (constantly true))
(spec/def ::forms (spec/* ::form))
(spec/def ::let-symbol (constantly true)); #(= `let %))

(spec/def ::basic-let-form (spec/cat
                            :let-symbol ::let-symbol
                            :bindings ::bindings
                            :forms ::forms))

(spec/def ::loop-symbol (constantly true))

(spec/def ::loop-form (spec/cat
                       :loop-symbol ::loop-symbol
                       :bindings ::bindings
                       :forms ::forms))

(spec/def ::fn-symbol (constantly true))
(spec/def ::fn-name symbol?)

(spec/def ::fn-args (spec/spec
                     (spec/coll-of symbol?)))


(spec/def ::fn-arity (spec/spec
                      (spec/cat
                       :args ::fn-args
                       :forms ::forms)))


(spec/def ::fn-form (spec/cat
                     :fn-symbol ::fn-symbol
                     :fn-name (spec/? ::fn-name)
                     :fn-arities (spec/* ::fn-arity)))

(spec/def ::type (constantly true))

(spec/def ::finally-symbol #(= % 'finally))
(spec/def ::catch-symbol #(= % 'catch))

(spec/def ::catch-form (spec/spec
                        (spec/cat
                         :catch-symbol ::catch-symbol
                         :type ::type
                         :var-name symbol?
                         :forms ::forms)))

(spec/def ::finally-form (spec/spec
                          (spec/cat
                           :finally-symbol ::finally-symbol
                           :forms ::forms)))

(spec/def ::non-catch #(and (not (spec/valid? ::catch-form %))
                            (not (spec/valid? ::finally-form %))))

(spec/def ::try-form (spec/cat
                      :try-symbol symbol?
                      :forms (spec/* ::non-catch)
                      :catch-forms (spec/* ::catch-form)
                      :finally-form (spec/? ::finally-form)))

(defmacro wrap [x]
  `(try
     (tag/tag-success ~x)
     (catch AntivalueException e#
       (tag/tag-failure (.state e#)))))

(defn unwrap [x]
  (let [y (tag/value x)]
    (if (tag/success? x)
      y
      (throw (AntivalueException. y)))))

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

(def anti ::anti)
(def either-sub ::either-sub)

(defn evals-to-keyword [kwd]
  (fn [x]
    (try
      (and (symbol? x)
           (= kwd (eval x)))
      (catch Throwable e
        false))))

(def anti-sym? (evals-to-keyword ::anti))

(def special-forms {'if :if ; OK
                    'do :do ;; OK
                    'let* :let ;; OK
                    'loop* :loop ;; OK
                    'recur :recur ;; OK
                    'throw :throw ;; OK
                    'def :def ;; ?
                    'var :var ;; ?
                    'monitor-enter :monitor-enter
                    'monitor-exit :monitor-exit
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

(defn either-sym? [x]
  (try
    (or (= 'either x)
        (= `either x)
        (= "either" (name x)))
    (catch Throwable _ false)))

(defn compile-either [deps args]
  (defined
    (if (empty? args) 
      nil
      `(try ~(tag/value (compile-sub deps (first args)))
            (catch AntivalueException ~(gensym)
              ~(tag/value (compile-either deps (rest args))))))))

(defn compile-anti [deps args]
  (assert (= 1 (count args)))
  (undefined
   (let [x (compile-sub deps (first args))]
     (if (defined? x)
       `(throw (AntivalueException. ~(tag/value x)))
       `(let [k# (wrap ~(tag/value x))]
          (if (tag/success? k#)
            (throw (AntivalueException. (tag/value k#)))
            (tag/value k#)))))))
    

(defn compile-binding [[deps bindings] {:keys [symbol expr]}]
  (let [c (compile-sub deps expr)
        v (tag/value c)]
    (println "COMPILE BINDING FOR" expr "->" c)
    (if (defined? c)
      [(disj deps symbol)
       (into bindings [symbol v])]
      (let [k `(wrap ~v)]
        [(conj deps symbol)
         (into bindings [symbol k])]))))
                                

(defn compile-bindings [deps bindings]
  (reduce
   compile-binding
   [deps []]
   bindings))

(defn compile-let-or-loop [p deps0 bindings0 forms]
  (let [[deps bindings] (compile-bindings deps0 bindings0)]
    (with-compiled [body (map (compile-sub deps) forms)]
      (dout body)
      `(~p ~bindings
        ~@body))))

(defn compile-let [deps form]
  (let [f (spec/conform ::basic-let-form form)]
    (if (= f ::spec/invalid)
      (error (spec/explain  ::basic-let-form form))
      (compile-let-or-loop `let deps (:bindings f) (:forms f)))))

(defn compile-do [deps args]
  (with-compiled [a (map (compile-sub deps) args)]
    `(do ~@a)))

(defn compile-catch-sub [deps f]
  (with-compiled [body (map (compile-sub deps) (:forms f))]
    `(catch ~(:type f) ~(:var-name f)
       ~@body)))

(defn compile-catch [deps form]
  (let [f (spec/conform ::catch-form form)]
    (if (= ::spec/invalid f)
      (error (spec/explain ::catch-form form))
      (compile-catch-sub deps f))))

(defn compile-throw [deps args]
  (with-compiled [[c] (map (compile-sub deps) args)]
    `(throw ~c)))

(defn compile-seq-sub [deps form] 
  (let [f (first form)
        sp (get special-forms f)
        args (rest form)]
    (cond
      (anti-sym? f) (dout (compile-anti (dout deps) (dout args)))
      (= :do sp) (compile-do deps args)
      (= :let sp) (compile-let deps form)
      (= :catch sp) (compile-catch deps form)
                                        ;(= :throw sp) (compile-throw deps args)
      (= :loop sp) (defined form)
      (= :quote sp) (defined form)
      (= :fn sp) (defined form)
      :default (compile-basic-seq deps form))))

(defn compile-seq [deps form]
  (println "COMPILE SEQ with deps=" deps "and form=" form)
  (if (either-sym? (first form))
    (compile-either deps (rest form))
    (compile-seq-sub deps (macroexpand form))))

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

(defn compile-primitive [deps form]
  (if (contains? deps form)
    (undefined `(unwrap ~form))
    (defined form)))

(defn compile-sub 
  ([deps form]
   ;(println "COMPILE-SUB on" form "with deps=" deps)
   ((cond
      (seq? form) compile-seq
      (vector? form) compile-vector
      (map? form) compile-map
      (set? form) compile-set
      :default compile-primitive) deps form))
  ([deps] #(compile-sub deps %)))

(defmacro either [& forms]
  (println "-------------------EITHER MACRO INVOKED!!!")
  (tag/value (compile-either #{} forms)))

(defmacro expect 
  ([f? x g]
   `(let [x# ~x]
      (make (~f? x#) x# (~g x#))))
  ([f? x]
   `(expect ~f? ~x identity)))

(defmacro make [p a b]
  `(if ~p ~a (anti ~b)))
