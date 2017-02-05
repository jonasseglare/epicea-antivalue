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

(defn remove-undefined [state x]
  (update-in state [:undefined]
             #(disj % x)))

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

(defn compile-either-sub [state args]
  (let [[f & r] args
        firstval (tag/value f)]
    (if (defined? f)
      firstval
      (if (empty? r)
        firstval
        `(let [y# ~firstval]
           (if (antivalue? y#)
             ~(compile-either-sub state r)
             y#))))))

(defn compile-either [state args]
  (let [cargs (compile-args state args)]
    ((if (some defined? cargs) defined undefined)
     (compile-either-sub state cargs))))

(defn prepare-arg [arg]
  (merge
   {:expr (tag/value arg)}
   (if (defined? arg)
     {:antivalue? false}
     {:antivalue? true
      :sym (gensym)})))

(defn get-expr [arg]
  ((if (contains? arg :sym) :sym :expr) arg))

(defn first-antivalue [args]
  `(or ~@(map (fn [arg] 
                `(antivalue-or-nil ~(:sym arg))) 
              (filter :antivalue? args))))

(defn make-farg-binding [prepared]
  (if (contains? prepared :sym)
    [(:sym prepared) (:expr prepared)]
    []))

(defn make-farg-bindings [prepared]
  (reduce into [] (map make-farg-binding prepared)))

(defn wrap-arg-binding [state prepared cb]
  (let [bindings (make-farg-bindings prepared)
        subexpr (cb (map get-expr prepared))]
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

(defn compile-fun-call-sub [state args0 cb]
  (let [prepared (prepare-args state args0)]
    ((if (prepared-has-undefined? prepared)
       undefined defined)
     (wrap-arg-binding
      state prepared
      cb))))

(defn compile-fun-call [state f args0]
  (compile-fun-call-sub 
   state args0
   (fn [args]
     `(~f ~@args))))

(defn compile-coll [state c args]
  (compile-fun-call-sub
   state args
   (fn [args]
     (into c args))))

(defn compile-vector [state args]
  (compile-coll state [] args))

(defn compile-set [state args]
  (compile-coll state #{} args))

(defn make-pair [state b]
  (if (map? state)
    (conj (:acc state) [(:last state) b])
    {:acc state :last b}))
  

(defn make-pairs [x]
  (reduce make-pair [] x))
   

(defn compile-map [state args]
  (compile-fun-call-sub
   state (apply concat args)
   (fn [args]
     (into {} (make-pairs args)))))


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
      (tag/tag 
       (tag/get-tag body)
       `(let ~(vec bindings)
          ~(tag/value body))))))

(defn compile-do-sub [state args]
  (let [[f & r] args]
    (if (empty? r)
      (tag/value f)
      (if (defined? f)
        `(do ~(tag/value f) ~(compile-do-sub state r))
        `(do (let [y# ~(tag/value f)]
               (if (antivalue? y#)
                 y#
                 ~(compile-do-sub state r))))))))
      

(defn compile-do [state args]
  (if (empty? args) 
    nil
    (let [c (compile-args state args)]
      ((if (some undefined? c)
         undefined defined)
       (compile-do-sub state c)))))

(defn compile-catch [state parsed-catch]
  (let [body (compile-sub 
              (remove-undefined state (:var-name parsed-catch))
              `(do ~@(:forms parsed-catch)))]
    (tag/tag-like 
     body
     (merge
      parsed-catch 
      {:forms [(tag/value body)]}))))
    
(defn compile-finally [state frm]
  (when frm
    (let [sub (compile-sub state `(do ~@(:forms frm)))]
      (tag/tag-like
       sub
       (merge 
        frm
        {:forms [(tag/value sub)]})))))

(defn compile-try [state x]
  (let [parsed (spec/conform ::macro/try-form x)]
    (if (= parsed ::spec/invalid)
      (macro/error (spec/explain ::macro/try-form x))
      (let [forms [(compile-sub state `(do ~@(:forms parsed)))]
            catch-forms (map #(compile-catch state %) (:catch-forms parsed))
            finally-form (compile-finally state (:finally-form parsed))
            is-undefined (or (some undefined? forms)
                             (some undefined? catch-forms)
                             (if finally-form
                               (undefined? finally-form)
                               false))]
        ((if is-undefined
           undefined
           defined)
         (spec/unform
          ::macro/try-form
          (reduce
           merge
           [parsed
            {:forms (map tag/value forms)
             :catch-forms (map tag/value catch-forms)}
            (if finally-form
              {:finally-form (tag/value finally-form)})])))))))
        

;; 'if :if ; OK
;; 'do :do ;; OK
;; 'let* :let ;; OK
;; 'loop* :loop ;; OK -don't compile
;; 'recur :recur ;; OK -don't compile
;; 'throw :throw ;; Treat as a funcall
;; 'def :def ;; Like a funcall
;; 'var :var ;; Like a funcall
;; 'monitor-enter ;; Like a funcall
;; 'monitor-exit ;; Like a funcall
;; 'fn* :fn ;; OK, don't descend there.
;; 'try :try ;; OK
;; 'catch :catch ;; OK Handled by try
;; 'quote :quote ;; OK

(defn compile-seq-sub [state x]
  (let [[f & args] x
        sf (get macro/special-forms f)]
    (cond
      (= :quote sf) (defined x)

      ;; Don't compile these forms (yet):
      (contains? #{:fn :loop :recur} sf) 
      (do
        (macro/warning "Not handling antivalues for " x)
        (defined x))

      (= :if sf) (compile-if state x)
      (= :let sf) (compile-let state x)
      (= :do sf) (compile-do state args)
      (= :try sf) (compile-try state x)
      :default (compile-fun-call state f args))))

(defn compile-import [state args]
  (assert (= 1 (count args)))
  (undefined (tag/value (compile-sub state (first args)))))

(defn compile-seq [state x]
  (let [[f & args] x]
    (cond
      (macro/compare-symbols `import f) (compile-import state args)
      (macro/compare-symbols `anti f) (compile-anti state args)
      (macro/compare-symbols `either f) (compile-either state args)
      :default (compile-seq-sub state (macroexpand x)))))

(defn compile-sub 
  ([state x]
   (cond
     (seq? x) (compile-seq state x)
     (map? x) (compile-map state x)
     (set? x) (compile-set state x)
     (vector? x) (compile-vector state x)
     :default (compile-primitive state x)))
  ([state]
   (fn [x] (compile-sub state x))))

(defmacro export [x]
  (tag/value (compile-sub init-state x)))

(defmacro either [& args]
  (let [c (compile-either init-state args)]
    (assert (defined? c))
    (tag/value c)))

(defmacro top [& forms]
  (let [c (compile-sub init-state `(do ~@forms))]
    (if (defined? c)
      (tag/value c)
      (macro/error "No antivalues are allowed to reach the top"))))

(defmacro expect [f x]
  `(let [x# ~x]
     (if (~f x#) x# (anti x#))))

;; (macroexpand '(export (+ (anti 3))))
