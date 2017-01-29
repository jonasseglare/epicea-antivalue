(ns epicea.antivalue.AntivalueException
  (:gen-class 
   :extends java.lang.Exception
   :state state
   :init init
   :constructors {[Object] []}))

(defn -init [x]
  [[] x])
