(ns clojure-lwjgl.test.scratch)

(defprotocol Foo
  (do-it [foo]))

(defrecord Bar [x]
  Foo
  (do-it [bar] x))

(def ^:dynamic dynamic-var)

(println (binding [dynamic-var 1]
           (->Bar dynamic-var)))


