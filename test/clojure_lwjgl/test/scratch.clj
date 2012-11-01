(ns clojure-lwjgl.test.scratch)

(defprotocol Foo
  (do-it [foo]))

(defrecord Bar [x]
    Foo
    (do-it [bar] x))

(println (do-it (assoc (->Bar 3) :x 9 )))

(println (loop [x [1 2 3]
                result []]
           (if (first x)
             (recur (rest x)
                    (conj result (+ 1 (first x))))
             result)))
