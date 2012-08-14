(ns clojure-lwjgl.translate)

(defrecord Translate [x y])

(defn combine [translate1 translate2]
  (->Translate (+ (:x translate1)
                  (:x translate2))
               (+ (:y translate1)
                  (:y translate2))))

