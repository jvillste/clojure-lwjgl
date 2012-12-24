(ns flow-gl.graphics.vector
  (:require (clojure-lwjgl.command [triangle-batch :as triangle-batch])))

(defn rectangle [x y width height color]
  (triangle-batch/create [x y
                          x (+ y height)
                          (+ x width) y

                          x (+ y height)
                          (+ x width) (+ y height)
                          (+ x width) y]

                         (apply concat (repeat 6 color))))

