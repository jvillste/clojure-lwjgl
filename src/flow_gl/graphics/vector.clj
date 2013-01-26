(ns flow-gl.graphics.vector
  (:require (flow-gl.graphics.command [triangle-batch :as triangle-batch])))

(defn rectangle [x y width height color]
  (triangle-batch/create [x y
                          x (+ y height)
                          (+ x width) y

                          x (+ y height)
                          (+ x width) (+ y height)
                          (+ x width) y]

                         (apply concat (repeat 6 color))))



(defn triangle [color x1 y1 x2 y2 x3 y3]
  (triangle-batch/create [x1 y1 x2 y2 x3 y3]
                         (apply concat (repeat 3 color))))
