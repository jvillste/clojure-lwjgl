(ns clojure-lwjgl.vector-rectangle
  (:require (clojure-lwjgl [triangle-batch :as triangle-batch]))
  (:use clojure.test))

(defn rectangle [x y width height color]
  (triangle-batch/create [x y
                          x (+ y height)
                          (+ x width) y

                          x (+ y height)
                          (+ x width) (+ y height)
                          (+ x width) y]

                         (apply concat (repeat 6 color))))

(deftest rectangle-test
  (is (= (rectangle 10 10 20 20 [0.0 0.0 1.0 1.0])
         '#clojure_lwjgl.triangle_batch.TriangleBatch{:coordinates [10 10 10 30 30 10 10 30 30 30 30 10],
                                                      :colors (0.0 0.0 1.0 1.0 0.0 0.0 1.0 1.0 0.0 0.0 1.0 1.0 0.0 0.0 1.0 1.0 0.0 0.0 1.0 1.0 0.0 0.0 1.0 1.0)})))

(run-tests)