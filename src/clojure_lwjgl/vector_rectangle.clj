(ns clojure-lwjgl.vector-rectangle
  (:require (clojure-lwjgl [triangle :as triangle]))
  (:use clojure.test))


(defn rectangle [x y width height color]
  [(triangle/single-color-triangle [[x y]
                           [x (+ y height)]
                           [(+ x width) y]]
                          color)

   (triangle/single-color-triangle [[x (+ y height)]
                           [(+ x width) (+ y height)]
                           [(+ x width) y]]
                          color)])

(deftest rectangle-test
  (is (= (rectangle 10 10 20 20 [0.0 0.0 1.0 1.0])
         '[{:coordinates (10.0 10.0 10.0 30.0 30.0 10.0), :colors (0.0 0.0 1.0 1.0 0.0 0.0 1.0 1.0 0.0 0.0 1.0 1.0)}
           {:coordinates (10.0 30.0 30.0 30.0 30.0 10.0), :colors (0.0 0.0 1.0 1.0 0.0 0.0 1.0 1.0 0.0 0.0 1.0 1.0)}])))
