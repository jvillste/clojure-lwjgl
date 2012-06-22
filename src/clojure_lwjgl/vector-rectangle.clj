(ns clojure-lwjgl.vector-rectangle
  (:require (clojure-lwjgl [triangle-list :as triangle-list])))

(defn vector-rectangle [x y width height color]
  [(triangle [x y
              (+ x width) y
              x (+ y + height)]
             color)])