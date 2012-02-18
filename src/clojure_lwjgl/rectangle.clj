(ns clojure-lwjgl.rectangle
  (:import [java.awt Color])
  (:require (clojure-lwjgl [visual :as visual]
                           [layoutable :as layoutable])))

(defrecord Rectangle [color])


(defn create [color]
  (Rectangle. color))

(defn color-to-java-color [color]
  (Color. (:red color)
          (:green color)
          (:blue color)
          (:alpha color)))

(defn render [rectangle graphics]
  (doto graphics
    (.setColor (color-to-java-color (:color rectangle)))
    (.drawRect 0 0 (:width rectangle) (:height rectangle))))

(extend Rectangle
  visual/Visual
  {:render render})

(extend Rectangle
  layoutable/Layoutable
  {:preferred-width #(:width %) 
   :preferred-height #(:height %) })
