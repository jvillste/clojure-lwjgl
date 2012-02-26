(ns clojure-lwjgl.rectangle
  (:import [java.awt Color RenderingHints]
           [java.awt.geom RoundRectangle2D$Float])
  (:require (clojure-lwjgl [visual :as visual]
                           [layoutable :as layoutable])))

(defrecord Rectangle [color width height arc])

(defn create [color width height arc]
  (Rectangle. color width height arc))

(defn color-to-java-color [color]
  (Color. (float (:red color))
          (float (:green color))
          (float (:blue color))
          (float (:alpha color))))

(defn render [rectangle graphics]
  (println "render rectangle " rectangle)
  (doto graphics
    (.setColor (color-to-java-color (:color rectangle)))
;;    (.setColor Color/BLUE)
    (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON )
    (.fill (RoundRectangle2D$Float. 0 0 (:width rectangle) (:height rectangle) (:arc rectangle) (:arc rectangle)))))

(extend Rectangle
  visual/Visual
  {:render render})

(extend Rectangle
  layoutable/Layoutable
  {:preferred-width #(:width %)
   :preferred-height #(:height %)})
