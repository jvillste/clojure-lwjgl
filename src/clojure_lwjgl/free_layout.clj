(ns clojure-lwjgl.free-layout
  (:require [clojure-lwjgl.layoutable :as layoutable]
            [clojure-lwjgl.component :as component]))


(defn layout [x y layoutable]
  (assoc layoutable
    :width (layoutable/preferred-width layoutable)
    :height (layoutable/preferred-height layoutable)
    :x x
    :y y))





(defrecord FreeLayout [children])

(defn set-location [component x y]
  (assoc component
    ::x x
    ::y y))

(defn create [children]
  (FreeLayout. children))

(defn get-visual [free-layout])

(extend FreeLayout
  component/Component
  {:get-visual get-visual})