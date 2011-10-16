(ns clojure-lwjgl.free-layout
  (:require [clojure-lwjgl.layoutable :as layoutable]
            [clojure-lwjgl.component :as component]))

(defrecord FreeLayout [children])

(defn layout [component]
  (assoc component
    :width (layoutable/preferred-width component)
    :height (layoutable/preferred-height component)
    :x (::x component)
    :y (::y component)))

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