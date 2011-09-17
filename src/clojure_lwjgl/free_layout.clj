(ns clojure-lwjgl.free-layout
  (:require [clojure-lwjgl.layoutable :as layoutable]))

(defn free-layout [component x y]
  (-> component
      (assoc :width (layoutable/preferred-width component))
      (assoc :height (layoutable/preferred-height component))
      (assoc :x x)
      (assoc :y y)))