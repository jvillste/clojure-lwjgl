(ns clojure-lwjgl.canvas)

(defrecord Canvas [children])

(defn render [canvas graphics]
  (doseq [child (:children canvas)]
    (visual/render child graphics)))