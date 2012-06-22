(ns clojure-lwjgl.triangle)

(defn triangle [coordinates color]
  {:coordinates (apply float coordinates)
   :colors (apply concat (repeat 3 (concat color [(float 1.0)])))})