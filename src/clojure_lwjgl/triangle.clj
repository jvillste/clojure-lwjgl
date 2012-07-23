(ns clojure-lwjgl.triangle)

(defn single-color-triangle [coordinates color]
  {:coordinates (map float (apply concat coordinates))
   :colors (apply concat (repeat 3 (concat color [(float 1.0)])))})

(defn multi-color-triangle [coordinates colors]
  {:coordinates (map float (apply concat coordinates))
   :colors (map float (apply concat colors))})

