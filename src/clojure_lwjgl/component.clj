(ns clojure-lwjgl.component)

(defprotocol Component
  (render [component graphics])
  (preferred-width [component])
  (preferred-height [component])
  (dispose [component]))

(defn free-layout [component x y]
  (-> component
      (assoc :width (preferred-width component))
      (assoc :height (preferred-height component))
      (assoc :x x)
      (assoc :y y)))