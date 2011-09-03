(ns clojure-lwjgl.component)

(defprotocol Component
  (render [component])
  (dispose [component]))
