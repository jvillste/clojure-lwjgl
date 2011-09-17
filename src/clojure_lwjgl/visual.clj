(ns clojure-lwjgl.visual)

(defprotocol Visual
  (render [visual graphics]))
