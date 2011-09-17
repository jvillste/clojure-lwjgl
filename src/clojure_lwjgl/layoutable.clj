(ns clojure-lwjgl.layoutable)

(defprotocol Layoutable
  (preferred-width [component])
  (preferred-height [component]))

