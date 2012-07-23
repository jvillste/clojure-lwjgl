(ns clojure-lwjgl.primitive-list)

(defprotocol PrimitiveList
  (create [primitives])
  (update [primitive-list primitives])
  (draw [primitive-list])
  (delete [primitive-list]))