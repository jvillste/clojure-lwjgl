(ns clojure-lwjgl.box
  (:require (clojure-lwjgl [layoutable :as layoutable]))
  (:use midje.sweet))

(defn box [margin parent child]
  [(assoc parent
     :width (+ (* 2 margin)
               (layoutable/preferred-width child))
     :height (+ (* 2 margin)
                (layoutable/preferred-height child)))
   (assoc child
     :x (+ margin
           (:x parent))
     :y (+ margin
           (:y parent)))])

(defrecord TestLayoutable [width height]
  layoutable/Layoutable
  (layoutable/preferred-height [test-layoutable] (:height test-layoutable))
  (layoutable/preferred-width [test-layoutable] (:width test-layoutable)))

(fact "box adds a margin"
  (box 100
       {:x 5 :y 10}
       (TestLayoutable. 15 10))
  => [{:height 210, :width 215, :y 10, :x 5}
      #clojure_lwjgl.box.TestLayoutable{:width 10, :height 10, :y 110, :x 105}])
