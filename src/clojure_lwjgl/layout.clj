(ns clojure-lwjgl.layout
  (:require (clojure-lwjgl [window :as window]
                           [visual :as visual]
                           [image-list :as image-list]
                           [visual-list :as visual-list]
                           [input :as input]
                           [group :as group]
                           [text :as text]
                           [free-layout :as free-layout]
                           [layoutable :as layoutable]
                           [rectangle :as rectangle])
            [clojure.zip :as zip]
            [clojure.contrib.dataflow :as dataflow])

  (:use midje.sweet)
  (:import [org.lwjgl.opengl GL11]))

(defn vertical-stack2 [x0 y0 visuals]
  (when (seq visuals)
    (let [visual (first visuals)]
      (println x0 " " y0)
      (recur x0
             (+ y0 (layoutable/preferred-height visual))
             (rest visuals)))))

(defn vertical-stack [x0 y0 visuals]
  (loop [visuals visuals
         layouted-visuals []
         y y0]
    (if (seq visuals)
      (let [visual (assoc (first visuals)
                     :y y
                     :x x0)]
        (recur (rest visuals)
               (conj layouted-visuals
                     visual)
               (+ y (layoutable/preferred-height visual))))
      layouted-visuals)))

(defrecord TestLayoutable [height]
  layoutable/Layoutable
  (layoutable/preferred-height [test-layoutable] (:height test-layoutable)))

(fact "vertical stack sets x and y coordinates"
  (vertical-stack 10
                  20
                  [(TestLayoutable. 10)
                   (TestLayoutable. 15)
                   (TestLayoutable. 10)])
  => [#clojure_lwjgl.layout.TestLayoutable{:height 10, :x 10, :y 20}
      #clojure_lwjgl.layout.TestLayoutable{:height 15, :x 10, :y 30}
      #clojure_lwjgl.layout.TestLayoutable{:height 10, :x 10, :y 45}])

(defn set-preferred-size [layoutable]
  (assoc layoutable
    :width (layoutable/preferred-width layoutable)
    :height (layoutable/preferred-height layoutable)))

(defn absolute-layout [layoutable x y]
  (-> layoutable
      set-preferred-size
      (assoc :x x
             :y y)))
