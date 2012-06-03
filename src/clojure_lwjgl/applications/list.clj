(ns clojure-lwjgl.applications.list
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

(defn create [visual-list]
  (let [[visual-list selection-rectangle] (visual-list/add visual-list (rectangle/create {:red 0.5 :green 0.5 :blue 0.5 :alpha 1}
                                                                                          100
                                                                                          15
                                                                                          10))]
    [visual-list
     {:children []
      :selection 0
      :selection-rectangle selection-rectangle}]))


(defn handle-event [list event]
  (cond
   (key-pressed event input/down)
   (assoc list
     :selection (min (+ 1
                        (:selection list))
                     (- (count (:children gui))
                        1)))

   (key-pressed event input/up)
   (assoc gui
     :selection (max (- (:selection list)
                        1)
                     0))
   :default
   list))

(defn copy-position-and-size [source target]
  (merge source
         (select-keys target
                      [:x :y :width :height])))

(defn layout [list visual-list]
  (-> visual-list
      (visual-list/apply-to-visuals #(layout/vertical-stack 5 5 %)
                                    (:children list))
      (visual-list/apply-to-visual (:selection-rectangle list)
                                   #(copy-position-and-size (visual-list/get-visual visual-list
                                                                                    (nth (:children list)
                                                                                         (:selection list)))
                                                            %))))