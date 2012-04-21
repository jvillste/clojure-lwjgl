(ns clojure-lwjgl.applications.application
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



(defn ids-to-visuals [gui ids]
  (reduce (fn [visuals visual-id] (conj visuals (assoc (visual-list/get-visual (:visual-list gui)
                                                                               visual-id)
                                                  :id visual-id)))
          []
          ids))

(defn update-visuals [gui visuals]
  (assoc gui
    :visual-list (reduce (fn [visual-list visual]
                           (visual-list/update-visual visual-list
                                                      (:id visual)
                                                      visual))
                         (:visual-list gui)
                         visuals)))

(defn apply-to-visual [gui function visual-id]
  (assoc gui :visual-list (visual-list/apply-to-visual (:visual-list gui)
                                                       visual-id
                                                       function)))

(defmacro thread-it [& [first-expr & rest-expr]]
  (if (empty? rest-expr)
    first-expr
    `(let [~'it ~first-expr]
       (thread-it ~@rest-expr))))

(defn apply-to-visuals [gui function visual-ids]
  (thread-it (ids-to-visuals gui visual-ids)
             (function it)
             (update-visuals gui it)))
