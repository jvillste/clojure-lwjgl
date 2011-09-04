(ns clojure-lwjgl.component
  (:use [clojure-lwjgl.texture :as texture])
  (:use [clojure-lwjgl.buffer :as buffer]))

(defrecord ComponentContainer [components
                               texture-coordinate-buffer
                               texture-coordinate-buffer-id
                               vertex-buffer
                               vertex-buffer-id
                               texture])

(defn create-component-container []
  (let [components (atom '())
        
        ]))

(defn add-component [component-container component]
  (reset! (:components component-container)
          (conj @(:components component-container) component)))

(defprotocol Component
  (render [component graphics])
  (get-width [component])
  (get-height [component])
  (dispose [component]))
