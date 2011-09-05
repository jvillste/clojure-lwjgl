(ns clojure-lwjgl.component
  (:use [clojure-lwjgl.texture :as texture])
  (:use [clojure-lwjgl.buffer :as buffer]))

(defrecord ComponentContainer [components
                               texture-coordinate-buffer
                               texture-coordinate-buffer-id
                               vertex-buffer
                               vertex-buffer-id
                               index-buffer
                               index-buffer-id
                               texture])

(defn create-component-container []
  (let [components (atom '())
        maximum-number-of-components 2
        texture-size 128

        texture-coordinate-buffer (buffer/create-float-buffer (* maximum-number-of-components 2 4))
        texture-coordinate-buffer-id (buffer/create-gl-buffer)
        vertex-buffer (buffer/create-float-buffer (* maximum-number-of-components 2 4))
        vertex-buffer-id (buffer/create-gl-buffer)
        index-buffer (buffer/create-int-buffer (* maximum-number-of-components 4))
        index-buffer-id (buffer/create-gl-buffer)
        texture (texture/create-texture texture-size)]

    (ComponentContainer. components
                         texture-coordinate-buffer
                         texture-coordinate-buffer-id
                         vertex-buffer
                         vertex-buffer-id
                         index-buffer
                         index-buffer-id
                         texture )))

(defn add-component [component-container component]
  (reset! (:components component-container)
          (conj @(:components component-container) component)))

(defprotocol Component
  (render [component graphics])
  (get-width [component])
  (get-height [component])
  (dispose [component]))
