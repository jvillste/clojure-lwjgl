(ns clojure-lwjgl.component
  (:use [clojure-lwjgl.texture :as texture])
  (:use [clojure-lwjgl.buffer :as buffer])
  (:use [clojure-lwjgl.texture-atlas :as texture-atlas]))

(defrecord ComponentContainer [components
                               vertex-buffer
                               vertex-buffer-id
                               index-buffer
                               index-buffer-id
                               texture-atlas])

(defn create-component-container []
  (let [maximum-number-of-components 2]
    (ComponentContainer. (list)
                         (buffer/create-float-buffer (* maximum-number-of-components 2 4))
                         (buffer/create-gl-buffer)
                         (buffer/create-int-buffer (* maximum-number-of-components 4))
                         (buffer/create-gl-buffer)
                         (texture-atlas/create-texture-atlas))))

(defn next-component-index [component-container]
  (/ (.limit (:vertex-buffer component-container))
     (* 2 4)))

(defn component-index-to-vertex-buffer-index [index] (* index 2 4))
(defn component-index-to-index-buffer-index [index] (* index 4))

(defn add-component [component-container component x1 y1]
  (let [component-index (next-component-index component-container)
        x2 (+ x (get-width component))
        y2 (+ y (get-height component))
        first-index-buffer-index (component-index-to-index-buffer-index component-index)]

    (buffer/update-buffer (:vertex-buffer component-container)
                          (component-index-to-vertex-buffer-index component-index)
                          [x1 y1
                           x2 y1
                           x2 y2
                           x1 y2])

    (buffer/update-buffer (:index-buffer component-container)
                          first-index-buffer-index 
                          [first-index-buffer-index
                           (+ first-index-buffer-index 1)
                           (+ first-index-buffer-index 2)
                           (+ first-index-buffer-index 3)])
    
    (assoc component-container :components
           (conj (:components component-container)
                 (assoc component :texture (texture-atlas/allocate-texture (:texture-atlas component-container)
                                                                           component-index
                                                                           (get-width component)
                                                                           (get-height component)))))))

(defprotocol Component
  (render [component graphics])
  (get-width [component])
  (get-height [component])
  (get-x [component])
  (get-y [component])
  (dispose [component]))
