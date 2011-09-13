(ns clojure-lwjgl.component-container
  (:require [clojure-lwjgl.texture :as texture]
            [clojure-lwjgl.buffer :as buffer]
            [clojure-lwjgl.buffered-image :as buffered-image]
            [clojure-lwjgl.component :as component]
            [clojure-lwjgl.texture-atlas :as texture-atlas]))

(defn- component-count [component-container]
  (count (:components component-container)))

(defn- component-index-to-vertex-buffer-index [index] (* index 3 4))
(defn- component-index-to-index-buffer-index [index] (* index 4))

(defrecord ComponentContainer [components
                               vertex-buffer
                               vertex-buffer-id
                               index-buffer
                               index-buffer-id
                               texture-atlas])

(defn create []
  (let [maximum-number-of-components 10]
    (ComponentContainer. (list)
                         (buffer/create-float-buffer (* maximum-number-of-components 3 4))
                         (buffer/create-gl-buffer)
                         (buffer/create-int-buffer (* maximum-number-of-components 4))
                         (buffer/create-gl-buffer)
                         (texture-atlas/create))))

(defn move-component
  ([component-container component component-index]
     (let [x1 (:x component)
           y1 (:y component)
           x2 (+ x1 (:width component))
           y2 (+ y1 (:height component))]

       (buffer/update-buffer (:vertex-buffer component-container)
                             (component-index-to-vertex-buffer-index component-index)
                             (float-array [x1 y2 0.0
                                           x2 y2 0.0
                                           x2 y1 0.0
                                           x1 y1 0.0]))

       (buffer/load-buffer (:vertex-buffer-id component-container)
                           (:vertex-buffer component-container))))


  ([component-container component]
     (move-component component-container component (::index component))))


(defn add-component [component-container component]
  (let [component-index (component-count component-container)
        first-index-buffer-index (component-index-to-index-buffer-index component-index)]

    (move-component component-container component component-index)

    (buffer/update-buffer (:index-buffer component-container)
                          first-index-buffer-index
                          (int-array [first-index-buffer-index
                                        (+ first-index-buffer-index 1)
                                        (+ first-index-buffer-index 2)
                                        (+ first-index-buffer-index 3)]))

    (buffer/load-element-buffer (:index-buffer-id component-container)
                                (:index-buffer component-container))

    (assoc component-container :components
           (conj (:components component-container)
                 (-> component
                     (assoc :buffered-image (texture-atlas/allocate-texture (:texture-atlas component-container)
                                                                            component-index
                                                                            (:width component)
                                                                            (:height component)))
                     (assoc ::index component-index))))))

(defn render-components [component-container]
  (doseq [component (:components component-container)]
    (component/render component (buffered-image/get-graphics (:buffered-image component))))

  (texture-atlas/load (:texture-atlas component-container)))

(defn draw [component-container]
  (buffer/draw-quads (:vertex-buffer-id component-container)
                     (:texture-coordinate-buffer-id (:texture-atlas component-container))
                     (:index-buffer-id component-container)
                     (component-count component-container)))

(defn dispose [component-container]
  (texture-atlas/dispose (:texture-atlas component-container))
  (buffer/delete (:vertex-buffer-id component-container))
  (buffer/delete (:index-buffer-id component-container)))