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
  (let [maximum-number-of-components 2
        texture-size 128]
    (ComponentContainer. (list)
                         (buffer/create-float-buffer (* maximum-number-of-components 2 4))
                         (buffer/create-gl-buffer)
                         (buffer/create-float-buffer (* maximum-number-of-components 2 4))
                         (buffer/create-gl-buffer)
                         (buffer/create-int-buffer (* maximum-number-of-components 4))
                         (buffer/create-gl-buffer)
                         (texture/create-texture texture-size))))

(defn maximum-allocated-y [texture-coordinate-buffer])

(defn update-buffer [buffer start-index values])

(defn new-texture-coordinates [component-container index width height]
  (let [max-x (:width (:texture component-container))
        max-y (:height (:texture component-container))
        y1 (/ (maximum-allocated-y (:texture-coordinate-buffer component-container))
              max-y)]
    {:x1 0
     :y1 y1
     :x2 (/ width
            max-x)
     :y2 (+ y1
            (/ height
               max-y))}))

(defn allocate-texture [component-container index width height]
  (let [{:keys [x1 y1 x2 y2]} (new-texture-coordinates component-container
                                                       width
                                                       height)]

    (update-buffer (:texture-coordinate-buffer component-container) index [x1 y1
                                                                           x2 y1
                                                                           x2 y2
                                                                           x1 y2])
    (texture/create-child-texture (:texture component-container)
                                  x1
                                  y1
                                  width
                                  height)))

(defn add-component [component-container component]
  (assoc component-container :components
         (conj (:components component-container)
               (-> component
                   (assoc :texture (allocate-texture))))))

(defprotocol Component
  (render [component graphics])
  (get-width [component])
  (get-height [component])
  (get-x [component])
  (get-y [component])
  (dispose [component]))
