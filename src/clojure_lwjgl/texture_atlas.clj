(ns clojure-lwjgl.texture-atlas
  (:require [clojure-lwjgl.texture :as texture]
            [clojure-lwjgl.buffer :as buffer]
            [clojure-lwjgl.buffered-image :as buffered-image]))

(defrecord TextureAtlas [texture-coordinate-buffer
                         texture-coordinate-buffer-id
                         texture])

(defn create []
  (let [initial-number-of-textures 10
        initial-base-texture-size 500]
    (TextureAtlas. (buffer/create-float-buffer (* initial-number-of-textures 4 2))
                   (buffer/create-gl-buffer)
                   (texture/create initial-base-texture-size initial-base-texture-size))))

(defn- maximum-y [coordinate-buffer]
  (apply max (map second (partition 2 (buffer/float-buffer-to-array coordinate-buffer)))))

(defn new-texture-coordinates [texture-atlas index width height]
  (let [max-x (:width (:texture texture-atlas))
        max-y (:height (:texture texture-atlas))
        y1 (maximum-y (:texture-coordinate-buffer texture-atlas))]
    {:x1 0
     :y1 y1
     :x2 (/ width
            max-x)
     :y2 (+ y1
            (/ height
               max-y))}))

(defn texture-index-to-texture-coordinate-index [texture-index] (* texture-index 4 2))

(defn allocate-texture [texture-atlas index width height]
  (let [{:keys [x1 y1 x2 y2]} (new-texture-coordinates texture-atlas
                                                       index
                                                       width
                                                       height)]
    (buffer/update-buffer (:texture-coordinate-buffer texture-atlas)
                          (texture-index-to-texture-coordinate-index index)
                          (float-array [x1 y1
                                        x2 y1
                                        x2 y2
                                        x1 y2]))

    (buffer/load-buffer (:texture-coordinate-buffer-id texture-atlas)
                        (:texture-coordinate-buffer texture-atlas))

    (buffered-image/create-child (:buffered-image (:texture texture-atlas))
                                 (* x1 (:width (:texture texture-atlas)))
                                 (* y1 (:height (:texture texture-atlas)))
                                 width
                                 height)))

(defn load [texture-atlas]
  (texture/load (:texture texture-atlas)))

(defn dispose [texture-atlas]
  (texture/delete (:texture texture-atlas))
  (buffer/delete (:texture-coordinate-buffer)))

