(ns clojure-lwjgl.texture-atlas
  (:require [clojure-lwjgl.texture :as texture]
            [clojure-lwjgl.buffer :as buffer]))

(defrecord TextureAtlas [texture-coordinate-buffer
                         texture-coordinate-buffer-id
                         texture])

(defn create-texture-atlas []
  (let [initial-number-of-textures 2
        initial-base-texture-size 128]
    (TextureAtlas. (buffer/create-float-buffer (* initial-number-of-textures 4 2))
                   (buffer/create-gl-buffer)
                   (texture/create initial-base-texture-size))))

(defn- maximum-y [coordinate-buffer]
  (apply max (map second (partition 2 (buffer/float-buffer-to-array coordinate-buffer)))))

(defn new-texture-coordinates [texture-atlas index width height]
  (let [max-x (:width (:texture texture-atlas))
        max-y (:height (:texture texture-atlas))
        y1 (/ (maximum-y (:texture-coordinate-buffer texture-atlas))
              max-y)]
    {:x1 0
     :y1 y1
     :x2 (/ width
            max-x)
     :y2 (+ y1
            (/ height
               max-y))}))

(defn allocate-texture [texture-atlas index width height]
  (let [{:keys [x1 y1 x2 y2]} (new-texture-coordinates texture-atlas
                                                       index
                                                       width
                                                       height)]

    (buffer/update-buffer (:texture-coordinate-buffer texture-atlas)
                          index
                          [x1 y1
                           x2 y1
                           x2 y2
                           x1 y2])

    (texture/create-child (:texture texture-atlas)
                          x1
                          y1
                          width
                          height)))

(defn dispose-texture [texture-atlas index])