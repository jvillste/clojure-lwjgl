(ns clojure-lwjgl.texture-atlas
  (:refer-clojure :exclude (load))
  (:require [clojure-lwjgl.texture :as texture]
            [clojure-lwjgl.buffer :as buffer]
            [clojure-lwjgl.buffered-image :as buffered-image]))

(defrecord TextureAtlas [texture-count
                         texture-coordinate-buffer
                         texture-coordinate-buffer-id
                         texture])

(defn texture-index-to-texture-coordinate-index [texture-index]
  (* texture-index
     4
     2))

(defn create []
  (let [initial-number-of-textures 10
        initial-base-texture-size 500]
    (TextureAtlas. 0
                   (buffer/create-float-buffer (texture-index-to-texture-coordinate-index initial-number-of-textures))
                   (buffer/create-gl-buffer)
                   (texture/create initial-base-texture-size initial-base-texture-size))))

(defn- maximum-y [coordinate-buffer]
  (apply max (map second (partition 2 (buffer/float-buffer-to-array coordinate-buffer)))))

(defn texture-x-to-texel-x [texture-atlas texture-x]
  (* texture-x
     (:width (:texture texture-atlas))))

(defn texture-y-to-texel-y [texture-atlas texture-y]
  (* texture-y
     (:height (:texture texture-atlas))))

(defn coordinate [texture-atlas texture-index coordinate-index]
  (.get (:texture-coordinate-buffer texture-atlas)
        (+ (texture-index-to-texture-coordinate-index texture-index)
           coordinate-index)))

(defn x1 [texture-atlas index]
  (coordinate texture-atlas index 0))

(defn y1 [texture-atlas index]
  (coordinate texture-atlas index 1))

(defn x2 [texture-atlas index]
  (coordinate texture-atlas index 4))

(defn y2 [texture-atlas index]
  (coordinate texture-atlas index 5))

(defn width [texture-atlas index]
  (- (x2 texture-atlas index)
     (x1 texture-atlas index)))

(defn height [texture-atlas index]
  (- (y2 texture-atlas index)
     (y1 texture-atlas index)))

(defn get-graphics [texture-atlas index]
  (buffered-image/get-graphics (buffered-image/create-child (:buffered-image (:texture texture-atlas))
                                                            (texture-x-to-texel-x texture-atlas (x1 texture-atlas index))
                                                            (texture-y-to-texel-y texture-atlas (y1 texture-atlas index))
                                                            (texture-x-to-texel-x texture-atlas (width texture-atlas index))
                                                            (texture-y-to-texel-y texture-atlas (height texture-atlas index)))))

(defn new-texture-coordinates [texture-atlas width height]
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

(defn allocate-texture [texture-atlas width height]
  (let [{:keys [x1 y1 x2 y2]} (new-texture-coordinates texture-atlas
                                                       width
                                                       height)
        index (:texture-count texture-atlas)]

    (buffer/update-buffer (:texture-coordinate-buffer texture-atlas)
                          (texture-index-to-texture-coordinate-index index)
                          (float-array [x1 y1
                                        x2 y1
                                        x2 y2
                                        x1 y2]))

    (assoc texture-atlas :texture-count (+ 1
                                           (:texture-count texture-atlas)))))

(defn load [texture-atlas]
  (texture/load (:texture texture-atlas))
  (buffer/load-buffer (:texture-coordinate-buffer-id texture-atlas)
                      (:texture-coordinate-buffer texture-atlas)))

(defn delete [texture-atlas]
  (texture/delete (:texture texture-atlas))
  (buffer/delete (:texture-coordinate-buffer-id texture-atlas)))
