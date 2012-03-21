(ns clojure-lwjgl.texture-list
  (:refer-clojure :exclude (load))
  (:require [clojure-lwjgl.texture :as texture]
            [clojure-lwjgl.buffer :as buffer]
            [clojure-lwjgl.buffered-image :as buffered-image]
            [clojure-lwjgl.texture-coordinate-buffer :as texture-coordinate-buffer]))

(defrecord TextureAtlas [texture-count
                         texture-coordinate-buffer
                         textures])

(defn create []
  (let [initial-number-of-textures 10
        initial-base-texture-size 500]
    (TextureAtlas. 0
                   (texture-coordinate-buffer/create)
                   {})))

(defn get-graphics [texture-list index]
  (buffered-image/get-graphics (:buffered-image ((:textures texture-list) index))))


(defn allocate-texture [texture-list width height]
  (let [{:keys [x1 y1 x2 y2]} (texture-coordinates width
                                                   height)
        index (:texture-count texture-list)]

    (-> texture-list
        (update-texture-coordinate-buffer index 0.0 0.0 1.0 1.0)
        (assoc :texture-count (+ 1
                                 (:texture-count texture-list))
               :needs-to-load true))))
