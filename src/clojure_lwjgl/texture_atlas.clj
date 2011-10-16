(ns clojure-lwjgl.texture-atlas
  (:refer-clojure :exclude (load))
  (:require [clojure-lwjgl.texture :as texture]
            [clojure-lwjgl.buffer :as buffer]
            [clojure-lwjgl.buffered-image :as buffered-image]
            [clojure-lwjgl.texture-coordinate-buffer :as texture-coordinate-buffer]))

(defrecord TextureAtlas [texture-count
                         texture-coordinate-buffer
                         texture])

(defn create []
  (let [initial-number-of-textures 10
        initial-base-texture-size 500]
    (TextureAtlas. 0
                   (texture-coordinate-buffer/create)
                   (texture/create initial-base-texture-size initial-base-texture-size))))

(defn texture-x-to-texel-x [texture-atlas texture-x]
  (* texture-x
     (:width (:texture texture-atlas))))

(defn texture-y-to-texel-y [texture-atlas texture-y]
  (* texture-y
     (:height (:texture texture-atlas))))

(defn get-graphics [texture-atlas index]
  (buffered-image/get-graphics (buffered-image/create-child (:buffered-image (:texture texture-atlas))

                                                            (texture-x-to-texel-x texture-atlas
                                                                                  (texture-coordinate-buffer/x1 (:texture-coordinate-buffer texture-atlas)
                                                                                                                index))

                                                            (texture-y-to-texel-y texture-atlas
                                                                                  (texture-coordinate-buffer/y1 (:texture-coordinate-buffer texture-atlas)
                                                                                                                index))

                                                            (texture-x-to-texel-x texture-atlas
                                                                                  (texture-coordinate-buffer/width (:texture-coordinate-buffer texture-atlas)
                                                                                                                index))

                                                            (texture-y-to-texel-y texture-atlas
                                                                                  (texture-coordinate-buffer/height (:texture-coordinate-buffer texture-atlas)
                                                                                                                index)))))


(defn- new-texture-coordinates-with-y1 [texture-atlas width height y1]
  (let [max-x (:width (:texture texture-atlas))
        max-y (:height (:texture texture-atlas))]
    {:x1 0
     :y1 y1
     :x2 (/ width
            max-x)
     :y2 (+ y1
            (/ height
               max-y))}))

(defn- new-texture-coordinates
  ([texture-atlas width height]
     (let [y1 (texture-coordinate-buffer/maximum-y (:texture-coordinate-buffer texture-atlas))]
       (new-texture-coordinates-with-y1 texture-atlas width height y1)))

  ([texture-atlas ignored-index width height]
     (let [y1 (texture-coordinate-buffer/maximum-y (:texture-coordinate-buffer texture-atlas)
                                                   ignored-index)]
       (new-texture-coordinates-with-y1 texture-atlas width height y1))))

(defn update-texture-coordinate-buffer [texture-atlas index x1 y1 x2 y2]
  (assoc texture-atlas :texture-coordinate-buffer
         (texture-coordinate-buffer/update (:texture-coordinate-buffer texture-atlas) index x1 y1 x2 y2)))

(defn resize-texture [texture-atlas index width height]
  (let [{:keys [x1 y1 x2 y2]} (new-texture-coordinates texture-atlas
                                                       width
                                                       height)]

    (update-texture-coordinate-buffer texture-atlas index x1 y1 x2 y2)
    (assoc texture-atlas :needs-to-load true)))


(defn allocate-texture [texture-atlas width height]
  (let [{:keys [x1 y1 x2 y2]} (new-texture-coordinates texture-atlas
                                                       width
                                                       height)
        index (:texture-count texture-atlas)]

    (-> texture-atlas
        (update-texture-coordinate-buffer index x1 y1 x2 y2)
        (assoc :texture-count (+ 1
                                 (:texture-count texture-atlas))
               :needs-to-load true))))

(defn load [texture-atlas]
  (if (:needs-to-load texture-atlas)
    (do
      (texture/load (:texture texture-atlas))
      (assoc texture-atlas
        :texture-coordinate-buffer (texture-coordinate-buffer/load (:texture-coordinate-buffer texture-atlas))
        :needs-to-load false))
    texture-atlas))

(defn delete [texture-atlas]
  (texture/delete (:texture texture-atlas))
  (texture-coordinate-buffer/delete (:texture-coordinate-buffer texture-atlas)))
