(ns clojure-lwjgl.image-list
  (:refer-clojure :exclude (load))
  (:require (clojure-lwjgl [quad-buffer :as quad-buffer]
                           [quad-list :as quad-list]
                           [draw :as draw]
                           [texture-atlas :as texture-atlas]
                           [zipper-list :as zipper-list]))
  (:import [java.awt Color AlphaComposite]))

(defrecord ImageList [image-count
                      image-ids
                      needs-to-load
                      quad-buffer
                      quad-list
                      texture-atlas])

(defn create []
  (ImageList. 0
              (zipper-list/create)
              false
              (quad-buffer/create)
              (quad-list/create)
              (texture-atlas/create)))

(defn next-index [image-list]
  (:image-count image-list))

(defn add-image [image-list id x y width height]
  (assoc image-list
    :image-count (+ 1
                    (:image-count image-list))
    :image-ids (zipper-list/add (:image-ids image-list)
                                id)
    :quad-buffer (quad-buffer/add-quad (:quad-buffer image-list)
                                       x
                                       y
                                       width
                                       height)
    :quad-list (quad-list/add-quad (:quad-list image-list))
    :texture-atlas (texture-atlas/allocate-texture (:texture-atlas image-list)
                                                   width
                                                   height)))

(defn move-image [image-list id x y]
  (assoc image-list
    :quad-buffer (quad-buffer/move-quad (:quad-buffer image-list)
                                        (zipper-list/index (:image-ids image-list)
                                                           id)
                                        x
                                        y)))

(defn resize-image [image-list id width height]
  (assoc image-list
    :quad-buffer (quad-buffer/resize-quad (:quad-buffer image-list)
                                          (zipper-list/index (:image-ids image-list)
                                                             id)
                                          width
                                          height)
    :texture-atlas (texture-atlas/resize-texture (:texture-atlas image-list)
                                                 (zipper-list/index (:image-ids image-list)
                                                                    id)
                                                 width
                                                 height)))

(defn get-graphics [image-list id]
  (texture-atlas/get-graphics (:texture-atlas image-list)
                              (zipper-list/index (:image-ids image-list)
                                                 id)))

(defn clear-image [image-list id]
  (doto (get-graphics image-list index)
    (.setComposite (AlphaComposite/getInstance AlphaComposite/CLEAR (float 0)))
    (.fillRect 0
               0
               (quad-buffer/quad-width (:quad-buffer image-list) (zipper-list/index (:image-ids image-list)
                                                                                    id))
               (quad-buffer/quad-height (:quad-buffer image-list) (zipper-list/index (:image-ids image-list)
                                                                                     id)))))

(defn- load [image-list]
  (assoc image-list
    :quad-buffer (quad-buffer/load (:quad-buffer image-list))
    :quad-list (quad-list/load (:quad-list image-list))
    :texture-atlas (texture-atlas/load (:texture-atlas image-list))))

(defn- draw-image-list [image-list]
  (draw/draw-quads (:vertex-buffer-id (:quad-buffer image-list))
                   (:buffer-id  (:texture-coordinate-buffer (:texture-atlas image-list)))
                   (:index-buffer-id (:quad-list image-list))
                   (:image-count image-list))
  image-list)

(defn draw [image-list]
  (-> image-list
      (load)
      (draw-image-list)))

(defn delete [image-list]
  (texture-atlas/delete (:texture-atlas image-list))
  (quad-buffer/delete (:quad-buffer image-list))
  (quad-list/delete (:quad-list image-list)))