(ns clojure-lwjgl.image-list
  (:require (clojure-lwjgl [quad-buffer :as quad-buffer]
                           [quad-list :as quad-list]
                           [draw :as draw]
                           [texture-atlas :as texture-atlas])))

(defrecord ImageList [image-count
                      quad-buffer
                      quad-list
                      texture-atlas])

(defn create []
  (ImageList. 0
              (quad-buffer/create)
              (quad-list/create)
              (texture-atlas/create)))

(defn add-image [image-list x y width height]
  (-> image-list
      (assoc :image-count (+ 1
                             (:image-count image-list)))

      (assoc :quad-buffer (quad-buffer/add-quad (:quad-buffer image-list)
                                                x
                                                y
                                                width
                                                height))

      (assoc :quad-list (quad-list/add-quad (:quad-list image-list)))

      (assoc :texture-atlas (texture-atlas/allocate-texture (:texture-atlas image-list)
                                                            width
                                                            height))))

(defn get-graphics [image-list index]
  (texture-atlas/get-graphics (:texture-atlas image-list) index))

(defn load [image-list]
  (quad-buffer/load (:quad-buffer image-list))
  (quad-list/load (:quad-list image-list))
  (texture-atlas/load (:texture-atlas image-list)))

(defn draw [image-list]
  (draw/draw-quads (:vertex-buffer-id (:quad-buffer image-list))
                   (:texture-coordinate-buffer-id (:texture-atlas image-list))
                   (:index-buffer-id (:quad-list image-list))
                   (:image-count image-list)))

(defn delete [image-list]
  (texture-atlas/delete (:texture-atlas image-list))
  (quad-buffer/delete (:quad-buffer image-list))
  (quad-list/delete (:quad-list image-list)))