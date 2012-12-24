(ns flow-gl.graphics.image-list
  (:refer-clojure :exclude (load))
  (:require [flow-gl.opengl.texture :as texture]
            [flow-gl.graphics.image :as image]
            [flow-gl.data.zipper-list :as zipper-list])
  (:import [java.awt Color AlphaComposite]))

(defrecord ImageList [images
                      id-list
                      shared-resources])

(defn create []
  (ImageList. {}
              (zipper-list/create)
              (image/create-shared-resources)))

(defn add-image-for-texture [image-list id x y texture]
  (assoc image-list
    :images (assoc (:images image-list)
              id (image/create (:shared-resources image-list)
                               x
                               y
                               texture))
    :id-list (zipper-list/add (:id-list image-list)
                              id)))

(defn add-image [image-list id x y width height]
  (add-image-for-texture image-list
                         id
                         x
                         y
                         (-> (texture/create width height)
                             (texture/load))))

(defn update-image [image-list id updater]
  (assoc image-list
    :images (assoc (:images image-list)
              id (updater ((:images image-list) id)))))

(defn move-image [image-list id x y]
  (update-image image-list
                id
                #(image/move % x y)))

(defn resize-image [image-list id width height]
  (update-image image-list
                id
                #(image/set-texture % (texture/create width height))))

(defn get-graphics [image-list id]
  (texture/get-graphics (:texture ((:images image-list) id))))

(defn draw-on-image [image-list id drawer]
  (drawer (get-graphics image-list id))
  (texture/load (:texture ((:images image-list) id)))
  image-list)

(defn clear-image [image-list id]
  (doto (get-graphics image-list id)
    (.setComposite (AlphaComposite/getInstance AlphaComposite/CLEAR (float 0)))
    (.fillRect 0
               0
               (image/width ((:images image-list) id))
               (image/height ((:images image-list) id))))
  (texture/load (:texture ((:images image-list) id))))

(defn draw [image-list]
  (doseq [image-id (zipper-list/items (:id-list image-list))]
    (image/render ((:images image-list) image-id)))
  image-list)

(defn delete [image-list]
  (doseq [image (vals (:images image-list))]
    (image/delete image))
  (image/delete-shared-resources (:shared-resources image-list)))
