(ns flow-gl.graphics.command.image
  (:require (flow-gl.graphics [image :as image]
                              [texture :as texture]
                              [image-list :as image-list]
                              [buffered-image :as buffered-image])
            [clojure-lwjgl.command.command :as command]))

(defrecord Image [x y file-name])

(defrecord ImageList [images])

(defrecord ImageListRunner [image-list])

(defn load-texture [file-name]
  (-> (buffered-image/create-from-file file-name)
      (texture/create-for-buffered-image)
      (texture/load)))

(defn create-image-list [images]
  (reduce (fn [image-list [index image]]
            (image-list/add-image-for-texture image-list
                                              index
                                              (:x image)
                                              (:y image)
                                              (load-texture (:file-name image))))
          (image-list/create)
          (map-indexed vector images)))

(defn combine-image-lists [image-list1 image-list2]
  (->ImageList (concat (:images image-list1)
                       (:images image-list2))))

(defn create-image-list-runner [image-list]
  (->ImageListRunner (create-image-list (:images image-list))))


(extend ImageList
  command/Command
  {:create-runner create-image-list-runner}
  command/CombinableCommand
  {:combine combine-image-lists})

(extend ImageListRunner
  command/CommandRunner
  {:delete (fn [image-list-runner] (image-list/delete (:image-list image-list-runner)))
   :run (fn [image-list-runner] (image-list/draw (:image-list image-list-runner)))})

(defn create [x y file-name] (->ImageList [(->Image x y file-name)]))
