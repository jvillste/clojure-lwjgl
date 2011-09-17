(ns clojure-lwjgl.quad-list
  (:refer-clojure :exclude (load))
  (:require [clojure-lwjgl.buffer :as buffer]))

(defn- quad-index-to-index-buffer-index [index] (* index 4))

(defrecord QuadList [quad-count
                     index-buffer
                     index-buffer-id])

(defn create []
  (let [maximum-number-of-quads 10]
    (QuadList. 0
               (buffer/create-int-buffer (quad-index-to-index-buffer-index maximum-number-of-quads))
               (buffer/create-gl-buffer))))

(defn add-quad [quad-list]
  (let [quad-index (:quad-count quad-list)
        first-index-buffer-index (quad-index-to-index-buffer-index quad-index)]

    (buffer/update-buffer (:index-buffer quad-list)
                          first-index-buffer-index
                          (int-array [first-index-buffer-index
                                      (+ first-index-buffer-index 1)
                                      (+ first-index-buffer-index 2)
                                      (+ first-index-buffer-index 3)]))

    (assoc quad-list :quad-count (+ 1
                                    (:quad-count quad-list)))))


(defn load [quad-list]
  (buffer/load-element-buffer (:index-buffer-id quad-list)
                              (:index-buffer quad-list)))


(defn delete [quad-list]
  (buffer/delete (:index-buffer-id quad-list)))