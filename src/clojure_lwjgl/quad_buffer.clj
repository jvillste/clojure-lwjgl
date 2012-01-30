(ns clojure-lwjgl.quad-buffer
  (:refer-clojure :exclude (load))
  (:require [clojure-lwjgl.buffer :as buffer]))

(defn- quad-index-to-vertex-buffer-index [index] (* index 3 4))

(defrecord QuadBuffer [quad-count
                       vertex-buffer
                       vertex-buffer-id])

(defn create []
  (let [maximum-number-of-quads 10]
    (QuadBuffer. 0
                 (buffer/create-float-buffer (quad-index-to-vertex-buffer-index maximum-number-of-quads))
                 (buffer/create-gl-buffer))))

(defn coordinate [quad-buffer quad-index coordinate-index]
  (.get (:vertex-buffer quad-buffer)
        (+ (quad-index-to-vertex-buffer-index quad-index)
           coordinate-index)))

(defn quad-x1 [quad-buffer index]
  (coordinate quad-buffer index 0))

(defn quad-y1 [quad-buffer index]
  (coordinate quad-buffer index 7))

(defn quad-x2 [quad-buffer index]
  (coordinate quad-buffer index 3))

(defn quad-y2 [quad-buffer index]
  (coordinate quad-buffer index 1))

(defn quad-width [quad-buffer index]
  (- (quad-x2 quad-buffer index)
     (quad-x1 quad-buffer index)))

(defn quad-height [quad-buffer index]
  (- (quad-y2 quad-buffer index)
     (quad-y1 quad-buffer index)))

(defn set-quad-coordinates [quad-buffer index x y width height]
  (let [x1 x
        y1 y
        x2 (+ x1 width)
        y2 (+ y1 height)]

    (buffer/update-buffer (:vertex-buffer quad-buffer)
                          (quad-index-to-vertex-buffer-index index)
                          ;; (float-array [x1 y1 0.0
                          ;;               x2 y1 0.0
                          ;;               x2 y2 0.0
                          ;;               x1 y2 0.0])
                          (float-array [x1 y2 0.0
                                       x2 y2 0.0
                                        x2 y1 0.0
                                        x1 y1 0.0])
                          )
    (assoc quad-buffer :needs-to-load true)))

(defn add-quad [quad-buffer x y width height]
  (-> quad-buffer
      (set-quad-coordinates (:quad-count quad-buffer)
                            x
                            y
                            width
                            height)

      (assoc :quad-count (+ 1
                            (:quad-count quad-buffer)))))


(defn move-quad [quad-buffer index x y]
  (set-quad-coordinates quad-buffer
                        x
                        y
                        (quad-width quad-buffer index)
                        (quad-height quad-buffer index)))

(defn resize-quad [quad-buffer index width height]
  (set-quad-coordinates quad-buffer
                        (quad-x1 quad-buffer index)
                        (quad-y1 quad-buffer index)
                        width
                        height))

(defn load [quad-buffer]
  (when (:needs-to-load quad-buffer)
    (buffer/load-buffer (:vertex-buffer-id quad-buffer)
                        (:vertex-buffer quad-buffer)))
  (assoc quad-buffer :needs-to-load false))

(defn delete [quad-buffer]
  (buffer/delete (:vertex-buffer-id quad-buffer)))