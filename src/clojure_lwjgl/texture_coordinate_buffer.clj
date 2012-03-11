(ns clojure-lwjgl.texture-coordinate-buffer
  (:import [org.lwjgl.opengl GL11 GL12])
  (:require [clojure-lwjgl.buffer :as buffer]))


(defrecord TextureCoordinateBuffer [buffer
                                    buffer-id
                                    needs-to-load])

(defn bind [texture-coordinate-buffer]
  (GL11/glEnableClientState GL11/GL_TEXTURE_COORD_ARRAY)
  (buffer/bind-buffer (:buffer-id  texture-coordinate-buffer))
  (GL11/glTexCoordPointer 2 GL11/GL_FLOAT 0 (long 0)))

(defn texture-index-to-texture-coordinate-index [texture-index]
  (* texture-index
     4
     2))

(defn create []
  (let [initial-number-of-textures 10]
    (TextureCoordinateBuffer. (buffer/create-float-buffer (texture-index-to-texture-coordinate-index initial-number-of-textures))
                              (buffer/create-gl-buffer)
                              true)))

(defn coordinate [texture-coordinate-buffer texture-index coordinate-index]
  (.get (:buffer texture-coordinate-buffer)
        (+ (texture-index-to-texture-coordinate-index texture-index)
           coordinate-index)))

(defn x1 [texture-coordinate-buffer index]
  (coordinate texture-coordinate-buffer index 0))

(defn y1 [texture-coordinate-buffer index]
  (coordinate texture-coordinate-buffer index 1))

(defn x2 [texture-coordinate-buffer index]
  (coordinate texture-coordinate-buffer index 4))

(defn y2 [texture-coordinate-buffer index]
  (coordinate texture-coordinate-buffer index 5))

(defn width [texture-coordinate-buffer index]
  (- (x2 texture-coordinate-buffer index)
     (x1 texture-coordinate-buffer index)))

(defn height [texture-coordinate-buffer index]
  (- (y2 texture-coordinate-buffer index)
     (y1 texture-coordinate-buffer index)))


(defn remove-nth [n values]
  (loop [result []
         index 0
         rest-values values]
    (let [value (first values)]
      (if value
        (if (= index n)
          (recur result
                 (+ index 1)
                 (rest values))
          (recur (conj result value)
                 (+ index 1)
                 (rest values)))
        result))))

(defn maximum-y
  ([texture-coordinate-buffer]
     (apply max (map second (partition 2 (buffer/float-buffer-to-array (:buffer texture-coordinate-buffer))))))

  ([texture-coordinate-buffer ignored-index]
     (apply max (remove-nth ignored-index (map second (partition 2 (buffer/float-buffer-to-array (:buffer texture-coordinate-buffer))))))))


(defn update [texture-coordinate-buffer index x1 y1 x2 y2]
  (assoc texture-coordinate-buffer
    :buffer (buffer/update-buffer (:buffer texture-coordinate-buffer)
                                  (texture-index-to-texture-coordinate-index index)
                                  (float-array [x1 y1
                                                x2 y1
                                                x2 y2
                                                x1 y2]))
    :needs-to-load true))

(defn load [texture-coordinate-buffer]
  (if (:needs-to-load texture-coordinate-buffer)
    (do
      (buffer/load-buffer (:buffer-id texture-coordinate-buffer)
                          (:buffer texture-coordinate-buffer))
      (assoc texture-coordinate-buffer :needs-to-load false))
    texture-coordinate-buffer))

(defn delete [texture-coordinate-buffer]
  (buffer/delete (:buffer-id texture-coordinate-buffer)))