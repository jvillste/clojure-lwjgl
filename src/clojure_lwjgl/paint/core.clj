(ns clojure-lwjgl.paint.core
  (:refer-clojure :exclude (load))
  (:require (clojure-lwjgl [window :as window]
                           [quad-buffer :as quad-buffer]
                           [quad-list :as quad-list]
                           [draw :as draw]
                           [texture :as texture]
                           [texture-coordinate-buffer :as texture-coordinate-buffer]))
  (:import [java.awt Color Font  RenderingHints]
           [org.lwjgl.opengl GL11]))

(def width 400)
(def height 400)

(defn add-quad [paint]
  (assoc paint
    :quad-buffer (quad-buffer/add-quad (:quad-buffer paint) 0 0 width height)
    :quad-list (quad-list/add-quad (:quad-list paint))
    :texture-coordinate-buffer (texture-coordinate-buffer/update (:texture-coordinate-buffer paint)
                                                                 0
                                                                 0.0
                                                                 0.0
                                                                 1.0
                                                                 1.0)))
(defn load [paint]
  (assoc paint
    :quad-buffer (quad-buffer/load (:quad-buffer paint))
    :quad-list (quad-list/load (:quad-list paint))
    :texture-coordinate-buffer (texture-coordinate-buffer/load (:texture-coordinate-buffer paint))
    :texture (texture/load (:texture paint))))

(defn create-paint []
  {:window (window/create)
   :quad-buffer (quad-buffer/create)
   :quad-list (quad-list/create)
   :texture-coordinate-buffer (texture-coordinate-buffer/create)
   :texture (texture/create width height)})

(defn draw-texture [paint]
  (let [graphics (texture/get-graphics (:texture paint))]
    (doto graphics
      (.setColor Color/RED)
      (.fillRect 0 0 200 200))
    paint))

(defn update-window [paint]
  (assoc paint :window (window/update (:window paint))))

(defn render [paint]
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
  (GL11/glLoadIdentity)

  (texture/bind (:texture paint))
  (draw/draw-quads (:vertex-buffer-id (:quad-buffer paint))
                            (:buffer-id (:texture-coordinate-buffer paint))
                            (:index-buffer-id (:quad-list paint))
                            1)
  paint)


(defn update [paint]
  (-> paint
      (render)
      (update-window)))


(def initial-paint
  (-> (create-paint)
      (add-quad)
      (draw-texture)
      (load)))

(println (into [] (clojure-lwjgl.buffer/int-buffer-to-array (:index-buffer (:quad-list initial-paint)))))
(println (into [] (clojure-lwjgl.buffer/float-buffer-to-array (:buffer (:texture-coordinate-buffer initial-paint)))))
(println (into [] (clojure-lwjgl.buffer/float-buffer-to-array (:vertex-buffer (:quad-buffer initial-paint)))))

(try
  (loop [paint initial-paint]
    (if (not @(:close-requested (:window paint)))
      (recur (update paint))
      (window/close (:window paint))))
  (catch Exception e
    (println e)
    (.printStackTrace e)
    (window/close (:window initial-paint))))
