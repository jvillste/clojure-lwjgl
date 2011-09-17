(ns clojure-lwjgl.core
  (:require [clojure-lwjgl.text :as text]
            [clojure-lwjgl.image-list :as image-list]
            [clojure-lwjgl.component :as component]
            [clojure-lwjgl.texture :as texture]
            [clojure-lwjgl.window :as window]
            [clojure-lwjgl.input :as input]
            [clojure-lwjgl.buffer :as buffer]
            [clojure-lwjgl.buffered-image :as buffered-image])
  (:import [org.lwjgl.opengl GL11]))

(defn initialize []
  (println "initialize"))

(def render (atom nil))

(def handle-input (atom nil))

(def input (input/create handle-input))

(reset! render
        (fn []

          (input/read-input input)

          (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
          (GL11/glLoadIdentity)

          ;;  (GL11/glTranslatef 100 100 0)
          ;;            (GL11/glScalef 3 3 1)
          (let [text (text/create "Foo")
                image-list (-> (image-list/create)
                               (image-list/add-image 0
                                                     0
                                                     (text/get-width text)
                                                     (text/get-height text)))]

;;            (println "vertex buffer: " (vec (buffer/float-buffer-to-array (:vertex-buffer (:quad-buffer image-list)))))
;;            (println "index buffer: " (vec (buffer/int-buffer-to-array (:index-buffer (:quad-list image-list)))))
;;            (println "texture coordinate buffer: " (vec (buffer/float-buffer-to-array (:texture-coordinate-buffer (:texture-atlas image-list)))))            
            (text/render text (image-list/get-graphics image-list 0))
            (image-list/load image-list)
            (image-list/draw image-list)
            ;;            (texture/draw (:texture (:texture-atlas image-list)))
            (image-list/delete image-list))))


(reset! handle-input
        (fn [input-state]
          (when (not (= (:type (:last-event input-state))
                        :mouse-moved))
            (println input-state))))

(window/open render initialize)

