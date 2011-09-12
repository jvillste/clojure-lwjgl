(ns clojure-lwjgl.core
  (:require [clojure-lwjgl.text :as text]
            [clojure-lwjgl.texture :as texture]
            [clojure-lwjgl.window :as window]
            [clojure-lwjgl.buffered-image :as buffered-image])
  (:import [org.lwjgl.opengl GL11]))

(defn initialize []
  (println "initialize"))

(def render (atom (fn [])))

(reset!  render (fn []

  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
  (GL11/glLoadIdentity)

  ;;  (GL11/glTranslatef 100 100 0)
;;  (GL11/glScalef 1 1 1)

  (let [text (text/create "Foo ja muuta tekstia")
        texture (texture/create 128 128)
        child-image (buffered-image/create-child (:buffered-image texture)
                                                 10
                                                 10
                                                 (text/get-width text)
                                                 (text/get-height text))]

    (text/render text (buffered-image/get-graphics child-image))
    (texture/load texture)
    (texture/draw texture)
;;    (println (:id texture))
    (texture/delete texture))

  ))

(window/open render initialize)



