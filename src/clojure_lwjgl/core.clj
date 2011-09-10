(ns clojure-lwjgl.core
  (:require [clojure-lwjgl.text :as text]
            [clojure-lwjgl.texture :as texture]
            [clojure-lwjgl.window :as window])
  (:import [org.lwjgl.opengl GL11]))

(defn initialize []
  (println "initialize")
  (def text (text/create "Foo"))
  (def texture (texture/create 128))
  (text/render text (texture/get-graphics texture)))

(def render (atom (fn [])))

(reset!  render (fn []

  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
  (GL11/glLoadIdentity)

  ;;  (GL11/glTranslatef 100 100 0)
;;  (GL11/glScalef 1 1 1)

  (let [text (text/create "Foo ja muuta tekstia")
        texture (texture/create 128)]
    (text/render text (texture/get-graphics texture))
    (texture/load texture)
    (texture/draw texture))

  ))


(window/open render initialize)


