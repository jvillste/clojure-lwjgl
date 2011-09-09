(ns clojure-lwjgl.core
  (:require [clojure-lwjgl.text :as text]
            [clojure-lwjgl.texture :as texture]
            [clojure-lwjgl.window :as window])
  (:import [org.lwjgl.opengl GL11]))


(defn initialize []
  (def text (text/create "Foo"))
  (def texture (texture/create 128))
  (text/render text (texture/get-graphics texture)))


(defn render []

  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
  (GL11/glLoadIdentity)

  ;;  (GL11/glTranslatef 100 100 0)
   (GL11/glScalef 40 40 1)


  (dotimes [n 20]
    (GL11/glRotatef (* (/ (System/nanoTime) 1000000000) (float 14)) 0 0 1)
    (GL11/glTranslatef (* n (float 0.24)) 0 0)
    (GL11/glRotatef (* (/ (System/nanoTime) 1000000000) (float 2)) 0 0 1)
    (texture/draw texture)))

(window/open render initialize)


