(ns flow-gl.opengl
  (:require (flow-gl.opengl [triangle-list :as triangle-list])
            (flow-gl.graphics [image :as image]))
  (:import [org.lwjgl.opengl GL11]))


(defn initialize []
  (triangle-list/create-shared-resources)
  (image/create-shared-resources))

(defn dispose []
  (triangle-list/delete-shared-resources)
  (image/delete-shared-resources))

(defn clear [r g b a]
  (GL11/glClearColor r g b a)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT))