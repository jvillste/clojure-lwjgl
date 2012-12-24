(ns flow-gl.opengl
  (:import [org.lwjgl.opengl GL11]))

(defn clear [r g b a]
  (GL11/glClearColor r g b a)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT))