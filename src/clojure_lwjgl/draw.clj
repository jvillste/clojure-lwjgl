(ns clojure-lwjgl.draw
  (:import [org.lwjgl.opengl GL11 GL12])
  (:require [clojure-lwjgl.buffer :as buffer]))

(defn draw-quads [vertex-buffer-id
                  texture-coordinate-buffer-id
                  index-buffer-id
                  quad-count]

  (println vertex-buffer-id
           texture-coordinate-buffer-id
           index-buffer-id
           quad-count)

  (GL11/glEnableClientState GL11/GL_TEXTURE_COORD_ARRAY)
  (buffer/bind-buffer texture-coordinate-buffer-id)
  (GL11/glTexCoordPointer 2 GL11/GL_FLOAT 0 (long 0))

  (GL11/glEnableClientState GL11/GL_VERTEX_ARRAY)
  (buffer/bind-buffer vertex-buffer-id)
  (GL11/glVertexPointer 3 GL11/GL_FLOAT 0 (long 0))

  (buffer/bind-element-buffer index-buffer-id)
  (GL12/glDrawRangeElements GL11/GL_QUADS 0 (- (* 4 3 quad-count) 1) (* 4 quad-count) GL11/GL_UNSIGNED_INT 0))

(defn draw-triangles [color-buffer-id
                      vertex-buffer-id
                      index-buffer-id
                      triangle-count]

  (GL11/glEnableClientState GL11/GL_VERTEX_ARRAY)
  (buffer/bind-buffer vertex-buffer-id)
  (GL11/glVertexPointer 3 GL11/GL_FLOAT 0 (long 0))

  (GL11/glEnableClientState GL11/GL_COLOR_ARRAY)
  (buffer/bind-buffer color-buffer-id)
  (GL11/glColorPointer 4 GL11/GL_FLOAT 0 (long 0))

  (buffer/bind-element-buffer index-buffer-id)
  (GL12/glDrawRangeElements GL11/GL_TRIANGLES 0 (- (* 3 4 triangle-count) 1) (* 3 triangle-count) GL11/GL_UNSIGNED_INT 0))
