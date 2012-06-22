(ns clojure-lwjgl.triangle-list
  (:require (clojure-lwjgl [window :as window]
                           [quad-buffer :as quad-buffer]
                           [quad-list :as quad-list]
                           [draw :as draw]
                           [texture :as texture]
                           [texture-coordinate-buffer :as texture-coordinate-buffer]
                           [frame-buffer-object :as frame-buffer-object]
                           [shader :as shader]
                           [buffered-image :as buffered-image]
                           [input :as input]
                           [buffer :as buffer])
            (clojure-lwjgl.paint [vector2d :as vector2d]))
  (:import [java.awt Color Font  RenderingHints]
           [org.lwjgl.opengl GL11 GL20 ARBVertexBufferObject ARBVertexProgram ARBVertexShader]))

(def vertex-shader-source "
#version 120

attribute vec2 vertex_coordinate_attribute;
attribute vec4 vertex_color_attribute;

varying vec4 color;

void main() {
    gl_Position = gl_ProjectionMatrix * gl_ModelViewMatrix * vec4(vertex_coordinate_attribute[0], vertex_coordinate_attribute[1], 0.0, 1.0);
    color = vertex_color_attribute;
}

")

(def fragment-shader-source "
#version 120

varying vec4 color;

void main() {
    gl_FragColor = color;
}
")

(defn update [triangle-list index triangle]
  (buffer/update-buffer (:vertex-coordinate-buffer triangle-list)
                        (* index 2 3)
                        (triangle :coordinates))
  (buffer/load-buffer (:vertex-coordinate-buffer-id triangle-list)
                      (:vertex-coordinate-buffer triangle-list))
  (buffer/update-buffer (:vertex-color-buffer triangle-list)
                        (* index 4 3)
                        (triangle :colors))
  (buffer/load-buffer (:vertex-color-buffer-id triangle-list)
                      (:vertex-color-buffer triangle-list))
  triangle-list)

(defn update-many [triangle-list start-index triangles]
  (loop [index start-index
         triangles triangles]
    (when (seq triangles)
      (update triangle-list
              index
              (first triangles))
      (recur (+ index 1) (rest triangles))))
  triangle-list)



(defn create [number-of-triangles]
  (let [shader-program (shader/compile-program vertex-shader-source
                                               fragment-shader-source)]
    {:number-of-triangles number-of-triangles
     :shader-program shader-program
     :vertex-coordinate-attribute-index (ARBVertexShader/glGetAttribLocationARB shader-program "vertex_coordinate_attribute")
     :vertex-color-attribute-index (ARBVertexShader/glGetAttribLocationARB shader-program "vertex_color_attribute")
     :vertex-coordinate-buffer-id (buffer/create-gl-buffer)
     :vertex-coordinate-buffer (buffer/create-float-buffer (* 3 2 number-of-triangles))
     :vertex-color-buffer-id (buffer/create-gl-buffer)
     :vertex-color-buffer (buffer/create-float-buffer (* 3 4 number-of-triangles))}))

(defn delete [triangle-buffer]
  (buffer/delete (:vertex-coordinate-buffer-id triangle-buffer))
  (buffer/delete (:vertex-color-buffer-id triangle-buffer))
  (shader/delete-program (:shader-program triangle-buffer)))

(defn render [triangle-list]
  (shader/enable-program (:shader-program triangle-list))

  (buffer/bind-buffer (:vertex-coordinate-buffer-id triangle-list))
  (ARBVertexProgram/glEnableVertexAttribArrayARB (:vertex-coordinate-attribute-index triangle-list))
  (ARBVertexProgram/glVertexAttribPointerARB (int (:vertex-coordinate-attribute-index triangle-list))
                                             (int 2)
                                             (int GL11/GL_FLOAT)
                                             (boolean GL11/GL_FALSE)
                                             (int 0)
                                             (long 0))

  (buffer/bind-buffer (:vertex-color-buffer-id triangle-list))
  (ARBVertexProgram/glEnableVertexAttribArrayARB (:vertex-color-attribute-index triangle-list))
  (ARBVertexProgram/glVertexAttribPointerARB (int (:vertex-color-attribute-index triangle-list))
                                             (int 4)
                                             (int GL11/GL_FLOAT)
                                             (boolean GL11/GL_FALSE)
                                             (int 0)
                                             (long 0))
  (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (* 4 (:number-of-triangles triangle-list))))

