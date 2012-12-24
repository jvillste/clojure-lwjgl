(ns flow-gl.opengl.triangle-list
  (:require (clojure-lwjgl [shader :as shader]
                           [buffer :as buffer]))
  (:import [org.lwjgl.opengl GL11 GL20 ARBVertexBufferObject ARBVertexProgram ARBVertexShader]))

(defrecord TriangleList [number-of-triangles
                         shader-program
                         vertex-coordinate-attribute-index
                         ertex-color-attribute-index
                         vertex-coordinate-buffer-id
                         vertex-coordinate-buffer
                         vertex-color-buffer-id
                         vertex-color-buffer])

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

(defn update [triangle-list index coordinates colors]
  (buffer/update-buffer (:vertex-coordinate-buffer triangle-list)
                        (* index 2 3)
                        coordinates
                        float)
  (buffer/load-buffer (:vertex-coordinate-buffer-id triangle-list)
                      (:vertex-coordinate-buffer triangle-list))

  (buffer/update-buffer (:vertex-color-buffer triangle-list)
                        (* index 4 3)
                        colors
                        float)
  (buffer/load-buffer (:vertex-color-buffer-id triangle-list)
                      (:vertex-color-buffer triangle-list))
  triangle-list)



(defn create [number-of-triangles]
  (let [shader-program (shader/compile-program vertex-shader-source
                                               fragment-shader-source)]
    (map->TriangleList {:number-of-triangles number-of-triangles
                        :shader-program shader-program
                        :vertex-coordinate-attribute-index (ARBVertexShader/glGetAttribLocationARB shader-program "vertex_coordinate_attribute")
                        :vertex-color-attribute-index (ARBVertexShader/glGetAttribLocationARB shader-program "vertex_color_attribute")
                        :vertex-coordinate-buffer-id (buffer/create-gl-buffer)
                        :vertex-coordinate-buffer (buffer/create-float-buffer (* 3 2 number-of-triangles))
                        :vertex-color-buffer-id (buffer/create-gl-buffer)
                        :vertex-color-buffer (buffer/create-float-buffer (* 3 4 number-of-triangles))})))



(defn delete [triangle-list]
  (buffer/delete (:vertex-coordinate-buffer-id triangle-list))
  (buffer/delete (:vertex-color-buffer-id triangle-list))
  (shader/delete-program (:shader-program triangle-list)))


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



