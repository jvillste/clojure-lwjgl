(ns flow-gl.opengl.triangle-list
  (:require (flow-gl.opengl [shader :as shader]
                            [buffer :as buffer]))
  (:import [org.lwjgl.opengl GL11 GL20 ARBVertexBufferObject ARBVertexProgram ARBVertexShader]))

(defrecord TriangleList [mode
                         number-of-triangles
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

(defn update [triangle-list coordinates colors]

  (buffer/load-buffer (:vertex-coordinate-buffer-id triangle-list)
                      :float
                      coordinates)

  (buffer/load-buffer (:vertex-color-buffer-id triangle-list)
                      :float
                      colors)

  (assoc triangle-list
    :number-of-triangles (/ (count coordinates)
                            2
                            3)))

(def shader-program-atom (atom nil))

(defn create-shared-resources []
  (reset! shader-program-atom (shader/compile-program vertex-shader-source
                                                 fragment-shader-source)))

(defn delete-shared-resources []
  (shader/delete-program @shader-program-atom)
  (reset! shader-program-atom nil))

(defn create [mode]
  (map->TriangleList {:mode mode
                      :vertex-coordinate-attribute-index (ARBVertexShader/glGetAttribLocationARB @shader-program-atom "vertex_coordinate_attribute")
                      :vertex-color-attribute-index (ARBVertexShader/glGetAttribLocationARB @shader-program-atom "vertex_color_attribute")
                      :vertex-coordinate-buffer-id (buffer/create-gl-buffer)
                      :vertex-color-buffer-id (buffer/create-gl-buffer)}))

(defn create-for-coordinates [mode coordinates colors]
  (-> (create mode)
      (update coordinates
              colors)))

(defn delete [triangle-list]
  (buffer/delete (:vertex-coordinate-buffer-id triangle-list))
  (buffer/delete (:vertex-color-buffer-id triangle-list))
  triangle-list)


(defn render [triangle-list]
  (shader/enable-program @shader-program-atom)

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

  (case (:mode triangle-list)
    :triangles (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (* 3 (:number-of-triangles triangle-list)))
    :triangle-strip (GL11/glDrawArrays GL11/GL_TRIANGLE_STRIP 0 (+ 2 (:number-of-triangles triangle-list)))
    :triangle-fan (GL11/glDrawArrays GL11/GL_TRIANGLE_FAN 0 (:number-of-triangles triangle-list)))

  triangle-list)