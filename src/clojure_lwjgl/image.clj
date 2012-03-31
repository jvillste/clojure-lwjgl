(ns clojure-lwjgl.image
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
attribute vec2 texture_coordinate_attribute;

varying vec2 texture_coordinate;

void main() {
    gl_Position = gl_ProjectionMatrix * vec4(vertex_coordinate_attribute[0], vertex_coordinate_attribute[1], 0.0, 1.0);
    texture_coordinate = texture_coordinate_attribute;
}

")

(def fragment-shader-source "
#version 120

uniform sampler2D texture;
varying vec2 texture_coordinate;

void main() {
    gl_FragColor = texture2D(texture, texture_coordinate);
}
")


(defn create-shared-resources []
  (let [shader-program (shader/compile-program vertex-shader-source
                                               fragment-shader-source)
        vertex-coordinate-attribute-index (ARBVertexShader/glGetAttribLocationARB shader-program "vertex_coordinate_attribute")
        texture-coordinate-attribute-index (ARBVertexShader/glGetAttribLocationARB shader-program "texture_coordinate_attribute")

        texture-coordinate-buffer-id (buffer/create-gl-buffer)
        texture-coordinate-buffer (buffer/update-buffer (buffer/create-float-buffer (* 4 2))
                                                        0
                                                        (map float [1 1
                                                                    1 0
                                                                    0 0
                                                                    0 1]))]
    (buffer/load-buffer texture-coordinate-buffer-id
                        texture-coordinate-buffer)


    {:shader-program shader-program
     :texture-coordinate-buffer-id texture-coordinate-buffer-id
     :texture-coordinate-buffer texture-coordinate-buffer
     :vertex-coordinate-attribute-index vertex-coordinate-attribute-index
     :texture-coordinate-attribute-index texture-coordinate-attribute-index}))

(defn quad [x y width height]
  [(+ x width) (+ y height)
   (+ x width) y
   x   y
   x   (+ y height)])

(defn width [image]
  (:width (:texture image)))

(defn height [image]
  (:height (:texture image)))

(defn update-vertexes [image]
  (buffer/update-buffer (:vertex-coordinate-buffer image)
                        0
                        (map float (quad (:x image)
                                         (:y image)
                                         (width image)
                                         (height image))))
  (buffer/load-buffer (:vertex-coordinate-buffer-id image)
                      (:vertex-coordinate-buffer image))
  image)

(defn move [image x y]
  (-> (assoc image
        :x x
        :y y)
      (update-vertexes)))

(defn set-texture [image texture]
  (texture/delete (:texture image))
  (-> (assoc image
        :texture texture)
      (update-vertexes)))

(defn create [shared-resources x y texture]
  (let [image (merge shared-resources
                     {:x x
                      :y y
                      :texture texture
                      :vertex-coordinate-buffer-id (buffer/create-gl-buffer)
                      :vertex-coordinate-buffer (buffer/create-float-buffer (* 4 2))})]

    (update-vertexes image)
    image))

(defn delete [image]
  (texture/delete (:texture image))
  (buffer/delete (:vertex-coordinate-buffer-id)))

(defn delete-shared-resources [shared-resources]
  (shader/delete-program (:shader-program shared-resources))
  (buffer/delete (:texture-coordinate-buffer-id)))

(defn render [image]
  (shader/enable-program (:shader-program image))
  (texture/bind (:texture image))

  (buffer/bind-buffer (:vertex-coordinate-buffer-id image))
  (ARBVertexProgram/glEnableVertexAttribArrayARB (:vertex-coordinate-attribute-index image))
  (ARBVertexProgram/glVertexAttribPointerARB (int (:vertex-coordinate-attribute-index image))
                                             (int 2)
                                             (int GL11/GL_FLOAT)
                                             (boolean GL11/GL_FALSE)
                                             (int 0)
                                             (long 0))

  (buffer/bind-buffer (:texture-coordinate-buffer-id image))
  (ARBVertexProgram/glEnableVertexAttribArrayARB (:texture-coordinate-attribute-index image))
  (ARBVertexProgram/glVertexAttribPointerARB (int (:texture-coordinate-attribute-index image))
                                             (int 2)
                                             (int GL11/GL_FLOAT)
                                             (boolean GL11/GL_FALSE)
                                             (int 0)
                                             (long 0))
  (GL11/glDrawArrays GL11/GL_QUADS 0 4))
