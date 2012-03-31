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

attribute vec2 position;
attribute vec2 texture_coordinate_attribute;

varying vec2 texture_coordinate;

void main() {
    gl_Position = gl_ProjectionMatrix * vec4(position[0],position[1], 0.0, 1.0);
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
        texture-coordinate-buffer-id (buffer/create-gl-buffer)
        texture-coordinate-buffer (buffer/update-buffer (buffer/create-float-buffer (* 4 2))
                                                        0
                                                        (map float [1 1
                                                                    1 0
                                                                    0 0
                                                                    0 1]))
        position-index (ARBVertexShader/glGetAttribLocationARB shader-program "position")
        texture-coordinate-attribute-index (ARBVertexShader/glGetAttribLocationARB shader-program "texture_coordinate_attribute")]
    (buffer/load-buffer texture-coordinate-buffer-id
                        texture-coordinate-buffer)

    {:shader-program shader-program
     :texture-coordinate-buffer-id texture-coordinate-buffer-id
     :texture-coordinate-buffer texture-coordinate-buffer
     :position-index position-index
     :texture-coordinate-attribute-index texture-coordinate-attribute-index}))

(defn quad [x y width height]
  [(+ x width) (+ y height)
   (+ x width) y
   x   y
   x   (+ y height)])

(defn move [image x y]
  (buffer/update-buffer (:vertex-coordinate-buffer image)
                        0
                        (map float (quad x
                                         y
                                         (:width (:texture image))
                                         (:height (:texture image)))))
  (buffer/load-buffer (:vertex-coordinate-buffer-id image)
                      (:vertex-coordinate-buffer image)))

(defn create [shared-resources x y texture]
  (let [image (merge shared-resources
                     {:texture texture
                      :vertex-coordinate-buffer-id (buffer/create-gl-buffer)
                      :vertex-coordinate-buffer (buffer/create-float-buffer (* 4 2))})]
    (move image x y)
    image))

(defn render [image]
  (shader/enable-program (:shader-program image))
  (texture/bind (:texture image))

  (buffer/bind-buffer (:vertex-coordinate-buffer-id image))
  (ARBVertexProgram/glEnableVertexAttribArrayARB (:position-index image))
  (ARBVertexProgram/glVertexAttribPointerARB (int (:position-index image))
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
