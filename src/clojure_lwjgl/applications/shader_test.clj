(ns clojure-lwjgl.applications.shader-test
  (:refer-clojure :exclude (load))
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


(defn load [paint]
  (buffer/load-buffer (:vertex-buffer-id paint)
                      (:vertex-buffer paint))
  (buffer/load-buffer (:texture-coordinate-buffer-id paint)
                      (:texture-coordinate-buffer paint))
  (assoc paint
    :texture (texture/load (:texture paint))))

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


(defn compile-program [paint]
  (when (not (= (:shader-program paint)
                nil))
    (shader/delete-program (:shader-program paint)))

  (assoc paint
    :shader-program (shader/compile-program vertex-shader-source
                                            fragment-shader-source)))

(defn quad [width height]
  [width height
   width 0
   0   0
   0   height])

(defn create-paint [window]
  {:window window
   :vertex-buffer-id (buffer/create-gl-buffer)
   :vertex-buffer (buffer/update-buffer (buffer/create-float-buffer (* 4 2))
                                        0
                                        (map float (quad 300 200)))
   :texture-coordinate-buffer-id (buffer/create-gl-buffer)
   :texture-coordinate-buffer (buffer/update-buffer (buffer/create-float-buffer (* 4 2))
                                                    0
                                                    (map float [1 1
                                                                1 0
                                                                0 0
                                                                0 1]))})

(defn load-images [paint]
  (assoc paint
    :texture (texture/create-for-buffered-image (buffered-image/create-from-file "birdie_logo.png"))))

(defn update-window [paint]
  (assoc paint :window (window/update (:window paint)
                                      3)))

(defn render-texture [paint texture]
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
  (GL11/glMatrixMode GL11/GL_MODELVIEW)
  (GL11/glLoadIdentity)

  (shader/enable-program (:shader-program paint))
  (texture/bind texture)

  (buffer/bind-buffer (:vertex-buffer-id paint))
  (let [position-index (ARBVertexShader/glGetAttribLocationARB (:shader-program paint) "position")]
    (ARBVertexProgram/glEnableVertexAttribArrayARB position-index)
    (ARBVertexProgram/glVertexAttribPointerARB (int position-index)
                                               (int 2)
                                               (int GL11/GL_FLOAT)
                                               (boolean GL11/GL_FALSE)
                                               (int 0)
                                               (long 0)))

  (buffer/bind-buffer (:texture-coordinate-buffer-id paint))
  (let [texture-coordinate-attribute-index (ARBVertexShader/glGetAttribLocationARB (:shader-program paint) "texture_coordinate_attribute")]
    (ARBVertexProgram/glEnableVertexAttribArrayARB texture-coordinate-attribute-index)
    (ARBVertexProgram/glVertexAttribPointerARB (int texture-coordinate-attribute-index)
                                               (int 2)
                                               (int GL11/GL_FLOAT)
                                               (boolean GL11/GL_FALSE)
                                               (int 0)
                                               (long 0)))
  (GL11/glDrawArrays GL11/GL_QUADS 0 4))

(defn render [paint]
  (render-texture paint
                  (:texture paint))
  paint)


(defn update [paint]
  (-> paint
      (compile-program)
      (render)
      (update-window)))

(comment
(let [window (window/create 700 500)]
    (try
      (let [initial-paint (-> (create-paint window)
                              (load-images)
                              (load))]
        (loop [paint initial-paint]
          (if (not @(:close-requested (:window paint)))
            (recur (update paint))
            (window/close window))))

      (catch Exception e
        (println e)
        (.printStackTrace e)
        (window/close window)))))



