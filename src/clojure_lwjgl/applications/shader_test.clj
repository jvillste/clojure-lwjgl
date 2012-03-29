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

(defn width [paint]
  (:width (:texture paint)))

(defn height [paint]
  (:height (:texture paint)))

(defn load [paint]
  (buffer/load-buffer (:vertex-buffer-id paint)
                      (:vertex-buffer paint))
  (assoc paint
    :texture (texture/load (:texture paint))))

(def vertex-shader-source "
attribute vec4 position;
varying vec2 texture_coordinate;

void main() {
    gl_Position = position;
    texture_coordinate = vec2(0.5, 0.5);
}

")

(def fragment-shader-source "
uniform sampler2D texture;
varying vec2 texture_coordinate;

void main() {
    float index = mod(gl_VertexID, 3.0);
    gl_FragColor =texture2D(texture,texture_coordinate); // vec4(1.0f, 0.2f, 0.2f, 1.0f);
}
")


(defn compile-program [paint]
  (when (not (= (:shader-program paint)
                nil))
    (shader/delete-program (:shader-program paint)))
  
  (assoc paint
    :shader-program (shader/compile-program vertex-shader-source
                                            fragment-shader-source)))

(defn create-paint [window]
  {:window window
   :vertex-buffer-id (buffer/create-gl-buffer)
   :vertex-buffer (buffer/update-buffer (buffer/create-float-buffer (* 4 4))
                                        0
                                        (map float [0.75 0.75 0.0 1.0
                                                    0.75 -0.75 0.0 1.0
                                                    -0.75 -0.75 0.0 1.0]))})

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
  ;;  (texture/bind texture)
  ;;  (buffer/bind-buffer (:vertex-buffer-id paint))

  (let [position-index (ARBVertexShader/glGetAttribLocationARB (:shader-program paint) "position")]
    (ARBVertexProgram/glEnableVertexAttribArrayARB position-index)
    (ARBVertexProgram/glVertexAttribPointerARB (int position-index)
                                               (int 4)
                                               (int GL11/GL_FLOAT)
                                               (boolean GL11/GL_FALSE)
                                               (int 0)
                                               (long 0)))
  (GL11/glDrawArrays GL11/GL_TRIANGLES 0 3)

  ;;  (GL11/glEnableClientState GL11/GL_VERTEX_ARRAY)
  ;;  (GL11/glVertexPointer 3 GL11/GL_FLOAT 0 (long 0))
  ;;  (GL12/glDrawRangeElements GL11/GL_QUADS 0 (- (* 4 3 quad-count) 1) (* 4 quad-count) GL11/GL_UNSIGNED_INT 0)
  )

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



