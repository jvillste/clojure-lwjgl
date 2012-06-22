(ns clojure-lwjgl.test.line
  (:require (clojure-lwjgl [image :as image]
                           [texture :as texture]
                           [buffered-image :as buffered-image]
                           [window :as window]))
  (:import [org.lwjgl.opengl GL11 GL20 ARBVertexBufferObject ARBVertexProgram ARBVertexShader]))

(defn render [application]
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
  (GL11/glMatrixMode GL11/GL_MODELVIEW)
  (GL11/glLoadIdentity)

  (doseq [image (:images application)]
    (image/render image))
  application)

(defn update [application]
  (-> application
      (render)))

(defn load-texture [file-name]
  (-> (buffered-image/create-from-file file-name)
      (texture/create-for-buffered-image)
      (texture/load)))

(defn create-application [window]
  (let [shared-image-resources (image/create-shared-resources)
        texture (load-texture "birdie_logo.png")]
    {:images [(image/create shared-image-resources 0 0 texture)
              (image/create shared-image-resources 100 0 texture)
              (image/create shared-image-resources 0 100 texture)]}))

(comment
(window/start 700 500
                3
                create-application
                update
                (fn [_])))