(ns clojure-lwjgl.test.triangle-list.clj
  (:require (clojure-lwjgl [triangle-list :as triangle-list]
                           [window :as window]
                           [vector-rectangle :as vector-rectangle]))
  (:import [org.lwjgl.opengl GL11 GL20 ARBVertexBufferObject ARBVertexProgram ARBVertexShader]))

(defn render [application]
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
  (GL11/glMatrixMode GL11/GL_MODELVIEW)
  (GL11/glLoadIdentity)

  (triangle-list/render (application :triangle-list))
  application)

(defn update [application]
  (-> application
      (render)))

(defn create-application [window]
  {:triangle-list (-> (triangle-list/create 1)
                      (triangle-list/update 0 {:coordinates (map float [0.0 0.0
                                                                        100.0 0.0
                                                                        100.0 100.0])
                                               :colors (map float [1.0 1.0 1.0 1.0
                                                                   0.0 0.0 1.0 1.0
                                                                   1.0 1.0 1.0 1.0])}))})

(comment
(window/start 700 500
                3
                create-application
                update
                (fn [])))