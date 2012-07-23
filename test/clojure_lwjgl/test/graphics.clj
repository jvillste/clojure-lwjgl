(ns clojure-lwjgl.test.graphics
  (:require (clojure-lwjgl [image :as image]
                           [image-list :as image-list]
                           [texture :as texture]
                           [buffered-image :as buffered-image]
                           [window :as window]
                           [font :as font]
                           [text-list :as text-list]
                           [triangle-batch :as triangle-batch]
                           [vector-rectangle :as vector-rectangle]
                           [primitive-list :as primitive-list]
                           [primitive :as primitive]))
  (:import [org.lwjgl.opengl GL11 GL20 ARBVertexBufferObject ARBVertexProgram ARBVertexShader]
           [java.awt Color Font FontMetrics RenderingHints]))

(defn render [application]
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)

  (println "render")
  (doseq [primitive-list (:primitive-lists application)]
    (println "drawing list")
    (primitive-list/draw primitive-list))

  application)

(defn update [application]
  (-> application
      (render)))

(def primitives [(text-list/->Text 20
                                   20
                                   "Foo"
                                   (font/create "LiberationSans-Regular.ttf" 17)
                                   [1.0 0.0 0.0 1.0])
                 (text-list/->Text 60
                                   20
                                   "Foo"
                                   (font/create "LiberationSans-Regular.ttf" 17)
                                   [1.0 0.0 0.0 1.0])
                 (triangle-batch/->TriangleBatch [10 10
                                                  20 10
                                                  20 20]
                                                 [1 0 0 1
                                                  1 0 0 1
                                                  1 0 0 1])
                 (vector-rectangle/rectangle 30 30 20 20 [0 1 0 1])])

(defn primitive-lists [primitives]
  (map (fn [primitive-group] ((primitive/list-creator (first primitive-group)) primitive-group))
       (partition-by primitive/list-creator primitives)))

(defn create-application [window]
  {:primitive-lists (primitive-lists primitives)})

(comment
(window/start 700 500
                1
                create-application
                update
                identity))