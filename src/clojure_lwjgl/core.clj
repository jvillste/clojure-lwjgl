(ns clojure-lwjgl.core
  (:require [clojure-lwjgl.text :as text]
            [clojure-lwjgl.component-container :as component-container]
            [clojure-lwjgl.component :as component]
            [clojure-lwjgl.texture :as texture]
            [clojure-lwjgl.window :as window]
            [clojure-lwjgl.buffered-image :as buffered-image])
  (:import [org.lwjgl.opengl GL11]))

(defn initialize []
  (println "initialize"))

(def render (atom (fn [])))

(reset! render
        (fn []

          (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
          (GL11/glLoadIdentity)

          ;;  (GL11/glTranslatef 100 100 0)
          
;;            (GL11/glScalef 3 3 1)
          (let [component-container (-> (component-container/create)
                                        (component-container/add-component (component/free-layout (text/create "Foo ja muuta tekstia")
                                                                                                  10
                                                                                                  200))

                                        (component-container/add-component (component/free-layout (text/create "Foo")
                                                                                                  10
                                                                                                  100))
                                        
                                        (component-container/add-component (component/free-layout (text/create "Foo4")
                                                                                                  100
                                                                                                  100)))]

            (doto component-container
              (component-container/render-components)
              (component-container/draw))


;;            (texture/draw (:texture (:texture-atlas component-container)))
            (component-container/dispose component-container))))

(window/open render initialize)



