(ns clojure-lwjgl.core
  (:require (clojure-lwjgl [text :as text]
                           [image-list :as image-list]
                           [component :as component]
                           [texture :as texture]
                           [window :as window]
                           [input :as input]
                           [buffer :as buffer]
                           [free-layout :as free-layout]
                           [text-field :as text-field]
                           [buffered-image :as buffered-image]))

  (:import [org.lwjgl.opengl GL11]))

(defn initialize []
  (println "initialize"))

(def render (atom nil))

(def handle-input (atom nil))

(def input (input/create handle-input))

(def component-manager (component-manager/create))

(component-manager/add-component component-manager
                                 (free-layout (text-field/create "Foobar")
                                              10
                                              10))
(component-manager/load)

(reset! render
        (fn []

          (input/read-input input)

          (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
          (GL11/glLoadIdentity)
          
          ;;  (GL11/glTranslatef 100 100 0)
          ;;            (GL11/glScalef 3 3 1)

          (component-manger/draw component-manager)))


(reset! handle-input
        (fn [input-state]
          (component-manager/handle-input component-manager
                                          input-state)))

(window/open render initialize)

