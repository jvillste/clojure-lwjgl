(ns clojure-lwjgl.core
  (:require [clojure.pprint :as pprint]
            (clojure-lwjgl [text :as text]
                           [image-list :as image-list]
                           [component :as component]
                           [texture :as texture]
                           [window :as window]
                           [input :as input]
                           [buffer :as buffer]
                           [free-layout :as free-layout]
                           [component-manager :as component-manager]
                           [text-field :as text-field]
                           [buffered-image :as buffered-image]))

  (:import [org.lwjgl.opengl GL11]))

(defn initialize []
  (println "initialize")

  (def component-manager (component-manager/create))

;;  (pprint/pprint component-manager)

  (println "adding component")
  (component-manager/add-component component-manager (text-field/create "Foobar"))
  (println "added component")
  (pprint/pprint component-manager))

(def render (atom nil))

(def handle-input (atom nil))

(def input (input/create handle-input))

(reset! render
        (fn []

          (input/read-input input)

          (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
          (GL11/glLoadIdentity)

          ;;  (GL11/glTranslatef 100 100 0)
          ;;            (GL11/glScalef 3 3 1)

          (component-manager/draw component-manager)))


(reset! handle-input
        (fn [input-state]
          (component-manager/handle-input component-manager
                                          input-state)))

(window/open render initialize)

