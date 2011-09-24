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

(defrecord Gui [input window component-manager])

(def input (input/create handle-input))

(defn render
  (fn []

    (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
    (GL11/glLoadIdentity)

    ;;  (GL11/glTranslatef 100 100 0)
    ;;            (GL11/glScalef 3 3 1)

    ;;  (component-manager/draw component-manager)
    ))


(reset! handle-input
        (fn [input-state]
          (component-manager/handle-input component-manager
                                          input-state)))

(def window (window/create)) 

(try
  (initialize)
  (catch Exception e
    (println "initializer failed")
    (.printStackTrace e)

    (reset! window/close-requested true)))


(while (not @window/close-requested)
  (try
    
    (window/update )
    (input/read-input input)
    (render)
    
    (catch Exception e
      (println e)
      (.printStackTrace e))))

(window/close window)