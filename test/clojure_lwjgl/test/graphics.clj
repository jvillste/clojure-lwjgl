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
                           [triangle-list :as triangle-list]
                           [primitive :as primitive]))
  (:import [org.lwjgl.opengl GL11 GL20 ARBVertexBufferObject ARBVertexProgram ARBVertexShader]
           [java.awt Color Font FontMetrics RenderingHints]))

(defn draw-view-part [application view-part]
  (doseq [primitive-list (get-in application [:view-part-primitive-lists view-part])]
    (primitive-list/draw primitive-list)))

(defn render [application]
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)

  (doseq [view-part (:view application)]
    (draw-view-part application view-part))

  application)

(defn primitive-lists-for-primitives [primitives]
  (map (fn [primitive-group] ((primitive/list-creator (first primitive-group)) primitive-group))
       (partition-by primitive/list-creator primitives)))

(defn update-view-part [application view-part]
  (doseq [primitive-list (get-in application [:view-part-primitive-lists view-part])]
    (primitive-list/delete primitive-list))

  (assoc-in application [:view-part-primitive-lists view-part] (primitive-lists-for-primitives ((get-in application [:view-parts view-part]) application))))

(defn update-view [application]
  (reduce update-view-part application (:view application)))

(defn background [{:keys [width height]}]
  [(vector-rectangle/rectangle 20 20
                               (- width 40)
                               (- height 40)
                               [0 1 0 1])])

(defn foreground [{:keys [count width height]}]
  [(text-list/->Text (/ width 2)
                     (/ height 2)
                     (str count)
                     (font/create "LiberationSans-Regular.ttf" 17)
                     [1.0 0.0 0.0 1.0])])

(defn update [application]
  (-> application
      (update-in [:count] inc)
      (update-view-part :foreground)
      (render)))

(defn create-application [window]
  (-> {:view-parts {:background background
                    :foreground foreground}
       :view [:background
              :foreground]
       :view-part-primitive-lists {}
       :width @(:width window)
       :height @(:height window)
       :count 0}
      (update-view)))

(comment
(window/start 700 500
                5
                create-application
                update
                identity
                (fn [state width height]
                  (-> state
                      (assoc
                          :width width
                          :height height)
                      (update-view)))))