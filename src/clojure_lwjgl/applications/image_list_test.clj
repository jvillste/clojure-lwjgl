(ns clojure-lwjgl.applications.image-list-test
  (:refer-clojure :exclude (load))
  (:require (clojure-lwjgl [window :as window]
                           [visual-list :as visual-list]
                           [visual :as visual]
                           [image-list :as image-list]
                           [input :as input]
                           [group :as group]
                           [text :as text]
                           [free-layout :as free-layout]
                           [layoutable :as layoutable]
                           [rectangle :as rectangle]
                           [fps :as fps])
            [clojure.zip :as zip]
            [clojure.contrib.dataflow :as dataflow])

  (:use midje.sweet)
  (:import [org.lwjgl.opengl GL11]))


(defn render-visual [visual image-list]
  (visual/render visual (image-list/get-graphics image-list (:id visual))))

(defn add-visual-to-image-list [image-list visual x y]
  (let [image-list (image-list/add-image image-list
                                         (:id visual)
                                         x
                                         y
                                         (layoutable/preferred-width visual)
                                         (layoutable/preferred-height visual))]

    (render-visual visual image-list)

    image-list))

(defn generate-id [] (rand-int 100000000))

(defn add-id [visual]
  (assoc visual
    :id (generate-id)))

(defn create-image-list [x y]
  (-> (image-list/create)
      (add-visual-to-image-list (add-id (rectangle/create {:red 0 :green 0 :blue 1 :alpha 1}
                                                          10
                                                          10
                                                          0))

                                x
                                y)))



(defn create-gui [window]
  (-> {:window window
       :image-lists (take 2 (repeatedly #(create-image-list (rand-int 200) (rand-int 200))))}
      (fps/initialize)))

(defn update-window [gui]
  (assoc gui :window (window/update (:window gui)
                                    60)))

(defn clear [gui]
  (let [scale 1]
    (GL11/glClearColor 1 1 1 0)
    (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
    (GL11/glMatrixMode GL11/GL_MODELVIEW)
    (GL11/glLoadIdentity)
    (GL11/glScalef scale (- scale) 1)
    (GL11/glTranslatef 0 (- (* (/ 1 scale) @(:height (:window gui)))) 0))

  gui)

(defmacro profile [& body]
  `(let [start-time# (System/nanoTime)
         value# (do ~@body)]
     (println (/ (- (System/nanoTime) start-time#)
                 1E9))
     value#))

(defn render [gui]
  (-> gui
      (fps/print)
      (assoc :image-lists (doall (map image-list/draw (:image-lists gui))))))

(defn update [gui]
  (-> gui
      (clear)
      (render)
      (update-window)))

(comment
(let [window (window/create 500 500)]
    (try
      (let [initial-gui (create-gui window)]
        (loop [gui initial-gui]
          (if (not @(:close-requested (:window gui)))
            (recur (update gui))
            (window/close window))))

      (catch Exception e
        (println e)
        (.printStackTrace e)
        (window/close window)))))

