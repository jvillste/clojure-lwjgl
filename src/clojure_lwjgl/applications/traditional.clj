(ns clojure-lwjgl.applications.traditional
  (:refer-clojure :exclude (load))
  (:require (clojure-lwjgl [window :as window]
                           [visual-list :as visual-list]
                           [image-list :as image-list]
                           [input :as input]
                           [group :as group]
                           [text :as text]
                           [free-layout :as free-layout])
            [clojure.zip :as zip])

  (:use midje.sweet)
  (:import [org.lwjgl.opengl GL11]))



;; miten välitetään component-manageri funktioille joilla lisätään komponentteja?
;; thread local binding
;; currytään bufferi funktiolle ennen kuin funktio annetaan sille funktiolle joka sitä kutsuu
;; state monad

(unfinished)


(defn view [model]
  )

(defn create-gui [window]
  (let [label (text/create "Foo")]
    {:window window
     :label label
     :image-list (-> (image-list/create)
                     (image-list/add-image :label
                                           10
                                           10
                                           (text/width label)
                                           (text/height label)))}))

(defn update-window [gui]
  (assoc gui :window (window/update (:window gui)
                                    30)))

(defn clear [gui]
  (GL11/glClearColor 1 1 1 0)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
  (GL11/glMatrixMode GL11/GL_MODELVIEW)
  (GL11/glLoadIdentity)
  gui)

(defn render [gui]
  (image-list/draw (:image-list gui))
  gui)

(defn handle-input [gui]
  (assoc gui
    :label ))

(defn update-view [gui]

  gui )

(defn update [gui]
  (-> gui
      (clear)
      (render)
      (update-window)))

(comment
  (let [window (window/create 500 500)]
    (try
      (let [initial-gui (-> (create-gui window)
                            (add-content))]
        (loop [gui initial-gui]
          (if (not @(:close-requested (:window gui)))
            (recur (update gui))
            (window/close window))))

      (catch Exception e
        (println e)
        (.printStackTrace e)
        (window/close window)))))


