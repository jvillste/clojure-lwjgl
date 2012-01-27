(ns clojure-lwjgl.applications.traditional
  (:refer-clojure :exclude (load))
  (:require (clojure-lwjgl [window :as window]
                           [visual-list :as visual-list]
                           [input :as input]
                           [text :as text]
                           [free-layout :as free-layout]))
  (:import [org.lwjgl.opengl GL11]))

(defn create-gui [window]
  {:window window
   :visual-list (visual-list/create)
   :mouse-state (input/create-initial-mouse-state)})

(defn add-content [gui]
  (assoc gui
    :visual-list (visual-list/add-visual (:visual-list gui)
                                         (free-layout/layout 10
                                                             10
                                                             (text/create "Foo")))))

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
  (visual-list/draw (:visual-list gui))
  gui)

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


