(ns clojure-lwjgl.gui
  (:require (clojure-lwjgl [window :as window]
                           [input :as input]
                           [event-queue :as event-queue]
                           [visual-list :as visual-list]
                           [text-field :as text-field])
            (clojure-lwjgl.frp [event-stream :as event-stream]))
  (:import [org.lwjgl.opengl GL11]))


(defn initialize-gl [gui]
  (GL11/glClearColor 1 1 1 0)
  (GL11/glEnable GL11/GL_BLEND)
  (GL11/glEnable GL11/GL_TEXTURE_2D)
  (GL11/glColorMask true, true, true, true)
  (GL11/glBlendFunc GL11/GL_SRC_ALPHA GL11/GL_ONE_MINUS_SRC_ALPHA)
  gui)

(defn open-view [gui]
  (assoc gui
    :visual-list (visual-list/add-visual (:visual-list gui)
                                         (text-field/create "Foobar"))))

(defn create []
  (-> {}
      (event-queue/initialize)
      (window/initialize)
      (input/initialize)
      (event-stream/initialize)
      (visual-list/initialize)
      (initialize-gl)
      (open-view)))

(defn clear [gui]
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
  (GL11/glLoadIdentity)
  ;;(GL11/glTranslatef 100 100 0)
  ;;(GL11/glScalef 3 3 1)
  gui)

(defn create-draw-event [gui]
  (event-queue/add-event gui {:type :draw}))

(defn create-update-event [gui]
  (event-queue/add-event gui {:type :update}))



(defn run []
  (let [initial-gui (create)]
    (try
      (loop [gui initial-gui]
        (if (not @(:close-requested (:window gui)))
          (recur (-> gui
                     (create-update-event)
                     (event-queue/call-event-handlers)
                     (clear)
                     (create-draw-event)
                     (event-queue/call-event-handlers)))
          (window/close (:window gui))))
      (catch Exception e
        (println e)
        (.printStackTrace e)
        (window/close (:window initial-gui))))))

