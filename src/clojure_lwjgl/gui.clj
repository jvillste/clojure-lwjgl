(ns clojure-lwjgl.gui
  (:require (clojure-lwjgl [window :as window]
                           [input :as input]
                           [event-queue :as event-queue]
                           [component-manager :as component-manager]
                           [text-field :as text-field]))
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
    :clojure-lwjgl.component-manager/component-manager (component-manager/add-component (:component-manager/component-manager gui)
                                                                                        (text-field/create "Foobar"))))

(defn create []
  (-> {:updaters #{}
       :event-handlers #{}}
      (window/initialize)
      (input/initialize)
      (component-manager/initialize)
      (initialize-gl)
      (open-view)))

(defn clear [gui]
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
  (GL11/glLoadIdentity)
  ;;(GL11/glTranslatef 100 100 0)
  ;;(GL11/glScalef 3 3 1)
  gui)

(defn call-drawers [gui]
  (reduce (fn [new-gui drawer] (drawer new-gui))
          gui
          (:drawers gui)))

(defn call-updaters [gui]
  (reduce (fn [new-gui updater] (updater new-gui))
          gui
          (:updaters gui)))

(defn call-event-handlers-for-single-event [gui event]
  (reduce (fn [gui event-handler] (event-handler gui event))
          gui
          (:event-handlers gui)))

(defn call-event-handlers [gui]
  (if (not (empty? (:event-queue gui)))
    (let [event (event-queue/oldest (:event-queue gui))
          new-gui (call-event-handlers-for-single-event gui event)]
      (recur (assoc new-gui
               :event-queue (event-queue/remove-oldest (:event-queue new-gui)))))
    gui))

(defn run []
  (let [initial-gui (create)]
    (try
      (loop [gui initial-gui]
        (if (not @(:close-requested (:window gui)))
          (recur (-> gui
                     ;;                     (create-new-frame-event)
                     (call-updaters)
                     (call-event-handlers)
                     (clear)
                     ;;                     (create-redraw-event)
                     (call-drawers)))
          (window/close (:window gui))))
      (catch Exception e
        (println e)
        (.printStackTrace e)
        (window/close (:window initial-gui))))))

