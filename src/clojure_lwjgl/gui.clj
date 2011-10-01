(ns clojure-lwjgl.gui
  (:require (clojure-lwjgl [window :as window]
                           [input :as input]
                           [component-manager :as component-manager]
                           [text-field :as text-field]))
  (:import [org.lwjgl.opengl GL11]))

(defrecord Gui [window
                input-state
                component-manager])

(defn initialize-gl []
  (GL11/glClearColor 1 1 1 0)
  (GL11/glEnable GL11/GL_BLEND)
  (GL11/glEnable GL11/GL_TEXTURE_2D)
  (GL11/glColorMask true, true, true, true)
  (GL11/glBlendFunc GL11/GL_SRC_ALPHA GL11/GL_ONE_MINUS_SRC_ALPHA))


(defn create-window []
  (let [window (window/create)]
    (initialize-gl)
    window))

(defn create []
  (Gui. (create-window)
        (input/create-initial-input-state)
        (-> (component-manager/create)
            (component-manager/add-component (text-field/create "Foobar")))))


(defn render [gui]
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
  (GL11/glLoadIdentity)
  ;;(GL11/glTranslatef 100 100 0)
  ;;(GL11/glScalef 3 3 1)

  (component-manager/draw (:component-manager gui))
  gui)

(defn read-input [gui]
  (assoc gui :input (input/read-input (:input-state gui))))

(defn handle-input [gui]
  (let [all-input-states (input/read-input (:input-state gui))]
    (assoc gui
      :component-manager (loop [input-states (seq (next all-input-states))
                                component-manager (:component-manager gui)]
                           (if input-states
                             (recur (next input-states)
                                    (component-manager/handle-input component-manager (first input-states)))
                             component-manager))
      :input-state (last all-input-states))))

(defn update-window [gui]
  (assoc gui :window (window/update (:window gui))))

(defn run []
  (let [initial-gui (create)]
    (try
      (loop [gui initial-gui]
        (if (not @(:close-requested (:window gui)))
          (recur (-> gui
                     (update-window)
                     (handle-input)
                     (render)))
          (window/close (:window gui))))
      (catch Exception e
        (println e)
        (.printStackTrace e)
        (window/close (:window initial-gui))))))

