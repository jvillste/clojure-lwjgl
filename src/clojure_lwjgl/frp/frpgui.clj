(ns clojure-lwjgl.frp.frpgui
  (:require (clojure-lwjgl [window :as window]
                           [input :as input]
                           [visual-list :as visual-list])
            (clojure-lwjgl.frp [event-stream :as event-stream]
                               [behavior :as behavior]))
  (:import [org.lwjgl.opengl GL11]))


(defn text-buffer [keyboard-events]
  (behavior/create keyboard-events
                   ""
                   (fn [current-value event]
                     (str current-value (:character event)))))

(defn create []
  (let [keyboard-events (event-stream/create)]
    (-> {:window (window/create 200 200)
         :keyboard-events keyboard-events
         :text-buffer (text-buffer keyboard-events)})))

(defn handle-input [gui]
  (println (:current-value (:text-buffer gui)
  gui)

(defn update [gui]
  (assoc gui
    :window (window/update (:window gui)
                           2)))

(defn run []
  (let [initial-gui (create)]
    (try
      (loop [gui initial-gui]
        (if (not @(:close-requested (:window gui)))
          (recur (-> gui
                     (update)
                     (handle-input)))
          (window/close (:window gui))))
      (catch Exception e
        (println e)
        (.printStackTrace e)
        (window/close (:window initial-gui))))))

(comment
(run))