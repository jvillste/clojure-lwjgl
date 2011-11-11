(ns clojure-lwjgl.frp.frpgui
  (:require (clojure-lwjgl [window :as window]
                           [input :as input]
                           [visual-list :as visual-list]
                           [text :as text])
            (clojure-lwjgl.frp [event-stream :as event-stream]
                               [behavior :as behavior]))
  (:import [org.lwjgl.opengl GL11]))

(defn text-buffer [keyboard-events]
  (behavior/create (event-stream/filter keyboard-events
                                        #(= (:type %)
                                            :key-pressed))
                   ""
                   (fn [current-value event]
                     (str current-value (:character event)))))

(defn create []
  (let [keyboard-events (event-stream/create)]
    (-> {:window (window/create 200 200)
         :keyboard-events keyboard-events
         :text-buffer (text-buffer keyboard-events)
         :visual-list (-> (visual-list/create)
                          (viual-list/add-visual (assoc (text/create)
                                                   :x 10
                                                   :y 10
                                                   :width 100

                                                   
                                                   :height 50)))})))

(defn handle-input [gui]
  (doseq [event (input/unread-keyboard-events)]
    (event-stream/send-event (:keyboard-events gui)
                             event))
  gui)

(defn update [gui]
  (println @(:current-value (:text-buffer gui)))
  (assoc gui
    :window (window/update (:window gui)
                           30)))

(defn run []
  (let [initial-gui (create)]
    (try
      (loop [gui initial-gui]
        (if (not @(:close-requested (:window gui)))
          (recur (-> gui
                     (handle-input)
                     (update)))
          (window/close (:window gui))))
      (catch Exception e
        (println e)
        (.printStackTrace e)
        (window/close (:window initial-gui))))))

(comment
  (run))
