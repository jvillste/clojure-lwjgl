(ns clojure-lwjgl.frp.frpgui
  (:require (clojure-lwjgl [window :as window]
                           [input :as input]
                           [visual-list :as visual-list]
                           [text :as text]
                           [free-layout :as free-layout])
            (clojure-lwjgl.frp [event-stream :as event-stream]
                               [behavior :as behavior]))
  (:import [org.lwjgl.opengl GL11]))

(defn create-text-buffer [keyboard-events]
  (behavior/create (event-stream/filter keyboard-events
                                        #(= (:type %)
                                            :key-pressed))
                   ""
                   (fn [current-value event]
                     (str current-value (:character event)))))


(defn create-view-visuals [text-behavior1 text-behavior2]
  (behavior/create-from-behavior text-behavior
                                 (fn [old-visuals new-text]
                                   [(free-layout/layout 10
                                                        10
                                                        (text/create new-text))
                                    (free-layout/layout 10
                                                        40
                                                        (text/create (str "second: " new-text)))])))

(defn create-visual-list [visuals-behavior]
  (behavior/create-from-behavior visual-behavior
                                 (fn [old-visual-list new-visual]
                                   )))

(defn create []
  (let [keyboard-events (event-stream/create)
        text-buffer1 (create-text-buffer keyboard-events)
        text-buffer2 (create-text-buffer keyboard-events)]
    (-> {:window (window/create 200 200)
         :keyboard-events keyboard-events
         :text-buffer text-buffer
         :visual-list (create-visual-list ) (-> (visual-list/create)
                          (visual-list/add-visual (create-view-visual text-buffer)))})))

(defn handle-input [gui]
  (doseq [event (input/unread-keyboard-events)]
    (event-stream/send-event (:keyboard-events gui)
                             event))
  gui)

(defn update [gui]
  (println @(:current-value (:text-buffer gui)))
  (assoc gui
    :window (window/update (:window gui)
                           5)))

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
