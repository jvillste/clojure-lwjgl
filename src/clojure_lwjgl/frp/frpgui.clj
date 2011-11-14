(ns clojure-lwjgl.frp.frpgui
  (:require (clojure-lwjgl [window :as window]
                           [input :as input]
                           [visual-list :as visual-list]
                           [text :as text]
                           [free-layout :as free-layout])
            (clojure-lwjgl.frp [event-stream :as event-stream]
                               [behavior :as behavior]))
  (:import [org.lwjgl.opengl GL11]))



(defn text-buffer [key-pressed-events]
  (behavior/create key-pressed-events
                   ""
                   (fn [current-value event]
                     (str current-value (:character event)))))


(defn text-buffer-list [key-pressed-events]
  (behavior/create key-pressed-events
                   {:active-buffer-index 0
                    :buffers [(text-buffer key-pressed-events)]}
                   (fn [current-text-buffers event]
                     (if (= (:key-code event)
                            input/enter)
                       (conj current-text-buffers (text-buffer key-pressed-events))
                       current-text-buffers))))

(defn active-text-buffer [key-pressed-events text-buffer-list]
  (behavior/create key-pressed-events
                   0
                   (fn [currently-active-text-buffer event]
                     (if (= (:key-code event)
                            input/down)
                       (max (count @(:current-value text-buffer-list))
                            (+ 1
                               currently-active-text-buffer))
                       currently-active-text-buffer))))

(defn create-view-visuals [text-behavior1 text-behavior2]
  ;; (behavior/create-from-behavior text-behavior1
  ;;                                (fn [old-visuals new-text]
  ;;                                  [(free-layout/layout 10
  ;;                                                       10
  ;;                                                       (text/create new-text))
  ;;                                   (free-layout/layout 10
  ;;                                                       40
  ;;                                                       currently-active-text-buffer
  ;;                                                       (text/create (str "second: " new-text)))]))
  )

(defn create-visual-list [visuals-behavior]
  ;; (behavior/create-from-behavior visual-behavior
  ;;                                (fn [old-visual-list new-visual]
  ;;                                  ))
  )

(defn create []
  (let [keyboard-events (event-stream/create)]
    (-> {:window (window/create 200 200)
         :keyboard-events keyboard-events
         :text-buffer-list (text-buffer-list (event-stream/filter keyboard-events
                                                                  #(= (:type %)
                                                                      :key-pressed)))
         ;; :visual-list (-> (visual-list/create)
         ;;                  (visual-list/add-visual (create-view-visuals text-buffer)))
         })))

(defn handle-input [gui]
  (doseq [event (input/unread-keyboard-events)]
    (println event)
    (event-stream/send-event (:keyboard-events gui)
                             event))
  gui)

(defn update [gui]
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






