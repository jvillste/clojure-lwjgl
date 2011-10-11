(ns clojure-lwjgl.input
  (:require [clojure-lwjgl.event-queue :as event-queue])
  (:import
   (java.awt.event KeyListener MouseAdapter KeyAdapter WindowAdapter)
   (org.lwjgl.input Mouse Keyboard)))

(defrecord MouseState [left-mouse-button-down
                       middle-mouse-button-down
                       right-mouse-button-down
                       mouse-x
                       mouse-y])
(defn create-initial-mouse-state []
  (MouseState. false
               false
               false
               0
               0))


(defn ^:dynamic get-time [] (System/nanoTime))





;; MOUSE

(defn mouse-button-event-type [lwjgl-event]
  (case [(:mouse-button lwjgl-event) (:mouse-button-state lwjgl-event)]
    [0 true] :left-mouse-button-down
    [1 true] :right-mouse-button-down
    [2 true] :middle-mouse-button-down
    [0 false] :left-mouse-button-up
    [1 false] :right-mouse-button-up
    [2 false] :middle-mouse-button-up
    nil))

(defn update-mouse-state [gui event]
  (update-in gui :mouse-state (fn [mouse-state]
                                (case (:type event)
                                  :mouse-moved (assoc mouse-state
                                                 :mouse-x (:mouse-x event)
                                                 :mouse-y (:mouse-y event))
                                  :left-mouse-button-down (assoc mouse-state :left-mouse-button-down true)
                                  :right-mouse-button-down (assoc mouse-state :right-mouse-button-down true)
                                  :middle-mouse-button-down (assoc mouse-state :middle-mouse-button-down true)
                                  :left-mouse-button-up (assoc mouse-state :left-mouse-button-down false)
                                  :right-mouse-button-up (assoc mouse-state :right-mouse-button-down false)
                                  :middle-mouse-button-up (assoc mouse-state :middle-mouse-button-down false)
                                  mouse-state))))

(defn create-mouse-event [lwjgl-event]
  (cond (or (> (:mouse-delta-x lwjgl-event) 0)
            (> (:mouse-delta-y lwjgl-event) 0))
        {:type :mouse-moved
         :mouse-delta-y (:mouse-delta-y lwjgl-event)
         :mouse-delta-x (:mouse-delta-x lwjgl-event)}

        (not (= 0 (:mouse-wheel-delta lwjgl-event)))
        {:type :mouse-wheel-moved
         :mouse-wheel-delta (:mouse-wheel-delta lwjgl-event)}

        (not (= (:mouse-button lwjgl-event) -1))
        {:type (mouse-button-event-type lwjgl-event)}))

(defn read-lwjgl-mouse-event []
  {:mouse-button (Mouse/getEventButton)
   :mouse-button-state (Mouse/getEventButtonState)
   :mouse-wheel-delta (Mouse/getEventDWheel)
   :mouse-x (Mouse/getEventX)
   :mouse-y (Mouse/getEventY)
   :mouse-delta-x (Mouse/getEventDX)
   :mouse-delta-y (Mouse/getEventDY)
   :time (Mouse/getEventNanoseconds)})

(defn unread-mouse-input-exists? [] (Mouse/next))

(defn create-mouse-events [gui]
  (reduce event-queue/add-event
          gui
          (take-while unread-mouse-input-exists?
                      (repeatedly #(create-mouse-event (read-lwjgl-mouse-event))))))



;; KEYBOARD

(defn read-lwjgl-keyboard-event []
  {:key-state (Keyboard/getEventKeyState)
   :key-code (Keyboard/getEventKey)
   :character (Keyboard/getEventCharacter)})

(defn create-keyboard-event [lwjgl-event]
  {:type (if (:key-state lwjgl-event)
           :key-pressed
           :key-released)
   :key-code (:key-code lwjgl-event)
   :character (:character lwjgl-event)})

(defn update-keyboard-state [gui event]
  (update-in gui :keys-down (fn [keys-down]
                              (case (:type event)
                                :key-pressed (conj keys-down (:key-code event))
                                :key-released (disj keys-down (:key-code event))
                                keys-down))))

(defn unread-keyboard-input-exists? [] (Keyboard/next))

(defn create-keyboard-events [gui]
  (if (unread-keyboard-input-exists?)
    (recur (update-in gui [:event-queue] (fn [event-queue]
                                           (event-queue/add event-queue (create-keyboard-event (read-lwjgl-keyboard-event))))))
    gui))



;; PUBLIC

(defn initialize [gui]
  (-> gui
      (assoc
          :mouse-state (create-initial-mouse-state)
          :keys-down #{})
      (event-queue/add-event-handlers [:update] create-mouse-events)
      (event-queue/add-event-handlers [:update] create-keyboard-events)
      (event-queue/add-event-handlers [:key-pressed
                                       :key-released] update-keyboard-state)
      (event-queue/add-event-handlers [:mouse-moved
                                       :mouse-wheel-moved
                                       :left-mouse-button-down
                                       :right-mouse-button-down
                                       :middle-mouse-button-down
                                       :left-mouse-button-up
                                       :right-mouse-button-up
                                       :middle-mouse-button-up] update-mouse-state)))