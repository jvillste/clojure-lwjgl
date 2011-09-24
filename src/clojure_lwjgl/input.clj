(ns clojure-lwjgl.input
  (:import
   (java.awt.event KeyListener MouseAdapter KeyAdapter WindowAdapter)
   (org.lwjgl.input Mouse Keyboard)))

(defrecord InputState [time
                       keys-down
                       left-mouse-button-down
                       middle-mouse-button-down
                       right-mouse-button-down
                       mouse-x
                       mouse-y
                       last-event])

(defn create-initial-input-state []
  (InputState. (System/nanoTime)
               #{}
               false
               false
               false
               0
               0
               {}))

(defn ^:dynamic get-time [] (System/nanoTime))

(defn apply-state-changes [state-changes input-states]
  (conj input-states (apply assoc (last input-states)
                            (concat state-changes
                                    [:time (get-time)]))))


(defn sort-input-states [input-states]
  (sort-by :time input-states))



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

(defn mouse-button-keyword [lwjgl-event]
  (nth [:left-mouse-button-down
        :right-mouse-button-down
        :middle-mouse-button-down]
       (:mouse-button lwjgl-event)))

(defn get-mouse-state-changes-for-event [lwjgl-event]
  (cond (or (> (:mouse-delta-x lwjgl-event) 0)
            (> (:mouse-delta-y lwjgl-event) 0))
        [:mouse-x (:mouse-x lwjgl-event)
         :mouse-y (:mouse-y lwjgl-event)
         :last-event {:type :mouse-moved}]

        (not (= 0 (:mouse-wheel-delta lwjgl-event)))
        [:mouse-wheel-delta (:mouse-wheel-delta lwjgl-event)
         :last-event {:type :mouse-wheel-moved}]

        (not (= (:mouse-button lwjgl-event) -1))
        [(mouse-button-keyword lwjgl-event) (:mouse-button-state lwjgl-event)
         :last-event {:type (mouse-button-event-type lwjgl-event)}]))

(defn read-lwjgl-mouse-event []
  {:mouse-button (Mouse/getEventButton)
   :mouse-button-state (Mouse/getEventButtonState)
   :mouse-wheel-delta (Mouse/getEventDWheel)
   :mouse-x (Mouse/getEventX)
   :mouse-y (Mouse/getEventY)
   :mouse-delta-x (Mouse/getEventDX)
   :mouse-delta-y (Mouse/getEventDY)})

(defn unread-mouse-input-exists? [] (Mouse/next))

(defn read-mouse-input [input-states]
  (if (unread-mouse-input-exists?)
      (recur (-> (read-lwjgl-mouse-event)
                 (get-mouse-state-changes-for-event)
                 (apply-state-changes input-states)))
      input-states))


;; KEYBOARD

(defn read-lwjgl-keyboard-event []
  {:key-state (Keyboard/getEventKeyState)
   :key-code (Keyboard/getEventKey)
   :character (Keyboard/getEventCharacter)})

(defn unread-keyboard-input-exists? [] (Keyboard/next))


(defn get-keyboard-state-changes-for-event [lwjgl-event old-input-state]
  [:keys-down (if (:key-state lwjgl-event)
                (conj (:keys-down old-input-state) (:key-code lwjgl-event))
                (disj (:keys-down old-input-state) (:key-code lwjgl-event)))
   :last-event {:type (if (:key-state lwjgl-event)
                        :key-pressed
                        :key-released)
                :key-code (:key-code lwjgl-event)
                :character (:character lwjgl-event)}])

(defn read-keyboard-input [input-states]
  (if (unread-keyboard-input-exists?)
      (recur (-> (read-lwjgl-keyboard-event)
                 (get-keyboard-state-changes-for-event (last input-states))
                 (apply-state-changes input-states)))
      input-states))


;; PUBLIC

(defn read-input [initial-input-state]
  (-> [initial-input-state]
      (read-mouse-input)
      (read-keyboard-input)
      (sort-input-states)))

