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

(defn create-new-input-state [previous-input-state sate-changes]
  (apply assoc previous-input-state
         (concat state-changes
                 [:time (System/nanoTime)])))

(defn- handle-input [input-states state-changes event]
  (conj input-states (create-new-state (first input-states)
                                       state-changes
                                       event)))

(defn entries-to-list [map keys]
  (flatten (seq (select-keys map keys))))

(defn get-mouse-button-state-changes [lwjgl-event]
  (let [event-type (case [(:mouse-button lwjgl-event) (:mouse-button-state lwjgl-event)]
                     [0 true] :left-mouse-button-down
                     [1 true] :right-mouse-button-down
                     [2 true] :middle-mouse-button-down
                     [0 false] :left-mouse-button-up
                     [1 false] :right-mouse-button-up
                     [2 false] :middle-mouse-button-up
                     nil)
        state-key (nth [:left-mouse-button-down
                        :right-mouse-button-down
                        :middle-mouse-button-down]
                       (:mouse-button lwjgl-event))]
    [state-key (:mouse-button-state lwjgl-event)
     :last-event {:type event-type}]))



(defn get-mouse-state-chages [lwjgl-event]
  (let [lwjgl-event (read-lwjgl-mouse-event)]
    (cond (or (> (:mouse-delta-x event) 0)
              (> (:mouse-delta-y event) 0))
          (concat (entries-to-list lwjgl-event [:mouse-x :mouse-y])
                  [:last-event {:type :mouse-moved
                                (entries-to-list lwjgl-event [:mouse-x :mouse-y])}])

          (not (= 0 (:mouse-wheel-delta event)))
          (concat (entries-to-list lwjgl-event [:mouse-wheel-delta])
                  [:last-event {:type :mouse-wheel-moved}])

          (not (= (:mouse-button lwjgl-event) -1))
          (get-mouse-button-state-changes lwjgl-event))))

(defn read-lwjgl-mouse-event []
  {
   :mouse-button (Mouse/getEventButton)
   :mouse-button-state (Mouse/getEventButtonState)
   :mouse-wheel-delta (Mouse/getEventDWheel)
   :mouse-x (Mouse/getEventX)
   :mouse-y (Mouse/getEventY)
   :mouse-delta-x (Mouse/getEventDX)
   :mouse-delta-y (Mouse/getEventDY)
   })

(defn read-input [input]
  (loop [new-input input]
    (if (Mouse/next)
      (recur (let [lwjgl-event (get-lwjgl-event)]

               (case (:type lwjgl-event)
                 :mouse-button (handle-mouse-button-press input lwjgl-event)
                 :mouse-move (handle-mouse-move input lwjgl-event)
                 :mouse-wheel (handle-mouse-wheel input lwjgl-event))
               new-input))
      new-input))


  (while (Keyboard/next)
    (handle-input input
                  [:keys-down (if (Keyboard/getEventKeyState)
                                (conj (:keys-down (:current-state input)) (Keyboard/getEventKey))
                                (disj (:keys-down (:current-state input)) (Keyboard/getEventKey)))]
                  {:type (if (Keyboard/getEventKeyState) :key-pressed :key-released)
                   :key-code (Keyboard/getEventKey)
                   :key-character (Keyboard/getEventCharacter)})))

