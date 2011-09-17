(ns clojure-lwjgl.input
  (:import
   (java.awt.event KeyListener MouseAdapter KeyAdapter WindowAdapter)
   (org.lwjgl.input Mouse Keyboard)))

(defrecord Input [listener input-state])

(defn create [listener]
  (Input. listener
          (agent {:time (System/nanoTime)
                  :keys-down #{}
                  :left-mouse-button-down false
                  :middle-mouse-button-down false
                  :right-mouse-button-down false
                  :mouse-x 0
                  :mouse-y 0})))

(defn- handle-input [input state-changes event]
  (send-off (:input-state input) (fn [input-state]
                                   (let [new-input-state (apply assoc input-state
                                                                (concat state-changes
                                                                        [:last-event event
                                                                         :time (System/nanoTime)]))]
                                     (@(:listener input) new-input-state)
                                     new-input-state))))

(defn read-input [input]
  (while (Mouse/next)
    (case [(Mouse/getEventButton) (Mouse/getEventButtonState)]
      [0 true] (handle-input input
                             [:left-mouse-button-down true]
                             {:type :left-mouse-button-down})

      [1 true] (handle-input input
                             [:right-mouse-button-down true]
                             {:type :right-mouse-button-down})

      [2 true] (handle-input input
                             [:middle-mouse-button-down true]
                             {:type :middle-mouse-button-down})

      [0 false] (handle-input input
                              [:left-mouse-button-down false]
                              {:type :left-mouse-button-up})

      [1 false] (handle-input input
                              [:right-mouse-button-down false]
                              {:type :right-mouse-button-up})

      [2 false] (handle-input input
                              [:middle-mouse-button-down false]
                              {:type :middle-mouse-button-up})
      nil)

    (when (or (> (Mouse/getEventDX) 0)
              (> (Mouse/getEventDY) 0))
      (handle-input input
                    [:mouse-x (Mouse/getEventX) :mouse-y (Mouse/getEventY)
                     :mouse-dx (Mouse/getEventDX) :mouse-dy (Mouse/getEventDY)]
                    {:type :mouse-moved}))

    (when (not (= (Mouse/getEventDWheel) 0))
      (handle-input input
                    [:mouse-wheel-delta (Mouse/getEventDWheel)]
                    {:type :mouse-wheel-moved})))

  (while (Keyboard/next)
    (handle-input input
                  [:keys-down (if (Keyboard/getEventKeyState)
                                (conj (:keys-down @(:input-state input)) (Keyboard/getEventKey))
                                (disj (:keys-down @(:input-state input)) (Keyboard/getEventKey)))]
                  {:type (if (Keyboard/getEventKeyState) :key-pressed :key-released)
                   :key-code (Keyboard/getEventKey)
                   :key-character (Keyboard/getEventCharacter)})))

