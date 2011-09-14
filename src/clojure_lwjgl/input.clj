(ns clojure-lwjgl.input
  (:import
   (java.awt.event KeyListener MouseAdapter KeyAdapter WindowAdapter)))

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

(defn handle-input [input state-changes event]
  (send-off (:input-state input) (fn [input-state]
                                   (let [new-input-state (apply assoc input-state
                                                                (concat state-changes
                                                                        [:last-event event
                                                                         :time (System/nanoTime)]))]
                                     ((:listener input) new-input-state)
                                     new-input-state))))

(defn create-mouse-input-handler [input]
  (proxy [MouseAdapter] []
    (mousePressed  [e] (println "pressed") (case (.getButton e)
                              1 (handle-input input
                                              [:left-mouse-button-down true]
                                              {:type :left-mouse-button-down})

                              2 (handle-input input
                                              [:middle-mouse-button-down true]
                                              {:type :middle-mouse-button-down})

                              3 (handle-input input
                                              [:right-mouse-button-down true]
                                              {:type :right-mouse-button-down})))

    (mouseReleased [e] (case (.getButton e)
                             1 (handle-input input
                                             [:left-mouse-button-down false]
                                             {:type :left-mouse-button-up})

                             2 (handle-input input
                                             [:middle-mouse-button-down false]
                                             {:type :middle-mouse-button-up})

                             3 (handle-input input
                                             [:right-mouse-button-down false]
                                             {:type :right-mouse-button-up})))
    (mouseEntered [e] )
    (mouseExited [e] )
    (mouseMoved [e] (println "mouse moved") (handle-input input
                                  [:mouse-x (.getX e) :mouse-y (.getY e)]
                                  {:type :mouse-moved}))
    (mouseDragged [e] )))

(defn create-keyboard-input-handler [input]
  (proxy [KeyAdapter] []
    (keyPressed [e] (println "key pressed") (handle-input input
                                  [:keys-down (conj (:keys-down @(:input-state input)) (.getKeyCode e))]
                                  {:type :key-pressed
                                   :key-code (.getKeyCode e)
                                   :key-character (.getKeyChar e)}))

    (keyReleased [e] (handle-input input
                                   [:keys-down (disj (:keys-down @(:input-state input)) (.getKeyCode e))]
                                   {:type :key-released
                                    :key-code (.getKeyCode e)
                                    :key-character (.getKeyChar e)}))))


