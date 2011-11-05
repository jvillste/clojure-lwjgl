(ns clojure-lwjgl.frp.signal
  (:require (clojure-lwjgl.frp [event-stream :as event-stream])))

(defrecord Signal [sources sinks update current-value])

(defn create [source initial-value]
  (let [current-value (atom initial-value)
        event-stream (event-stream/create source
                                          (fn [event]
                                            (if (not (= event current-value))
                                              (do (reset! current-value
                                                          event)
                                                  event)
                                              nil)))]
    (Signal. (:sources event-stream)
             (:sinks event-stream)
             (:update event-stream)
             current-value)))

