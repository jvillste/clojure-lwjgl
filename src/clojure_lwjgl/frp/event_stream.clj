(ns clojure-lwjgl.frp.event-stream
  (:require (clojure-lwjgl [input :as input])))

(defrecord EventStream [sources sinks update])

(defn add-sink [event-stream sink]
  (swap! (:sinks event-stream)
         #(conj % sink)))

(defn create
  ([] (EventStream. (atom [])
                    (atom [])
                    (fn [event] event)))
  ([source update]
     (let [event-stream  (EventStream. (atom [source])
                                       (atom [])
                                       update)]
       (add-sink source event-stream)
       event-stream)))

(defn send-event [event-stream event]
  (let [processed-event ((:update event-stream) event)]
    (when processed-event
      (doseq [sink @(:sinks event-stream)]
        (send-event sink processed-event)))))