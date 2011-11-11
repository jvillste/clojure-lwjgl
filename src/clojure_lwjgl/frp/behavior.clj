(ns clojure-lwjgl.frp.behavior
  (:require (clojure-lwjgl.frp [event-stream :as event-stream])))

(defrecord Behavior [changes current-value])

(defn create [source initial-value function]
  (let [current-value (atom initial-value)
        changes (event-stream/create source
                                     (fn [event]
                                       (let [new-value (function @current-value event)]
                                         (if (not (= new-value @current-value))
                                           (do (reset! current-value
                                                       new-value)
                                               new-value)
                                           nil))))]
    
    (Behavior. changes
               current-value)))


(defn create-from-behavior [source-behavior function]
  (create (:changes source-behavior)
          (function @(:current-value source-behavior))
          function))