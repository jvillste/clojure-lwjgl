(ns clojure-lwjgl.frp.signal
  (:require (clojure-lwjgl.frp [event-stream :as event-stream])))

(defrecord Signal [changes current-value])


(defn hold [source initial-value]
  (let [current-value (atom initial-value)
        changes (event-stream/map source
                                  (fn [new-value]
                                    (if (not (= new-value @current-value))
                                      (do (reset! current-value
                                                  new-value)
                                          new-value)
                                      nil)))]

    (Signal. changes
               current-value)))

(defn fold [source initial-value function]
  (let [current-value (atom initial-value)
        changes (event-stream/create source
                                     (fn [event]
                                       (let [new-value (function @current-value event)]
                                         (if (not (= new-value @current-value))
                                           (do (reset! current-value
                                                       new-value)
                                               new-value)
                                           nil))))]

    (Signal. changes
               current-value)))

(defn changes [signal]
  (:changes signal))

(defn value [signal]
  @(:current-value signal))

(defn create [initial-value]
  (hold (event-stream/create)
        initial-value))

(defn set [signal new-value]
  (event-stream/send-event (changes signal)
                           new-value))

(defn map [source-signal function]
  (hold (event-stream/map (:changes source-signal)
                          function)
        (function (value source-signal))))


(defmethod print-method Signal [signal writer]
  (print-method (str "(Signal " (value signal) ")") writer))