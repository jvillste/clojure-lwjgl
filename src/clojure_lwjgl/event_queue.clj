(ns clojure-lwjgl.event-queue)

(defn create [] (sorted-map))

(defn add [event-queue event]
  (update-in event-queue
             [(:time event)]
             (fn [events]
               (if events
                 (conj events
                       event)
                 (vector event)))))

(defn oldest [event-queue]
  (first (second (first event-queue))))

(defn remove-oldest [event-queue]
  (let [[first-time-stamp events] (first event-queue)]
    (if (= 1 (count events))
      (dissoc event-queue first-time-stamp)
      (assoc-in event-queue [first-time-stamp] (rest events)))))
