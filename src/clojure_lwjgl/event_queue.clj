(ns clojure-lwjgl.event-queue)

(defn create [] (sorted-map))

(defn initialize [gui]
  (assoc gui
    :event-queue (create)
    :event-handlers {}))

(defn oldest [event-queue]
  (first (second (first event-queue))))

(defn remove-oldest [event-queue]
  (let [[first-time-stamp events] (first event-queue)]
    (if (= 1 (count events))
      (dissoc event-queue first-time-stamp)
      (assoc-in event-queue [first-time-stamp] (rest events)))))

(defn add [event-queue event]
  (update-in event-queue
             [(:time event)]
             (fn [events]
               (if events
                 (conj events
                       event)
                 (vector event)))))

(defn add-event [gui event]
  (assoc gui
    :event-queue (add (:event-queue gui)
                      (assoc event
                        :time (System/nanoTime)))))



(defn add-event-handler [gui type event-handler]
  (assoc gui
    :event-handlers (update-in (:event-handlers gui)
                               [type]
                               (fn [event-handlers]
                                 (if event-handlers
                                   (conj event-handlers
                                         event-handler)
                                   (vector event-handler))))))

(defn remove-event-handler [gui type event-handler]
  (assoc gui
    :event-handlers (let [event-handler-vector ((:event-handlers gui) type)]
                      (if (= 1 (count event-handler-vector))
                        (dissoc (:event-handlers gui) type)
                        (assoc-in (:event-handlers gui) [type] (apply vector
                                                                      (filter (fn [event-handler-candidate]
                                                                                (not (= event-handler-candidate
                                                                                        event-handler)))
                                                                              event-handler-vector)))))))

(defn call-event-handlers-for-single-event [gui event]
  (reduce (fn [gui event-handler] (event-handler gui event))
          gui
          (:event-handlers gui)))



(defn call-event-handlers [gui]
  (if (not (empty? (:event-queue gui)))
    (let [event (oldest (:event-queue gui))
          new-gui (call-event-handlers-for-single-event gui event)]
      (recur (assoc new-gui
               :event-queue (remove-oldest (:event-queue new-gui)))))
    gui))
