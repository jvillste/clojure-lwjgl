(ns clojure-lwjgl.applications.traditional)




(defn run []
  (let [initial-gui (create)]
    (try
      (loop [gui initial-gui]
        (if (not @(:close-requested (:window gui)))
          (recur (-> gui
                     (create-update-event)
                     (event-queue/call-event-handlers)
                     (clear)
                     (create-draw-event)
                     (event-queue/call-event-handlers)))
          (window/close (:window gui))))
      (catch Exception e
        (println e)
        (.printStackTrace e)
        (window/close (:window initial-gui))))))