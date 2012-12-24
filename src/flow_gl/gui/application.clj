(ns flow-gl.gui.application
  (:require (flow-gl.opengl [window :as window])
            (flow-gl.gui [view :as view])
            [flow-gl.debug :as debug]))


(defn start [width height framerate initialize event-handler root-layoutable-constructor]
  (debug/reset-log)
  (let [window-atom (window/create width height)]
    (try
      (let [state-atom (-> (view/create width height event-handler root-layoutable-constructor)
                                                      (assoc :window-atom window-atom)
                                                      (initialize)
                                                      (atom))]
        (loop []
          (let [{:keys [resize-requested close-requested width height]} @window-atom]
            (if close-requested
              (do (view/send-close-event @state-atom)
                  (swap! window-atom window/close))
              (do (swap! window-atom window/update framerate)
                  (when resize-requested
                    (view/handle-resize state-atom width height))
                  (view/update state-atom)
                  (recur))))))
      (catch Exception e
        (println "Exception in main loop: " e)
        (.printStackTrace e)
        (swap! window-atom window/close)
        (throw e))
      (finally (debug/write-log)))
    nil))

(defn close [application-state]
  (swap! (:window-atom application-state) window/request-close)
  application-state)