(ns flow-gl.gui.application
  (:require (flow-gl.opengl [window :as window])
            (flow-gl.gui [view :as view])
            [flow-gl.debug :as debug]
            [flow-gl.dataflow :as dataflow]))


(defn start [root-layoutable-constructor & {:keys [handle-event initialize width height framerate]
                                            :or {handle-event (fn [state view event] state)
                                                 initialize (fn [state state-atom] state)
                                                 width 700
                                                 height 500
                                                 framerate 30}} ]
  (debug/reset-log)
  (let [window-atom (window/create width height)]

    (try
      (let [state-atom (-> (view/create width height handle-event root-layoutable-constructor)
                           (assoc :window-atom window-atom)
                           (atom))]

        (swap! state-atom
               (fn [state]
                 (-> state
                     ;; (assoc :state-atom state-atom)
                     (initialize state-atom)
                     (dataflow/propagate-changes)
                     ((fn [view-state]
                        (flow-gl.debug/debug :initialization "Initial view state:")
                        (flow-gl.debug/debug-all :initialization (dataflow/dependency-tree view-state))
                        view-state)))))

        (loop []
          (let [{:keys [resize-requested close-requested width height]} @window-atom]
            (if close-requested
              (do (view/send-close-event @state-atom)
                  (swap! window-atom window/close))
              (do (window/update framerate)
                  (when resize-requested
                    (swap! window-atom assoc
                           :resize-requested false)
                    (window/resize width height)
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