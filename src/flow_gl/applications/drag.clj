(ns flow-gl.applications.drag
  (:require (flow-gl.gui [drawable :as drawable]
                         [layoutable :as layoutable]
                         [layout :as layout]
                         [view :as view]
                         [application :as application])

            (flow-gl.graphics.command [triangle-batch :as triangle-batch])
            [flow-gl.graphics.vector :as graphics-vector]

            (flow-gl [dataflow :as dataflow]
                     [debug :as debug])))

(defn end-point [x y mouse-over-id]
  (-> (drawable/->FilledCircle (if (dataflow/get-value-or-initialize mouse-over-id false)
                                 [0 1 0 1]
                                 [0 0.6 0 1])
                               10)
      (assoc :x x :y y)
      (view/with-mouse-over mouse-over-id)))

(defn view []
  (layout/->Absolute (apply concat (for [line-id (dataflow/get-global-value :lines)]
                                     (let [[x1 y1 x2 y2] (dataflow/get-global-value [:line line-id])]
                                       (flow-gl.debug/debug :default x1 y1 x2 y2)
                                       [(drawable/->Line [1 0 0 0.5]
                                                         10
                                                         x1 y1 x2 y2)
                                        (end-point x1 y1 [:mouse-over line-id :from])
                                        (end-point x2 y2 [:mouse-over line-id :to])])))))


(defonce state-atom-atom (atom nil))

(defn initialize [state state-atom]
  (reset! state-atom-atom state-atom)
  (dataflow/define-to state
    :lines [1 2]
    [:line 1] [10 50 100 100]
    [:line 2] [10 50 10 100]))

(defn start []
  (application/start view
                     :initialize initialize
                     :framerate 30))

(defn refresh []
  (when @state-atom-atom
    (swap! @state-atom-atom view/set-view view)))

(refresh)

(comment

(start)

  (.start (Thread. start))

  (debug/set-active-channels
   ;; :view-definition
   ;; :initialization
   :dataflow
   ;; :events
   :view-update
   :default
   ;; :render
   )
  )
