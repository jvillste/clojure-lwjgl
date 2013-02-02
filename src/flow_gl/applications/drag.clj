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
  (let [radius 15]
    (-> (drawable/->FilledCircle (if (dataflow/get-value-or-initialize mouse-over-id false)
                                   [0 1 0 1]
                                   [0 0.6 0 1])
                                 15)
        (assoc :x (- x radius) :y (- y radius))
        (view/with-mouse-over mouse-over-id))))

(defn view []
  (layout/->Absolute (apply concat (for [line-id (dataflow/get-global-value :lines)]
                                     (let [[x1 y1 x2 y2] (dataflow/get-global-value [:line line-id])]
                                       (flow-gl.debug/debug :default x1 y1 x2 y2)
                                       [(drawable/->Line [1 0 0 0.5]
                                                         2
                                                         x1 y1 x2 y2)
                                        (-> (end-point x1 y1 [:mouse-over line-id :from])
                                            (view/add-mouse-event-handler [:drag-handler line-id :from]
                                                                          (fn [state event]
                                                                            (if (= (:type event)
                                                                                   :left-mouse-button-down)
                                                                              (-> state
                                                                                  (dataflow/define-to :drag-start [(dataflow/get-global-value-from state :mouse-x)
                                                                                                                   (dataflow/get-global-value-from state :mouse-y)
                                                                                                                   x1 y1])
                                                                                  (view/capture-mouse (fn [state event]
                                                                                                        (case (:type event)
                                                                                                          :left-mouse-button-up (-> state
                                                                                                                                    (dataflow/undefine :drag-start)
                                                                                                                                    (view/release-mouse))
                                                                                                          :mouse-moved (if-let [[drag-start-x drag-start-y original-x1 original-y1] (dataflow/get-global-value-from state :drag-start)]
                                                                                                                         (let [delta-x (- (:mouse-x event) drag-start-x)
                                                                                                                               delta-y (- (:mouse-y event) drag-start-y)]
                                                                                                                           (dataflow/define-to state [:line line-id] [(+ delta-x original-x1)
                                                                                                                                                                      (+ delta-y original-y1)
                                                                                                                                                                      x2 y2]))


                                                                                                                         state)
                                                                                                          state))))
                                                                              state))))
                                        (end-point x2 y2 [:mouse-over line-id :to])])))))

(defn view2 []
  (layout/->Absolute [(-> (drawable/->Rectangle 100 100 [1 1 1 1])
                          (assoc :x 50 :y 50)
                          (view/add-mouse-event-handler [:handler]
                                                        (fn [state event]
                                                          (println event)
                                                          state)))]))


(defn initialize [state state-atom]
  (dataflow/define-to state
    :lines [1 2]
    [:line 1] [10 200 100 100]
    [:line 2] [10 50 10 100]))

(defn start []
  (application/start view
                     :initialize initialize
                     :framerate 30))

(defn refresh []
  (when @application/state-atom-atom
    (swap! @application/state-atom-atom view/set-view view)))

(refresh)


(comment
  (start)

  (defn generate []
    (println "generated")
    1)

  (println (take 2 (repeatedly generate)))

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
