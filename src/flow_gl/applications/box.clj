(ns flow-gl.applications.box
  (:require (flow-gl.gui [input :as input]
                         [drawable :as drawable]
                         [layoutable :as layoutable]
                         [layout :as layout]
                         [view :as view]
                         [application :as application])

            (flow-gl.graphics.command [triangle-batch :as triangle-batch])
            [flow-gl.graphics.vector :as graphics-vector]

            (flow-gl [dataflow :as dataflow]
                     [debug :as debug])))

(defn view []
  (let [size 100]
    (layout/->Absolute [(assoc (drawable/->Rectangle size size [1 1 0 1])
                          :x (* size (dataflow/get-global-value :x))
                          :y (* size (dataflow/get-global-value :y)))])))

(defn handle-event [state view event]
  (cond (input/key-pressed? event input/down)
        (dataflow/apply-to-value state :y inc)

        (input/key-pressed? event input/up)
        (dataflow/apply-to-value state :y dec)

        (input/key-pressed? event input/right)
        (dataflow/apply-to-value state :x inc)

        (input/key-pressed? event input/left)
        (dataflow/apply-to-value state :x dec)

        :default state))

(defn initialize [state state-atom]
  (dataflow/define-to state
    :x 0
    :y 0))

(defn start []
  (application/start view
                     :handle-event handle-event
                     :initialize initialize
                     :framerate 160))

(defn refresh []
  (when @application/state-atom-atom
    (swap! @application/state-atom-atom
           view/set-view
           view)))

(refresh)

(defn start-async []
  (.start (Thread. start)))

(comment
  (start-async)
  )