(ns flow-gl.applications.mouse-over
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [view :as view]
                         [application :as application])

            (flow-gl [dataflow :as dataflow]
                     [debug :as debug])))


(defn start []
  (application/start
   (fn [] (layout/->VerticalStack (vec (for [i (range 10)]
                                         (-> (drawable/->Rectangle 100 10 (if (dataflow/get-value-or-initialize [:mouse-over i] false)
                                                                            [1 1 0 1]
                                                                            [0.8 0.8 0 1]))
                                             (view/with-mouse-over [:mouse-over i]))))))
   :framerate 60))

(comment
(start)

(debug/set-active-channels
   ;; :view-definition
   ;; :initialization
   ;;:dataflow
   ;; :events
   ;; :view-update
   ;; :default
   ;; :render
   ))
