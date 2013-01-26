(ns flow-gl.applications.dataflow-explorer
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [view :as view]
                         [application :as application])

            (flow-gl [dataflow :as dataflow]
                     [debug :as debug])
            (flow-gl.graphics [font :as font])))

(defn start [dataflow]
  (let [font (font/create "LiberationSans-Regular.ttf" 15)]
    (application/start
     (fn []
       (flow-gl.debug/debug :default "dataflow: " dataflow)
       (layout/->VerticalStack (vec (for [path (flow-gl.debug/debug :default "paths " (sort (keys (:flow-gl.dataflow/functions dataflow))))]
                                      (-> (drawable/->Text (str path) font [1 1 1 1])
                                          (view/add-mouse-event-handler path (fn [state event]
                                                                               (println path event)
                                                                               state))))))))))

(defn test []
  (start (-> (dataflow/create)
             (dataflow/define-to
               :a 1
               :b #(+ (dataflow/get-value :a)
                      1))) ))

(comment
(test)

(debug/set-active-channels
   ;; :view-definition
   ;; :initialization
   ;; :dataflow
   :events
   ;; :view-update
   :default
   ;; :render
   ))