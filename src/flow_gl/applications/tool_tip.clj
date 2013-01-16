(ns flow-gl.applications.tool-tip
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [view :as view]
                         [application :as application])
            (flow-gl.graphics [font :as font])

            (flow-gl [dataflow :as dataflow]
                     [debug :as debug])))


(defn start []
  (application/start
   (fn [] (layout/->Stack (concat [(layout/->VerticalStack (vec (for [i (range 10)]
                                                                  (-> (drawable/->Rectangle 100 10 (if (dataflow/get-value-or-initialize i false)
                                                                                                     [1 1 0 1]
                                                                                                     [(/ i 10) 1 0 1]))
                                                                      (view/with-mouse-over i)
                                                                      (view/add-mouse-event-handler [:tooltip i]
                                                                                                    (fn [view-state event]))))))
                                   ]
                                  [(drawable/->Text (str i) (font/create "LiberationSans-Regular.ttf" 15) [0 1 0 1]) ])))))

(comment
  (start))