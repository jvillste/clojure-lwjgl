(ns flow-gl.applications.vector
  (:require (flow-gl.gui [drawable :as drawable]
                         [layoutable :as layoutable]
                         [layout :as layout]
                         [view :as view]
                         [application :as application])

            (flow-gl.graphics.command [triangle-batch :as triangle-batch])
            [flow-gl.graphics.vector :as graphics-vector]

            (flow-gl [dataflow :as dataflow]
                     [debug :as debug])))



(defn line-view []
  (layout/->Absolute (let [n 50
                           l 200
                           l2 30]
                       (for [i (range 1 n)]
                         (let [angle (-> (* 4 Math/PI)
                                         (* i)
                                         (/ n))]
                           (-> (drawable/->Line [1 0 0 1]
                                       1
                                       (* l2 (Math/cos angle))
                                       (* l2 (Math/sin angle))
                                       (* l (Math/cos angle))
                                       (* l (Math/sin angle)))
                               (assoc :x l :y l)))))))


(defn filled-circle-view []
  (layout/->Absolute (for [i (range 15)]
                       (let [r (* i 5)]
                         (-> (drawable/->FilledCircle [1 0 0 1]
                                             r)
                             (assoc :x (+ (* i r) (+ r 50))
                                    :y (+ r 50)))))))

(defn circle-view []
  (layout/->Absolute (for [i (range 15)]
                       (let [r (* i 5)]
                         (-> (drawable/->Circle [1 1 0 1]
                                       r
                                       (* 1.2 i))
                             (assoc :x (+ (* i r) (+ r 50))
                                    :y (+ r 50)))))))

(defn rounded-rectangle-view []
  (layout/->Absolute (concat (for [[x y] (->> (drawable/rounded-rectangle-vertices 200 100 30)
                                              (partition 2))]
                               (-> (drawable/->FilledCircle [1 1 0 1]
                                                   2)
                                   (assoc :x (+ 50 x)
                                          :y (+ 50 y))))

                             [(-> (drawable/->FilledRoundedRectangle 300 300 100 [1 1 0 1])
                                  (assoc :x 500
                                         :y 50))])))

(defn polyline-view []
  (layout/->Absolute (let [coordinates [0 0
                                        0 40
                                        100 40
                                        100 100]
                           width 15]
                       (concat [(-> (drawable/->Polyline [1 1 0 1] width coordinates)
                                    (assoc :x 50
                                           :y 50))]
                               (for [[x y] (->> (drawable/polyline-coordinates width coordinates)
                                                (partition 2))]
                                 (-> (drawable/->FilledCircle [1 1 0 1]
                                                     2)
                                     (assoc :x (+ 200 x)
                                            :y (+ 50 y))))
                               (for [[x y] (->> coordinates
                                                (partition 2))]
                                 (-> (drawable/->FilledCircle [1 0 0 1]
                                                     2)
                                     (assoc :x (+ 200 x)
                                            :y (+ 50 y))))))))


(defn clock []
  (layout/->Absolute [(let [n 50
                            l 200
                            l2 30]
                        (let [angle (-> (dataflow/get-global-value :time)
                                        (mod 1e9)
                                        (/ 1e9)
                                        (* (* 2 Math/PI)))]
                          (-> (drawable/->Line [1 0 0 1]
                                      1
                                      (* l2 (Math/cos angle))
                                      (* l2 (Math/sin angle))
                                      (* l (Math/cos angle))
                                      (* l (Math/sin angle)))
                              (assoc :x l :y l))))]))

(defonce sa (atom nil))

(defn initialize [state state-atom]
  (reset! sa state-atom)
  state)

(defn start []
  (application/start filled-circle-view
                     :initialize initialize
                     :framerate 60))

(defn refresh []
  (when @sa
    (swap! @sa view/set-view
           ;;filled-circle-view
           ;;circle-view
           ;;line-view
           ;;rounded-rectangle-view
           ;;polyline-view
           clock
           )))

(refresh)

(comment

  (start)
(.start (Thread. start))

  )
