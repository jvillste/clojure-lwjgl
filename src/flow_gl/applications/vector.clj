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


(defn line-coordinates [width x1 y1 x2 y2]
  (let [line-angle (Math/atan2 (- x2 x1)
                               (- y2 y1))
        cap-angle (- Math/PI
                     line-angle)

        half-width (/ width 2)

        cap-x (* (Math/cos cap-angle)
                 half-width)
        cap-y (* (Math/sin cap-angle)
                 half-width)

        v1x (- x1 cap-x)
        v1y (- y1 cap-y)
        v2x (- x2 cap-x)
        v2y (- y2 cap-y)
        v3x (+ x2 cap-x)
        v3y (+ y2 cap-y)
        v4x (+ x1 cap-x)
        v4y (+ y1 cap-y)]

    [v1x v1y
     v4x v4y
     v2x v2y

     v4x v4y
     v2x v2y
     v3x v3y]))

(defn single-color [coordinates color]
  (apply concat (-> (count coordinates)
                    (/ 2)
                    (repeat color))))

(defrecord Line [color line-width x1 y1 x2 y2]
  drawable/Drawable
  (drawing-commands [this]
    (let [coordinates (line-coordinates line-width x1 y1 x2 y2)]
      [(triangle-batch/create coordinates
                              (single-color coordinates color))]))

  layoutable/Layoutable
  (preferred-width [this] (max x1 x2))
  (preferred-height [this] (max y1 y2))

  Object
  (toString [this] (layoutable/describe-layoutable this "Line" :color :x1 :y1 :x2 :y2)))

(defn view []
  (layout/->Absolute (let [n 50
                           l 100
                           l2 30]
                       (for [i (range 1 n)]
                         (let [angle (-> (* 4 Math/PI)
                                         (* i)
                                         (/ n))]
                           (-> (->Line [1 0 0 1]
                                       3
                                       (* l2 (Math/cos angle))
                                       (* l2 (Math/sin angle))
                                       (* l (Math/cos angle))
                                       (* l (Math/sin angle)))
                               (assoc :x l :y l)))))))

(defonce sa (atom nil))

(defn initialize [state state-atom]
  (reset! sa state-atom)
  state)

(defn start []
  (application/start view :initialize initialize))

(defn refresh []
  (when @sa
    (swap! @sa (fn [state]
                 (println "refresh")
                 (-> state
                     (dataflow/define-to [:elements] view)
                     (dataflow/propagate-changes))))))

(refresh)
(comment

  (start)
  (.start (Thread. start))

  )
