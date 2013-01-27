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

(defn single-color [coordinates color]
  (apply concat (-> (count coordinates)
                    (/ 2)
                    (repeat color))))

(defn single-color-triangle-batch [coordinates color]
  (triangle-batch/create coordinates
                         (single-color coordinates color)))


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

(defrecord Line [color line-width x1 y1 x2 y2]
  drawable/Drawable
  (drawing-commands [this]
    [(single-color-triangle-batch (line-coordinates line-width x1 y1 x2 y2)
                                  color)])

  layoutable/Layoutable
  (preferred-width [this] (max x1 x2))
  (preferred-height [this] (max y1 y2))

  Object
  (toString [this] (layoutable/describe-layoutable this "Line" :color :x1 :y1 :x2 :y2)))

(defn arc-vertices [from-angle to-angle radius]
  (let [angle-difference (- to-angle from-angle)
        n (int (* 20
                  (/ angle-difference
                     (* 2 Math/PI))))
        cos-sin (for [i (range n)]
                  (let [angle (-> (* i angle-difference)
                                  (/ n)
                                  (+ from-angle))]
                    [(Math/cos angle)
                     (Math/sin angle)]))]
    (map (fn [[cos sin]]
           [(* radius cos)
            (* radius sin)] )
         cos-sin)))

(defn circle-coordinates [segment]
  (let [n 20
        cos-sin (for [i (range n)]
                  (let [angle (-> (* i 2 Math/PI)
                                  (/ n))]
                    [(Math/cos angle)
                     (Math/sin angle)]))]
    (apply concat

           (let [[cos2 sin2] (first cos-sin)
                 [cos1 sin1] (last cos-sin)]
             (segment cos1 sin1 cos2 sin2))

           (for [[[cos1 sin1] [cos2 sin2]] (partition 2 1 cos-sin)]
             (segment cos1 sin1 cos2 sin2)))))

(defrecord FilledCircle [color radius]
  drawable/Drawable
  (drawing-commands [this]
    [(single-color-triangle-batch (circle-coordinates (fn [cos1 sin1 cos2 sin2]
                                                        [0 0
                                                         (* radius cos2) (* radius sin2)
                                                         (* radius cos1) (* radius sin1)]))
                                  color)])

  layoutable/Layoutable
  (preferred-width [this] (* 2 radius))
  (preferred-height [this] (* 2 radius))

  Object
  (toString [this] (layoutable/describe-layoutable this "FilledCircle" :color :radius)))

(defrecord Circle [color radius line-width]
  drawable/Drawable
  (drawing-commands [this]
    [(single-color-triangle-batch (circle-coordinates (fn [cos1 sin1 cos2 sin2]
                                                        (let [inner-radius (- radius line-width)

                                                              x1 (* inner-radius
                                                                    cos1)
                                                              y1 (* inner-radius
                                                                    sin1)

                                                              x2 (* radius
                                                                    cos1)
                                                              y2 (* radius
                                                                    sin1)

                                                              x3 (* radius
                                                                    cos2)
                                                              y3 (* radius
                                                                    sin2)

                                                              x4 (* inner-radius
                                                                    cos2)
                                                              y4 (* inner-radius
                                                                    sin2)]
                                                          [x1 y1
                                                           x3 y3
                                                           x2 y2

                                                           x1 y1
                                                           x4 y4
                                                           x3 y3])))
                                  color)])

  layoutable/Layoutable
  (preferred-width [this] (* 2 radius))
  (preferred-height [this] (* 2 radius))

  Object
  (toString [this] (layoutable/describe-layoutable this "Circle" :color :radius :line-width)))

(defn rounded-rectangle-vertices [width height radius]
  (apply concat (concat (map (fn [[x y]] [(+ x radius) (+ y radius)]) (arc-vertices Math/PI
                                                                                    (* (/ 3 2) Math/PI)
                                                                                    radius))
                        (map (fn [[x y]] [(+ x (- width radius)) (+ y radius)]) (arc-vertices (* (/ 3 2) Math/PI)
                                                                                              (* 2 Math/PI)
                                                                                              radius))
                        (map (fn [[x y]] [(+ x (- width radius)) (+ y (- height radius))]) (arc-vertices 0
                                                                                                         (/ Math/PI 2)
                                                                                                         radius))
                        (map (fn [[x y]] [(+ x radius) (+ y (- height radius))]) (arc-vertices (/ Math/PI 2)
                                                                                               Math/PI
                                                                                               radius)))))

(defn triangle-fan [vertices]
  (let [root-x (first vertices)
        root-y (second vertices)]
    (apply concat (for [[x1 y1 x2 y2] (partition 4 2 (drop 2 vertices))]
                    [root-x root-y
                     x2 y2
                     x1 y1]))))

(defrecord FilledRoundedRectangle [width height radius color]
  drawable/Drawable
  (drawing-commands [this]
    [(single-color-triangle-batch (triangle-fan (rounded-rectangle-vertices width height radius))
                                  color)])

  layoutable/Layoutable
  (preferred-width [this] width)
  (preferred-height [this] height)

  Object
  (toString [this] (layoutable/describe-layoutable this "FilledRoundedRectangle" :radius)))


(defn line-view []
  (layout/->Absolute (let [n 50
                           l 200
                           l2 30]
                       (for [i (range 1 n)]
                         (let [angle (-> (* 4 Math/PI)
                                         (* i)
                                         (/ n))]
                           (-> (->Line [1 0 0 1]
                                       1
                                       (* l2 (Math/cos angle))
                                       (* l2 (Math/sin angle))
                                       (* l (Math/cos angle))
                                       (* l (Math/sin angle)))
                               (assoc :x l :y l)))))))


(defn filled-circle-view []
  (layout/->Absolute (for [i (range 15)]
                       (let [r (* i 5)]
                         (-> (->FilledCircle [1 0 0 1]
                                             r)
                             (assoc :x (+ (* i r) (+ r 50))
                                    :y (+ r 50)))))))

(defn circle-view []
  (layout/->Absolute (for [i (range 15)]
                       (let [r (* i 5)]
                         (-> (->Circle [1 1 0 1]
                                       r
                                       (* 1.2 i))
                             (assoc :x (+ (* i r) (+ r 50))
                                    :y (+ r 50)))))))

(defn rounded-rectangle-view []
  (layout/->Absolute (concat (for [[x y] (->> (rounded-rectangle-vertices 200 100 30)
                                              (partition 2))]
                               (-> (->FilledCircle [1 1 0 1]
                                                   2)
                                   (assoc :x (+ 50 x)
                                          :y (+ 50 y))))

                             [(-> (->FilledRoundedRectangle 300 300 100 [1 1 0 1])
                                  (assoc :x 500
                                         :y 50))])))


(defn clock []
  (layout/->Absolute [(let [n 50
                            l 200
                            l2 30]
                        (let [angle (-> (dataflow/get-global-value :time)
                                        (mod 1e9)
                                        (/ 1e9)
                                        (* (* 2 Math/PI)))]
                          (-> (->Line [1 0 0 1]
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
  (application/start rounded-rectangle-view
                     :initialize initialize
                     :framerate 10))

(defn refresh []
  (when @sa
    (swap! @sa view/set-view rounded-rectangle-view)))

(refresh)

(comment

  (start)
(.start (Thread. start))

  )
