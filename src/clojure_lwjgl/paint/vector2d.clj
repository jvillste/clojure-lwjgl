(ns clojure-lwjgl.paint.vector2d)

(defn apply-operator [operator vector1 vector2]
  {:x (operator (:x vector1)
                (:x vector2))
   :y (operator (:y vector1)
                (:y vector2))})

(defn add [vector1 vector2]
  (apply-operator + vector1 vector2))

(defn substract [vector1 vector2]
  (apply-operator - vector1 vector2))

(defn multiply [constant vector]
  {:x (* (:x vector)
         constant)
   :y (* (:y vector)
         constant)})

(defn length [vector]
  (Math/sqrt (+ (* (:x vector)
                   (:x vector))
                (* (:y vector)
                   (:y vector)))))


(defn interpolate [maximum-distance vector1 vector2]
  (let [delta (substract vector2
                         vector1)

        distance (length delta)

        number-of-moves (/ distance
                           maximum-distance)]
    (if (= 0
           distance)
      '()
      (map (fn [n] (add vector1
                        (multiply (/ n number-of-moves)
                                  delta)))
           (range 1
                  number-of-moves)))))

(defn create [x y]
  {:x x :y y})
