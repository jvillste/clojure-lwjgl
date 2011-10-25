(ns clojure-lwjgl.test.paint.vector2d
  (:use clojure.test)
  (:require [clojure-lwjgl.paint.vector2d :as vector2d]))

(deftest interpolate-test
  (is (= '({:x 2.0, :y 1.0} {:x 3.0, :y 1.0})
         (vector2d/interpolate 1
                               {:x 1 :y 1}
                               {:x 4 :y 1})))
  (is (= '({:x 1.0, :y 2.0} {:x 1.0, :y 3.0})
         (vector2d/interpolate 1
                               {:x 1 :y 1}
                               {:x 1 :y 4})))
  (is (= '()
         (vector2d/interpolate 1
                               {:x 1 :y 1}
                               {:x 1 :y 1})))
  (is (= '({:x 1.0, :y 2.0})
         (vector2d/interpolate 1
                               {:x 1 :y 3}
                               {:x 1 :y 1})))
  (is (= '({:x 3.1679497056621564, :y 2.4452998037747706} {:x 2.335899411324313, :y 1.8905996075495417} {:x 1.5038491169864687, :y 1.3358994113243126})
         (vector2d/interpolate 1
                               {:x 4 :y 3}
                               {:x 1 :y 1}))))

(run-tests 'clojure-lwjgl.test.paint.vector2d)