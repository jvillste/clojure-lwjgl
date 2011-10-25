(ns clojure-lwjgl.test.paint
  (:use clojure.test)
  (:require [clojure-lwjgl.paint.core :as core]))


(deftest interpolate-test
  (is (= '({:x 1, :y 1} {:x 2.0, :y 1.0} {:x 3.0, :y 1.0})
         (core/interpolate 1
                           {:x 1 :y 1}
                           {:x 4 :y 1})))
  (is (= '({:x 1, :y 1} {:x 1.0, :y 2.0} {:x 1.0, :y 3.0})
         (core/interpolate 1
                           {:x 1 :y 1}
                           {:x 1 :y 4}))))

(deftest interpolate-coordinates-test
  (is (= '({:y 1, :x 1} {:x 2.0, :y 1.0} {:x 3.0, :y 1.0} {:x 4.0, :y 1.0} {:y 1, :x 5})
         (core/interpolate-coordinates [{:x 1 :y 1} {:x 3 :y 1} {:x 5 :y 1}]
                                       {:x 1 :y 1}
                                       1))))

(run-tests 'clojure-lwjgl.test.paint)