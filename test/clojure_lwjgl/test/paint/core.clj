(ns clojure-lwjgl.test.paint.core
  (:use clojure.test)
  (:require [clojure-lwjgl.paint.core :as core]))

(deftest interpolate-coordinates-test
  (is (= '({:y 1, :x 1} {:x 2.0, :y 1.0} {:y 1, :x 3} {:x 4.0, :y 1.0} {:y 1, :x 5})
         (core/interpolate-coordinates [{:x 1 :y 1} {:x 3 :y 1} {:x 5 :y 1}]
                                       1)))
  (is (= [{:y 44, :x 594}]
         (core/interpolate-coordinates [{:x 594 :y 44}]
                                       51))))

(run-tests 'clojure-lwjgl.test.paint.core)