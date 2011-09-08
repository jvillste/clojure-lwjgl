(ns test.clojure-lwjgl.component
  (:use clojure.test)
  (:use [clojure-lwjgl.component :as component])
  (:use [clojure-lwjgl.buffer :as buffer]))

(deftest new-texture-coordinates-test
  (is (= (component/new-texture-coordinates {:texture {:width 128 :height 128}} 0 100 10)
         {})))


(deftest maximum-allocated-y-test
  (is (= (component/maximum-allocated-y (buffer/create-float-buffer-from-values
                                  [0.0 0.0
                                   0.5 0.0
                                   0.5 1.0
                                   0.0 1.0]))
         1.0)))