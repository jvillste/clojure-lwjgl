(ns test.clojure-lwjgl.component
  (:use [clojure.test :only (deftest is)])
  (:require [clojure-lwjgl.component :as component])
  (:require [clojure-lwjgl.buffer :as buffer]))

(deftest new-texture-coordinates-test
  (is (= (component/new-texture-coordinates {:texture {:width 128 :height 128}} 0 100 10)
         {})))


(defn test [] (binding [clojure-lwjgl.component/bar (fn [] "this is new bar")] (component/foo)))

(deftest create-component-container-test
  
  (binding [clojure-lwjgl.texture/create-gl-texture (fn [] 1)
            clojure-lwjgl.buffer/create-gl-buffer (fn [] 1)]
    (let [container (component/create-component-container)]
      (is (= (.limit (:texture-coordinate-buffer container))
             16)))))

(deftest maximum-allocated-y-test
  (is (= (component/maximum-allocated-y (buffer/create-float-buffer-from-values
                                  [0.0 0.0
                                   0.5 0.0
                                   0.5 1.0
                                   0.0 1.0]))
         1.0)))

(deftest update-buffer-test
  (is (= (into [] (buffer/float-buffer-to-array (buffer/update-buffer (buffer/create-float-buffer-from-values [1 2 3 4])
                                                                      1
                                                                      (float-array [9 9])))) 
          [1.0 9.0 9.0 4.0])))