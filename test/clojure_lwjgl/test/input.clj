(ns clojure-lwjgl.test.input
  (:use clojure.test)
  (:require [clojure-lwjgl.input :as input]))


(deftest mouse-button-down
  (is (= [:middle-mouse-button-down true :last-event {:type :middle-mouse-button-down}]
         (input/get-mouse-state-changes-for-event {
                                                   :mouse-button 2
                                                   :mouse-button-state true
                                                   :mouse-wheel-delta 0
                                                   :mouse-x 0
                                                   :mouse-y 0
                                                   :mouse-delta-x 0
                                                   :mouse-delta-y 0
                                                   }))))

(deftest mouse-move
  (is (= [:mouse-x 10 :mouse-y 100 :last-event {:type :mouse-moved}]
         (input/get-mouse-state-changes-for-event {
                                                   :mouse-button -1
                                                   :mouse-button-state true
                                                   :mouse-wheel-delta 0
                                                   :mouse-x 10
                                                   :mouse-y 100
                                                   :mouse-delta-x 10
                                                   :mouse-delta-y 0
                                                   }))))

(deftest key-down
  (is (= [:keys-down #{1} :last-event {:type :key-pressed, :key-code 1, :character "a"}]
         (input/get-keyboard-state-changes-for-event {:key-state true
                                                      :key-code 1
                                                      :character "a"}
                                                     {:keys-down #{}}))))

(deftest key-up
  (is (= [:keys-down #{2} :last-event {:type :key-released, :key-code 1, :character nil}]
         (input/get-keyboard-state-changes-for-event {:key-state false
                                                      :key-code 1
                                                      :character nil}
                                                     {:keys-down #{1 2}}))))


(deftest apply-state-changes
  (binding [input/get-time (fn [] 100)]
    (is (= [{:foo 3} {:foo 4, :bar 5, :foobar 9} {:time 100, :foo 1, :bar 2, :foobar 9}]
           (input/apply-state-changes [:foo 1 :bar 2] [{:foo 3} {:foo 4 :bar 5 :foobar 9}])))))


(deftest sort-input-states
  (is (= [{:time 1} {:time 2 :foo :bar} {:time 4}]
         (input/sort-input-states [{:time 2 :foo :bar} {:time 4} {:time 1}]))))

(run-tests 'clojure-lwjgl.test.input)

