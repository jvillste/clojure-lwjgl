(ns clojure-lwjgl.test.gui
  (:use clojure.test)
  (:require [clojure-lwjgl.gui :as gui]
            [clojure-lwjgl.event-queue :as event-queue]))


(deftest call-event-handlers-for-single-event-test
  (is (= {:handler-1 2, :hanlder-2 2}
         (dissoc (gui/call-event-handlers-for-single-event {:event-handlers #{(fn [gui event] (assoc gui
                                                                                                :handler-1 (:time event)))
                                                                              (fn [gui event] (assoc gui
                                                                                                :hanlder-2 (:time event)))}}
                                                           {:type :foo :time 2})
                 :event-handlers))))

(deftest call-event-handlers-test
  (is (= {:handler-2 :foo, :handler-1 2, :event-queue {}}
         (dissoc (gui/call-event-handlers {:event-queue (-> (event-queue/create)
                                                            (event-queue/add {:type :foo :time 2})
                                                            (event-queue/add {:type :bar :time 1}))
                                           :event-handlers #{(fn [gui event] (assoc gui
                                                                               :handler-1 (:time event)))
                                                             (fn [gui event] (assoc gui
                                                                               :handler-2 (:type event)))}})
                 :event-handlers))))


(deftest call-event-handlers-test-2
  (is (= {:handler-1 3, :handler-2 3, :event-queue {}}
         (dissoc (gui/call-event-handlers {:event-queue (-> (event-queue/create)
                                                            (event-queue/add {:type :foo :time 2})
                                                            (event-queue/add {:type :bar :time 1}))
                                           :event-handlers #{(fn [gui event] (if (= (:time event) 2)
                                                                               (assoc gui
                                                                                 :event-queue (event-queue/add (:event-queue gui)
                                                                                                               {:time 3}))
                                                                               (assoc gui
                                                                                 :handler-1 (:time event))))
                                                             (fn [gui event] (assoc gui
                                                                               :handler-2 (:time event)))}})
                 :event-handlers))))


(run-tests 'clojure-lwjgl.test.gui)

