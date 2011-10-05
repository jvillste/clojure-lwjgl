(ns clojure-lwjgl.test.event-queue
  (:use [clojure.test :only (deftest is run-tests)])
  (:require [clojure-lwjgl.event-queue :as event-queue]))

(defn pq [& values]
  (apply conj clojure.lang.PersistentQueue/EMPTY values))

(deftest event-queue-add-test
  (is (= [[1 '({:time 1})] [2 '({:time 2})] [3 '({:time 3})]]
         (into [] (-> (event-queue/create)
                      (event-queue/add {:time 1})
                      (event-queue/add {:time 3})
                      (event-queue/add {:time 2}))))))

(deftest event-queue-oldest-test
  (is (= {:time 1}
         (-> (event-queue/create)
             (event-queue/add {:time 3})
             (event-queue/add {:time 1})
             (event-queue/add {:time 2})
             (event-queue/oldest)))))

(deftest event-queue-remove-oldest-test
  (is (= {1 [{:time 1, :order :b} {:time 1, :order :c}], 2 [{:time 2}], 3 [{:time 3}]}
         (-> (event-queue/create)
             (event-queue/add {:time 3})
             (event-queue/add {:time 1 :order :a})
             (event-queue/add {:time 1 :order :b})
             (event-queue/add {:time 1 :order :c})
             (event-queue/add {:time 2})
             (event-queue/remove-oldest)))))

(run-tests 'clojure-lwjgl.test.event-queue)
