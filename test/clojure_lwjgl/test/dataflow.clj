(ns clojure-lwjgl.test.dataflow
  (:require (clojure-lwjgl [logged-access :as logged-access]))
  (:use clojure.test))

(defn dependants [dataflow path]
  (filter #(contains? (get-in dataflow [::dependencies %])
                      path)
          (keys (::dependencies dataflow))))


(deftest dependants-test
  (is (= (dependants {::dependencies {[:a] #{[:b] [:c]}
                                      [:b] #{[:d]}}}
                     [:b])
         '([:a])))

  (is (= (dependants {::dependencies {[:a] #{[:b] [:c]}
                                      [:b] #{[:d]}}}
                     [:e])
         '())))

(defn update-dependencies [dataflow path]
  (println "updating dependencies for " path)
  (reduce (fn [dataflow dependant-path]
            (println "updating " dependant-path)
            (-> dataflow
                (assoc-in dependant-path ((get-in dataflow [::functions dependant-path]) dataflow))
                (update-dependencies dependant-path)))
          dataflow
          (dependants dataflow path)))

(defn define [dataflow path function]
  (println "defining " path)
  (logged-access/with-access-logging
    (-> dataflow
        (assoc-in path (function dataflow))
        (assoc-in [::functions path] function)
        (assoc-in [::dependencies path] @logged-access/reads)
        (update-dependencies path))))

(deftest define-test
  (is (= (-> {}
             (define [:b]
               (fn [_] 1))
             (define [:c]
               (fn [_] 1))
             (define [:a]
               (fn [dataflow]
                 (+ 1
                    (logged-access/get dataflow :b)
                    (logged-access/get dataflow :c))))
             (define [:b]
               (fn [_] 2))
             (dissoc ::functions))
         
         {:a 4
          :b 2
          :c 1
          :clojure-lwjgl.test.dataflow/dependencies {[:a] #{[:c] [:b]}
                                                     [:c] #{}
                                                     [:b] #{}}})))

(run-tests)

(comment
  (thread-it dataflow
             (assoc-in it [:x] 1)
             (assoc-in it [:y] (+ 1
                                  (get it [:x]))))

  )

