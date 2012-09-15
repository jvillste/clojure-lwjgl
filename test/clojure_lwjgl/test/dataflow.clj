(ns clojure-lwjgl.test.dataflow
  (:require (clojure-lwjgl [logged-access :as logged-access])
            clojure.set)
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


(def ^:dynamic dataflow)

(defmacro with-dataflow [dataflow & body]
  `(binding [dataflow ~dataflow]
     ~@body))

(defn update-value [dataflow path]
  (-> dataflow
      (assoc-in path (with-dataflow dataflow ((get-in dataflow [::functions path]))))
      (update-in [::changed-paths] #(clojure.set/union % #{path}))))

(defn update-dependencies [dataflow path]
  (println "updating dependencies for " path)
  (reduce (fn [dataflow dependant-path]
            (println "updating " dependant-path)
            (-> dataflow
                (update-value dependant-path)
                (update-dependencies dependant-path)))
          dataflow
          (dependants dataflow path)))

(defn define [dataflow path function]
  (println "defining " path)
  (let [function (if (fn? function)
                   function
                   (fn [] function))
        path (if (vector? path)
               path
               [path])]
    (logged-access/with-access-logging
      (-> dataflow
          (assoc-in [::functions path] function)
          (assoc-in [::dependencies path] @logged-access/reads)
          (update-value path)
          (update-dependencies path)))))

(defn get-val [key]
  (logged-access/get dataflow key))

(defn get-val-in [path]
  (logged-access/get-in dataflow path))

(defn add-listener [dataflow path listener]
  (assoc-in dataflow [::listeners path] listener))

(deftest define-test
  (is (= (-> {}
             (define :b 1)
             (define :c 1)
             (define :a #(+ 1
                            (get-val :b)
                            (get-val :c)))
             (define :b 2)
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

  (println (-> {}
               (define :b 1)
               (define :c 1)
               (define :a #(+ 1
                              (get-val :b)
                              (get-val :c)))
               (define :b 2)
               (dissoc ::functions)))
  )

