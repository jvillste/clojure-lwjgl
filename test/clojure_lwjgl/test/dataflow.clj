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

(def ^:dynamic current-dataflow)

(def ^:dynamic current-path [])

(defn update-value [dataflow path]
  (println "updating" path)
  (logged-access/with-access-logging
    (-> dataflow
        (assoc-in path (binding [current-dataflow dataflow]
                         ((get-in dataflow [::functions path]))))
        #_(assoc-in [::dependencies path] @logged-access/reads)
        #_(update-in [::changed-paths] conj path))))

(defn changes [dataflow]
  (::changed-paths dataflow))

(defn reset-changes [dataflow]
  (assoc dataflow
    ::changed-paths #{}))

(defn update-dependant-paths [dataflow path]
  (println "dependant " path)
  (reduce (fn [dataflow dependant-path]
            (-> dataflow
                (update-value dependant-path)
                (update-dependant-paths dependant-path)))
          dataflow
          (dependants dataflow path)))

(defn as-path [keyword-or-path]
  (if (vector? keyword-or-path)
    keyword-or-path
    [keyword-or-path]))

(defn define [dataflow & paths-and-functions]
  (reduce (fn [dataflow [path function]]
            (let [function (if (fn? function)
                             function
                             (fn [] function))
                  path (apply vector (concat current-path (as-path path)))]
              (println "defining " path)
              (-> dataflow
                  (assoc-in [::functions path] function)
                  (update-value path)
                  (update-dependant-paths path))))
          dataflow
          (partition 2 paths-and-functions)))

(defn define-child [path function]
  (let [child-path (concat current-path path)]
    (binding [current-path path]
      (swap! current-dataflow (fn [dataflow] (-> dataflow
                                               (define child-path function)
                                               (assoc-in [:children path] child-path)))))))

(defn undefine [dataflow path]
  (-> dataflow
      (update-in [::functions] dissoc path)
      (update-in [::dependencies] dissoc path)))

(defn apply-to-value [dataflow path function]
  (define dataflow path (function (get-in dataflow (as-path path)))))

(defn get-value [key]
  (println "get-value " key)
  (logged-access/get current-dataflow (conj current-path key)))

(defn values-to-map [& keys]
  (reduce (fn [result key] (assoc result key (get-value key)))
          {}
          keys))

(defmacro with-values [keys & body]
  `(let [{:keys [~@keys]} (values-to-map ~@(map #(keyword (name %)) keys))]
     ~@body))

(defn get-value-in [path]
  (logged-access/get-in current-dataflow (concat current-path path)))


(defn create []
  {::changed-paths #{}})

(deftest define-test
  (is (= (-> (create)

             (define
               :b 1
               :c 1)

             (define :d #(+ (get-value :b)
                            (get-value :c)))

             (dissoc ::functions))

         nil)))


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
                              (get-value :b)
                              (get-value :c)))
               (define :b 2)
               (dissoc ::functions)))
  )

