(ns clojure-lwjgl.test.dataflow
  (:require (clojure-lwjgl [logged-access :as logged-access])
            clojure.set)
  (:use clojure.test))

(defn strip [dataflow]
  (dissoc dataflow
          ::children
          ::functions
          ::dependencies
          ::changed-paths))

(defn print-dataflow [dataflow]
  (clojure.pprint/pprint (strip dataflow)))

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

(def ^:dynamic parent-path [])

(defn debug [value message]
  (println message " " value)
  value)

(defn update-value [dataflow path]
  (println "updating " path " in " (strip dataflow))
  (logged-access/with-access-logging
    (let [new-dataflow (atom dataflow)
          new-value (binding [parent-path current-path
                              current-path path
                              current-dataflow new-dataflow]
                      ((get-in dataflow [::functions path])))]
      (-> @new-dataflow
          (assoc path new-value)
          (assoc-in [::dependencies path] @logged-access/reads)
          (update-in [::changed-paths] conj path)))))

(defn changes [dataflow]
  (::changed-paths dataflow))

(defn reset-changes [dataflow]
  (assoc dataflow
    ::changed-paths #{}))

(defn update-dependant-paths [dataflow path]
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

(defn define [& paths-and-functions]
  (swap! current-dataflow (fn [dataflow]
                            (reduce (fn [dataflow [path function]]
                                      (let [function (if (fn? function)
                                                       function
                                                       (fn [] function))
                                            path (apply vector (concat current-path (as-path path)))]
                                        (-> dataflow
                                            (assoc-in [::functions path] function)
                                            (update-value path)
                                            (update-dependant-paths path))))
                                    dataflow
                                    (partition 2 paths-and-functions)))))

#_(defn define-child [path function]
  (let [path (as-path path)
        child-path (apply vector (concat current-path path))]
    (binding [current-path child-path]
      (swap! current-dataflow (fn [dataflow] (-> dataflow
                                                 (define child-path function)
                                                 (assoc-in [::children path] child-path)))))))

(defn undefine [dataflow path]
  (-> dataflow
      (update-in [::functions] dissoc path)
      (update-in [::dependencies] dissoc path)))

(defn apply-to-value [dataflow path function]
  (define dataflow path (function (get-in dataflow (as-path path)))))

(defn get-value [key]
  (println "get value " (conj current-path key))
  (logged-access/get @current-dataflow (conj current-path key)))

(defn get-parent-value [key]
  (println "get parent value " (conj parent-path key))
  (logged-access/get @current-dataflow (conj parent-path key)))

(defn values-to-map [& keys]
  (reduce (fn [result key] (assoc result key (get-value key)))
          {}
          keys))

(defmacro with-values [keys & body]
  `(let [{:keys [~@keys]} (values-to-map ~@(map #(keyword (name %)) keys))]
     ~@body))

#_(defn get-value-in [path]
    (logged-access/get-in @current-dataflow (concat current-path path)))


(defn create []
  {::changed-paths #{}})



#_(deftest define-test
    (is (= (-> (create)

               (define :a #(do (define-child :text "foo")
                               (get-value :text)))

               #_(define
                   :b 1
                   :c 1)

               #_(define :d #(+ (get-value :b)
                                (get-value :c)))

               (print-dataflow)
               (dissoc ::functions))

           nil)))


(run-tests)

(comment
(binding [current-dataflow (atom (create))]
    (define
      :value "jees"
      
      [:a :value] #(get-parent-value :value)
      
      :a #(do (define [:text :value] (fn [] (get-parent-value :value)))
              (define [:text] (fn [] (str "text value: " (get-value :value))))
              (str "A value: " (get-value :text))))
    
    (print-dataflow @current-dataflow)))

