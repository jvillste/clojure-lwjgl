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
  (logged-access/with-access-logging
    (-> dataflow
        (assoc-in path (with-dataflow dataflow ((get-in dataflow [::functions path]))))
        (assoc-in [::dependencies path] @logged-access/reads)
        (update-in [::changed-paths] conj path))))

(defn dl [value message]
  (println message " " value)
  value)

(defn unnotified-changes? [dataflow]
  (not (empty? (::changed-paths dataflow))))

(defn notify-listeners [dataflow]
  (println "notifying changes to " (::changed-paths dataflow))
  (reduce (fn [dataflow changed-path]
            (-> (if-let [listener (get-in dataflow [::listeners changed-path])]
                  (listener dataflow)
                  dataflow)
                (update-in [::changed-paths] disj changed-path)))
          dataflow
          (::changed-paths dataflow)))

(defn update-dependant-paths [dataflow path]
  (println "updating dependencies for " path)
  (reduce (fn [dataflow dependant-path]
            (println "updating " dependant-path)
            (-> dataflow
                (update-value dependant-path)
                (update-dependant-paths dependant-path)))
          dataflow
          (dependants dataflow path)))

(defn define [dataflow & paths-and-functions]
  (reduce (fn [dataflow [path function]]
            (println "defining " path)
            (let [function (if (fn? function)
                             function
                             (fn [] function))
                  path (if (vector? path)
                         path
                         [path])]
              (-> dataflow
                  (assoc-in [::functions path] function)
                  (update-value path)
                  (update-dependant-paths path))))
          dataflow
          (partition 2 paths-and-functions)))

(defn apply-to-value [dataflow path function]
  (define dataflow path (function (get-in dataflow path))))

(defn get-value [key]
  (logged-access/get dataflow key))

(defn values-to-map [& keys]
  (reduce (fn [result key] (assoc result key (get-value key)))
          {}
          keys))

(defmacro with-values [keys & body]
  `(let [{:keys [~@keys]} (values-to-map ~@(map #(keyword (name %)) keys))]
     ~@body))

(defn get-value-in [path]
  (logged-access/get-in dataflow path))

(defn add-listener [dataflow path listener]
  (assoc-in dataflow [::listeners path] listener))

(defn create []
  {::changed-paths #{}})

(deftest define-test
  (is (= (-> (create)
             (add-listener [:a] #(do (println ":a changed to " (:a %))
                                     %))

             (define
               :path "Foo"
               [:d :e] 2
               :b 1
               :c 1
               :a #(with-values [b c]
                     (+ 1 b c)))
             (notify-listeners)
             (define :b 2)
             (notify-listeners)

             (dissoc ::functions)
             (dissoc ::listeners))

         {:a 4
          :b 2
          :c 1
          :clojure-lwjgl.test.dataflow/dependencies {[:a] #{[:c] [:b]}
                                                     [:c] #{}
                                                     [:b] #{}}})))


(defn background [])
(defn foreground [])
(defn preview [])

(deftest define-test2
  (is (= (-> (create)
             (define
               :archive-path "/home/jukka/Pictures/dia"
               [:view-parts :bacground] background
               [:view-parts :foreground] foreground
               [:view-parts :preview] preview
               :view [:background
                      :preview
                      :foreground]
               :width  10
               :height  10
               :document-number 0
               :page-number 0
               :status "Started"
               :view-part-command-runners {}
               :scheduled-threads #{})
             (add-listener [:view-parts :bacground] #(do (println "backgournd changed")
                                                         %))
             (notify-listeners))

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
                              (get-value :b)
                              (get-value :c)))
               (define :b 2)
               (dissoc ::functions)))
  )

