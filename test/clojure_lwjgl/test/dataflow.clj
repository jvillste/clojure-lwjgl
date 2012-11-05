(ns clojure-lwjgl.test.dataflow
  (:require (clojure-lwjgl [logged-access :as logged-access])
            clojure.set)
  (:use clojure.test))


(defn multimap-add
  "Adds key-value pairs the multimap."
  ([mm k v]
     (assoc mm k (conj (get mm k #{}) v)))
  ([mm k v & kvs]
     (apply multimap-add (multimap-add mm k v) kvs)))

(defn multimap-del
  "Removes key-value pairs from the multimap."
  ([mm k v]
     (assoc mm k (disj (get mm k) v)))
  ([mm k v & kvs]
     (apply multimap-del (multimap-del mm k v) kvs)))

(defn strip [dataflow]
  (dissoc dataflow
          ::children
          ::functions
          ::dependencies
          ::changed-paths))

(defn print-dataflow [dataflow]
  (doseq [key (sort (keys (strip dataflow)))]
    (println (str key " = " (get dataflow key) " depends on " (get-in dataflow [::dependencies key])))))

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
  #_(println "updating " path)
  (logged-access/with-access-logging
    (let [new-dataflow (atom dataflow)
          new-value (binding [parent-path current-path
                              current-path path
                              current-dataflow new-dataflow]
                      ((get-in dataflow [::functions path])))]
      (println "updating " path " with " new-value " was " (get dataflow path))
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
            (if (not (= dependant-path current-path))
              (do
                (println "updating dependant path " dependant-path " of " path " parent " parent-path " current path " current-path)
                (-> dataflow
                    (update-value dependant-path)
                    (update-dependant-paths dependant-path)))
              dataflow))
          dataflow
          (dependants dataflow path)))

(defn as-path [keyword-or-path]
  (if (vector? keyword-or-path)
    keyword-or-path
    [keyword-or-path]))

(defn undefine [dataflow path]
  (println "undefinig " path)
  (-> (reduce (fn [dataflow child]
                (undefine dataflow child))
              dataflow
              (get-in dataflow [::children path]))
      (update-in [::functions] dissoc path)
      (update-in [::dependencies] dissoc path)
      (update-in [::children] dissoc path)
      (dissoc path)))

(defn define [& paths-and-functions]
  (swap! current-dataflow (fn [dataflow]
                            (reduce (fn [dataflow [path function]]

                                      (let [function (if (fn? function)
                                                       function
                                                       (fn [] function))
                                            path (apply vector (concat current-path (as-path path)))]
                                        (println "defining " path " parent " parent-path " current " current-path " function " function " was " (get-in dataflow [::functions path]))
                                        (-> dataflow
                                            (assoc-in [::functions path] function)
                                            (update-in [::children] #(multimap-add % current-path path))
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



(defn apply-to-value [dataflow path function]
  (define dataflow path (function (get-in dataflow (as-path path)))))

(defn get-value [path-or-key]
  (logged-access/get @current-dataflow (concat current-path (as-path path-or-key))))

(defn get-parent-value [key]
  (logged-access/get @current-dataflow (conj parent-path key)))

(defn absolute-path [local-path-or-key]
  (concat current-path (as-path local-path-or-key)))

(defn get-global-value [path]
  (logged-access/get @current-dataflow path))

(defn bind [target source]
  (define target #(get-global-value source)))

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
  {::changed-paths #{}
   ::children {}})



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
  (defn text [value-path]
    )

(binding [current-dataflow (atom (create))]


    (define
      [:value 1] "Foo1"
      [:value 2] "Foo2"
      [:value 3] "Foo3"
      [:order] [1 3 2]

      :a #(do (doseq [i (get-global-value [:order])]
                #_(bind [:text i :value] [:value i])
                (define [:text i] (fn [] (str "text value: " (get-global-value [:value i])))))
              (apply vector (map (fn [i] (get-value [:text i]))
                                 (get-global-value [:order])))))

    (define [:order] [1 2 3])

    
    (print-dataflow @current-dataflow)

    #_(println (clojure.pprint/pprint (strip @current-dataflow)))

    #_(println (strip (undefine @current-dataflow [:a] )))
    #_(println (strip @current-dataflow))
    #_(println (::children @current-dataflow))
    #_(println (::functions @current-dataflow))))
