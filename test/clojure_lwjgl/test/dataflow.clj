(ns clojure-lwjgl.test.dataflow
  (:require (clojure-lwjgl [logged-access :as logged-access])
            clojure.set
            [slingshot.slingshot :as slingshot])
  (:use clojure.test))

(defmacro when-> [argument condition body]
  `(if ~condition
     (-> ~argument ~body)
     ~argument))

(defn multimap-add
  "Adds key-value pairs to the multimap."
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
  (doseq [key (sort (filter vector? (keys (strip dataflow))))]
    (println (str key " = " (get dataflow key) " depends on " (get-in dataflow [::dependencies key]))))

  #_(println "Dependencies " (::dependencies dataflow))
  (println "Changes " (::changed-paths dataflow))
  dataflow)

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

(defn undefine [dataflow path]
  #_(println "undefinig " path)
  (-> (reduce (fn [dataflow child]
                (undefine dataflow child))
              dataflow
              (get-in dataflow [::children path]))
      (update-in [::functions] dissoc path)
      (update-in [::dependencies] dissoc path)
      (update-in [::children] dissoc path)
      (update-in [::changed-paths] conj path)
      (dissoc path)))

(defn undefine-many [dataflow paths]
  (reduce (fn [dataflow path]
            (undefine dataflow path))
          dataflow
          paths))

(defn assoc-if-defined [dataflow path value]
  (if (not (= value ::undefined))
    (assoc dataflow path value)
    dataflow))

(defn update-value [dataflow path]

  (logged-access/with-access-logging
    (let [old-children (get-in dataflow [::children path])
          new-dataflow (atom (assoc-in dataflow [::children path] #{}))
          old-value (get dataflow path)
          new-value (slingshot/try+ (binding [parent-path current-path
                                              current-path path
                                              current-dataflow new-dataflow]
                                      ((get-in dataflow [::functions path])))
                                    (catch [:type ::undefined-value] _
                                      ::undefined))
          changed (not (= old-value new-value))
          new-children (get-in @new-dataflow [::children path])
          children-to-be-undefined (if (= new-value ::undefined)
                                     #{} #_new-children
                                     (clojure.set/difference old-children new-children))]
      #_(println "Updating " path " = " new-value #_(apply str (take 100 (str new-value))))
      (-> @new-dataflow
          (undefine-many children-to-be-undefined)
          (assoc-if-defined path new-value)
          (assoc-in [::dependencies path] @logged-access/reads)
          (when-> changed (update-in [::changed-paths] conj path))))))

(defn changes [dataflow]
  (::changed-paths dataflow))

(defn reset-changes [dataflow]
  (assoc dataflow
    ::changed-paths #{}))

(defn update-dependant-paths [dataflow path]
  (reduce (fn [dataflow dependant-path]
            (if (not (= dependant-path current-path))
              (do
                #_(println "updating dependant path " dependant-path " of " path)
                (let [old-value (get dataflow dependant-path)]
                  (-> dataflow
                      (update-value dependant-path)
                      ((fn [dataflow] (if (not (= old-value
                                                  (get dataflow dependant-path)))
                                        (update-dependant-paths dataflow dependant-path)
                                        dataflow))))))
              dataflow))
          dataflow
          (dependants dataflow path)))

(defn as-path [keyword-or-path]
  (if (instance? java.util.Collection keyword-or-path)
    (vec keyword-or-path)
    [keyword-or-path]))

(defn absolute-path [local-path-or-key]
  (vec (concat current-path (as-path local-path-or-key))))

(defn define-to [dataflow & paths-and-functions]
  (reduce (fn [dataflow [path function]]
            (let [function (if (fn? function)
                             function
                             (fn [] function))
                  path (absolute-path path)
                  old-value (get dataflow path)]
              (-> dataflow
                  (assoc-in [::functions path] function)
                  (update-in [::children] #(multimap-add % current-path path))
                  (update-value path)
                  ((fn [dataflow] (if (not (= old-value
                                              (get dataflow path)))
                                    (update-dependant-paths dataflow path)
                                    dataflow))))))
          dataflow
          (partition 2 paths-and-functions)))

(defn define [& paths-and-functions]
  (swap! current-dataflow (fn [dataflow]
                            (apply define-to dataflow paths-and-functions))))

(defn initialize [& paths-and-functions]
  (apply define (->> paths-and-functions
                     (partition 2)
                     (map (fn [[path function]]
                            (swap! current-dataflow (fn [dataflow]
                                                      (update-in dataflow [::children] #(multimap-add % current-path (absolute-path path)))))
                            [path function]))
                     (filter (fn [[path function]]
                               (not (contains? @current-dataflow (absolute-path path)))))
                     (apply concat))))

(defn define-with-prefix [prefix & paths-and-functions]
  (let [prefix (as-path prefix)
        add-prefix (fn [[path function]]
                     [(concat prefix (as-path path))
                      function])]
    (apply define (->> paths-and-functions
                       (partition 2)
                       (map add-prefix)
                       (apply concat)))))

(defn get-global-value-from [dataflow path]
  #_(println "Get " path)
  (let [path (as-path path)]
    (if (contains? dataflow path)
      (logged-access/get dataflow path)
      (do (logged-access/add-read path)
          (slingshot/throw+ {:type ::undefined-value} (str "Undefined value: " path))))))

(defn get-global-value [path]
  (get-global-value-from @current-dataflow path))

(defn get-value [path-or-key]
  (get-global-value (absolute-path path-or-key)))

(defn get-value-from [dataflow path-or-key]
  (get-global-value-from dataflow (absolute-path path-or-key)))

(defn get-parent-value [key]
  (get-global-value (apply vector (conj parent-path key))))

(defn bind [target source]
  (define target #(get-global-value source)))

(defn apply-to-value [dataflow path function]
  (define-to dataflow path (function (get dataflow (absolute-path path)))))

(defn values-to-map [& keys]
  (reduce (fn [result key] (assoc result key (get-value key)))
          {}
          keys))

(defmacro with-values [keys & body]
  `(let [{:keys [~@keys]} (values-to-map ~@(map #(keyword (name %)) keys))]
     ~@body))

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

  (-> (create)
      (define-to
        [:value 1] "Foo1"
        [:value 2] "Foo2"
        [:value 3] "Foo3"
        [:order] [1 3 2]

        :a #(do (doseq [i (get-global-value [:order])]
                  #_(bind [:text i :value] [:value i])
                  (define [:text i] (fn [] (str "text value: " (get-global-value [:value i])))))
                (apply vector (map (fn [i] (get-value [:text i]))
                                   (get-global-value [:order])))))

      (print-dataflow)
      (reset-changes)
      #_(define-to [:value 2] "Foo2")
      (define-to [:order] [1 2])

      (print-dataflow)



      #_(print-dataflow))

  (-> (create)
      (define-to :a (fn [] (define :a1 (fn [] (define :a2 1)
                                         (inc (get-value :a2))))
                      (inc (get-value :a1))))
      (define-to [:a :a1 :a2] 2)
      (print-dataflow))

  (-> (create)
      (define-to :a 1)
      (define-to :b #(let [a (get-global-value :a)]
                       (if (= a 3)
                         1
                         2)))
      (define-to :c #(inc (get-global-value :b)))

      (print-dataflow)
      (reset-changes)
      (define-to :a 1)
      (define-to :a 2)
      (define-to :a 3)
      (print-dataflow)))
