(ns flow-gl.dataflow
  (:require [flow-gl.logged-access :as logged-access]
            [flow-gl.debug :as debug]
            clojure.set
            [slingshot.slingshot :as slingshot]
            [clojure.data.priority-map :as priority-map])
  (:use clojure.test
        flow-gl.threading))

;; DEPENDANTS

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


;; DEBUG

(defn function-to-string [dataflow key]
  (str key " (height: " (get-in dataflow [::heights key]) ") = " (if (contains? dataflow key)
                                                                   #_(apply str (take 100 (str (get dataflow key))))
                                                                   (str (get dataflow key))
                                                                   "UNDEFINED!")
       (if (empty? (get-in dataflow [::dependencies key]))
         ""
         (str " depends on " (reduce (fn [string key]
                                       (str string " " key (if (contains? dataflow key)
                                                             ""
                                                             " = UNDEFINED! ")))
                                     ""
                                     (get-in dataflow [::dependencies key]))))))

(defn describe-functions [dataflow functions]
  (for [function functions]
    (function-to-string dataflow function)))

(defn describe-dataflow [dataflow]
  (describe-functions dataflow
                      (sort (keys (::functions dataflow)))))

(defn describe-dataflow-undefined [dataflow]
  (describe-functions dataflow
                      (filter #(not (contains? dataflow %))
                              (sort (keys (::functions dataflow))))))


(defn dependency-tree-for-path [dataflow path depth]
  (into [(str (apply str (take depth (repeat "  ")))
              (str path " = " (if (contains? dataflow path)
                                (apply str (take 100 (str (get dataflow path))))
                                #_(str (get dataflow path))
                                "UNDEFINED!")))]
        (mapcat #(dependency-tree-for-path dataflow % (inc depth))
                (get-in dataflow [::dependencies path]))))


(defn dependency-tree [dataflow]
  (mapcat #(dependency-tree-for-path dataflow % 0)
          (filter #(empty? (dependants dataflow %))
                  (sort (keys (::functions dataflow))))))


(defn debug-dataflow [dataflow]
  (debug/debug-all :dataflow (describe-dataflow dataflow))
  dataflow)

;; UTILITIES

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


;; STATE

(def ^:dynamic current-dataflow)

(def ^:dynamic current-path [])

(def ^:dynamic parent-path [])

;; PATHS

(defn as-path [keyword-or-path]
  (if (instance? java.util.Collection keyword-or-path)
    (vec keyword-or-path)
    [keyword-or-path]))

(defn absolute-path [local-path-or-key]
  (vec (concat current-path (as-path local-path-or-key))))


;; CREATE

(defn create []
  {::changed-paths #{}
   ::children {}
   ::need-to-be-updated (priority-map/priority-map)
   ::heights {}})



;; DEFINE

(defn height [dataflow dependencies]
  (+ 1
     (apply max (-> (map #(get-in dataflow [::heights %] 0)
                         dependencies)
                    (conj -1)))))

(defn undefine [dataflow path]
  (let [path (as-path path)]
    (debug/debug :dataflow "undefining" path)
    (-> (reduce (fn [dataflow child]
                  (undefine dataflow child))
                dataflow
                (get-in dataflow [::children path]))
        (update-in [::functions] dissoc path)
        (update-in [::dependencies] dissoc path)
        (update-in [::children] dissoc path)
        (update-in [::changed-paths] conj path)
        (dissoc path))))

(defn undefine-many [dataflow paths]
  (reduce (fn [dataflow path]
            (undefine dataflow path))
          dataflow
          paths))

(defn declare-changed [dataflow path]
  (update-in dataflow [::changed-paths] conj path))

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

      (-> @new-dataflow
          (undefine-many children-to-be-undefined)
          (when-> (not (= new-value ::undefined))
                  (assoc path new-value))
          (assoc-in [::dependencies path] @logged-access/reads)
          ((fn [dataflow]
             (assoc-in dataflow [::heights path] (height dataflow @logged-access/reads))))
          (when-> changed (declare-changed path))
          (when-> (= new-value ::undefined)
                  ((fn [dataflow]
                     (flow-gl.debug/debug :dataflow "Warning: " (function-to-string dataflow path))
                     dataflow)))))))

(defn is-defined? [dataflow path]
  (contains? (::functions dataflow)
             path))

(defn schedule-for-update [dataflow path]
  (assoc-in dataflow [::need-to-be-updated path] (get-in dataflow [::heights path])))




(defn define-to
  ([dataflow path function & paths-and-functions]
     (apply define-to (define-to dataflow path function)
            paths-and-functions))

  ([dataflow path function]
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
                             (do
                               (flow-gl.debug/debug :dataflow "defined " path " = " (apply str (take 100 (str (get dataflow path)))))
                               (reduce schedule-for-update
                                       dataflow
                                       (dependants dataflow path)))
                             dataflow)))))))



(defn define [& paths-and-functions]
  (swap! current-dataflow (fn [dataflow]
                            (apply define-to dataflow paths-and-functions))))

(defn initialize [& paths-and-functions]
  (let [paths-and-functions (->> paths-and-functions
                                 (partition 2)
                                 (map (fn [[path function]]
                                        (swap! current-dataflow (fn [dataflow]
                                                                  (update-in dataflow [::children] #(multimap-add % current-path (absolute-path path)))))
                                        [path function]))
                                 (filter (fn [[path function]]
                                           (not (contains? @current-dataflow (absolute-path path)))))
                                 (apply concat))]
    (when (not (empty? paths-and-functions))
      (apply define paths-and-functions))))

(defn define-with-prefix [prefix & paths-and-functions]
  (let [prefix (as-path prefix)
        add-prefix (fn [[path function]]
                     [(concat prefix (as-path path))
                      function])]
    (apply define (->> paths-and-functions
                       (partition 2)
                       (map add-prefix)
                       (apply concat)))))


;; UPDATE


(defn changes [dataflow]
  (::changed-paths dataflow))

(defn reset-changes [dataflow]
  (assoc dataflow
    ::changed-paths #{}))

(defn propagate-changes [dataflow]
  (flow-gl.debug/debug :dataflow "propagate changes " (vec (map first (::need-to-be-updated dataflow))))
  (let [dataflow (reduce (fn [dataflow [path priority]]
                           (let [old-value (get dataflow path)]
                             (-> dataflow
                                 (update-value path)
                                 (update-in [::need-to-be-updated] dissoc path)
                                 ((fn [dataflow] (if (not (= old-value
                                                             (get dataflow path)))
                                                   (do (flow-gl.debug/debug :dataflow "updated path " path " = " (apply str (take 100 (str (get dataflow path)))))
                                                       (reduce schedule-for-update dataflow (dependants dataflow path)))
                                                   dataflow))))))
                         dataflow
                         (::need-to-be-updated dataflow))]
    (if (empty? (::need-to-be-updated dataflow))
      dataflow
      (recur dataflow))))

(comment

  (do (debug/reset-log)
      (-> (create)
          (define-to
            :a 1
            :b #(+ 1
                   (get-global-value :a))
            :c #(+ 1
                   (get-global-value :b))
            :d 1
            :e #(+ (get-global-value :b)
                   (get-global-value :d)))

          (define-to :d 2)
          (define-to :a 3)

          (propagate-changes)
          ((fn [dataflow]
             (debug/debug-all (describe-dataflow dataflow)))))
      (debug/write-log)))



;; ACCESS

(defn get-global-value-from [dataflow path]
  (logged-access/get dataflow (as-path path)))

(defn get-global-value [path]
  (let [path (as-path path)]
    (if (contains? @current-dataflow path)
      (get-global-value-from @current-dataflow path)
      (do (logged-access/add-read path)
          (slingshot/throw+ {:type ::undefined-value} (str "Undefined value: " path))))))

(defn get-value [path-or-key]
  (get-global-value (absolute-path path-or-key)))

(defn get-value-or-initialize [path-or-key default]
  (when (not (is-defined? @current-dataflow (absolute-path path-or-key)))
    (initialize path-or-key default))
  (get-value path-or-key))

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


(defn property [element-path key]
  (get-global-value (concat element-path (as-path key))))

(defn property-from [dataflow element-path key]
  (get-global-value-from dataflow (concat element-path (as-path key))))

(defn define-property-to [dataflow element-path key value]
  (define-to dataflow (concat element-path (as-path key)) value))

(defn define-property [element-path key value]
  (define (concat element-path [key]) value))


;; TESTS

(deftest describe-dataflow-test
  (is (= (-> (create)
             (define-to :foo 2)
             (define-to :foo2 #(+ (get-global-value :foo)
                                  2))
             (describe-dataflow))
         '("[:foo] (height: 0) = 2"
           "[:foo2] (height: 1) = 4 depends on  [:foo]"))))


(deftest dependency-tree-for-path-test
  (is (= (-> (create)
             (define-to :foo 2)
             (define-to :foo2 #(+ (get-global-value :foo)
                                  2))
             (dependency-tree-for-path [:foo2] 0))
         ["[:foo2] = 4"
          "  [:foo] = 2"])))


(deftest dependency-tree-test
  (is (= (-> (create)
             (define-to :foo 2)
             (define-to :foo2 #(+ (get-global-value :foo)
                                  2))

             (define-to :foo3 #(+ (get-global-value :foo)
                                  2))

             (define-to :foo4 #(+ (get-global-value :foo3)
                                  2))
             (dependency-tree))
         '("[:foo2] = 4"
           "  [:foo] = 2"
           "[:foo4] = 6"
           "  [:foo3] = 4"
           "    [:foo] = 2"))))


;; TESTS

(deftest propagate-test
  (is (= (-> (create)
             (define-to
               :a 1

               :b #(+ 1
                      (get-global-value :a))

               :c #(+ 1
                      (get-global-value :a))

               :d #(+ (get-global-value :b)
                      (get-global-value :c)))

             (define-to :a 2)

             (propagate-changes)
             (debug-dataflow)
             (get-value-from :d))
         6)))

(deftest children-test
  (is (= (-> (create)
             (define-to
               :a #(do (initialize :b 1)
                       (get-value :b)))

             (define-to [:a :b] 2)

             (propagate-changes)
             (debug-dataflow)

             (get-value-from :a))
         2)))


(deftest dynamic-reconfiguration-test
  (let [dataflow (-> (create)
                     (define-to
                       :a 1
                       :b 2
                       :c #(+ (get-global-value :b)
                              1)
                       :d #(if (= (get-global-value :a)
                                  1)
                             (get-global-value :b)
                             (get-global-value :c))
                       :e #(+ (get-global-value :d)
                              1))

                     (debug-dataflow)

                     (define-to :a 2)

                     (propagate-changes)
                     (debug-dataflow))]
    (are [x y] (= x y)
         3 (get-value-from dataflow :d)
         4 (get-value-from dataflow :e)
         2 (get-in dataflow [::heights [:d]])
         3 (get-in dataflow [::heights [:e]]))))

(deftest undefined-test
  (let [dataflow (-> (create)
                     (define-to
                       :a #(+ (get-global-value :b)
                              1))

                     (debug-dataflow)

                     (define-to
                       :b #(+ (get-global-value :c)
                              1))

                     (define-to :c 1)

                     (propagate-changes)
                     (debug-dataflow))]
    (are [x y] (= x y)
         3 (get-value-from dataflow :a))))

(comment

  (do
    (debug/set-active-channels #_:view-definition
                               #_:initialization
                               :dataflow
                               #_:events
                               #_:view-update
                               #_:default)
    (debug/reset-log)
    (undefined-test)
    (debug/write-log))

  (do (debug/reset-log)
      (-> (create)
          (define-to
            :a #(do (initialize :b 1)
                    (get-value :b)))

          (define-to [:a :b] 2)

          (propagate-changes)
          (debug-dataflow))
      (debug/write-log))

  (-> {:p (priority-map/priority-map)}
      (assoc-in [:p :a] 3)
      (assoc-in [:p :b] 1)
      (assoc-in [:p :c] 2)
      (get :p)
      (seq)))



(run-tests)
