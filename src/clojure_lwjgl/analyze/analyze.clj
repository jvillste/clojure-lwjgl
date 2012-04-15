(ns clojure-lwjgl.analyze.analyze
  (:require [analyze.core :as analyze]))

(def analyzed
  (binding [analyze/*children* true]
    (doall (map #(apply analyze/analyze-path %)
                '[["clojure_lwjgl/analyze/analyze_test.clj" clojure-lwjgl.analyze.analyze-test]]))))

(defn visit-tree [f expr]
  [(f expr) (for [child (:children expr)]
              (visit-tree f child))])


(defn analyze [expr]
  (cond
   (= (:op expr)
      :def)
   {:name (-> expr :var meta :name)
     :children (map #(visit-tree :op %) (:children expr))}))

(comment
  (clojure.pprint/pprint (binding [analyze/*children* true]
                           (analyze/analyze-one {:ns {:name 'clojure.repl} :context :eval}
                                                '(+ 1 1))))

  (defn bar [x] x)

  (->> (analyze/analyze-form '(defn foo [x] (clojure.pprint/pprint x)))
       clojure.pprint/pprint)

(->> analyzed
       first
       (map analyze)
       clojure.pprint/pprint
       )

  )

