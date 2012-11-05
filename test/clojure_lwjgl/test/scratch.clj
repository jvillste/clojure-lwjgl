(ns clojure-lwjgl.test.scratch)

(defprotocol Foo
  (do-it [foo]))

(defrecord Bar [x]
    Foo
    (do-it [bar] x))

(println (do-it (assoc (->Bar 3) :x 9 )))

(println (loop [x [1 2 3]
                result []]
           (if (first x)
             (recur (rest x)
                    (conj result (+ 1 (first x))))
             result)))


(in-ns 'clojure-lwjgl.test.dataflow)

(binding [current-dataflow (atom (create))]
    (define
      :value "jees"
      
      [:a :value] #(get-parent-value :value)
      
      :a #(do (define [:text :value] (fn [] (get-parent-value :value)))
              (define [:text] (fn [] (str "text value: " (get-value :value))))
              (str "A value: " (get-value :text))))
    
    (define :value "jees2")
    
    (println (strip @current-dataflow))
    #_(println (::children @current-dataflow)))