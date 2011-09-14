(ns clojure-lwgl.test)


(defn fizzbuzz [x]
  (if (or (= x 3)
          (= x 6))
    "fizz"
    (if (mod x 5)
      "buzz"
      x)))


(def test-suite
  [{:name "fizzbuzz_should_return_number_if_value_is_not_multiple_of_3_or_5"
    :result (= 2 (fizzbuzz 2))}
   {:name "fizzbuzz_should_return_fizz_if_number_is_multiple_of_3"
    :result [(= "fizz" (fizzbuzz 3)) (= "fizz" (fizzbuzz 6))]}
   {:name "fizzbuzz_should_return_buzz_if_number_is_multiple_of_5"
    :result (map #(= "buzz"(fizzbuzz %))
                 (range 5 50 5))}
   {:name "fizzbuzz_should_return_fizzbuzz_if_value_is_multiple_of_3_or_5"
    :result (= "fizzbuzz" (fizzbuzz 15))}])

(doseq [test test-suite]
  (println (:name test) (:result test)))





