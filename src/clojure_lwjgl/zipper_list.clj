(ns clojure-lwjgl.zipper-list
  (:require [clojure.zip :as zip])
  (:use midje.sweet))

(defn create [] (zip/vector-zip [nil]))

(defn insert-item [zipper-list item-id index]
  (loop [zipper-list (zip/down zipper-list)
         index index]
    (if (> index 0)
      (recur (zip/next zipper-list)
             (- index 1))
      (-> zipper-list
          (zip/insert-right item-id)
          (zip/up)))))

(defn item-index [zipper-list item-id]
  (loop [zipper-list (zip/right (zip/down zipper-list))
         index 0]
    (if (= nil zipper-list)
      nil
      (if (= (zip/node zipper-list)
             item-id)
        index
        (recur (zip/next zipper-list)
               (+ 1 index))))))

(fact "item index grows when items are inserted before it"
  (-> (create)
      (insert-item :item-1 0)
      (insert-item :item-2 0)
      (item-index :item-1))  => 1)



