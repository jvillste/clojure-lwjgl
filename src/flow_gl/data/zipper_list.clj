(ns flow-gl.data.zipper-list
  (:refer-clojure :exclude (count remove find))
  (:require [clojure.zip :as zip])
  (:use clojure.test))

(defn create [] (zip/vector-zip [nil]))

(defn insert [zipper-list item-id index]
  (loop [zipper-list (zip/down zipper-list)
         index index]
    (if (> index 0)
      (recur (zip/next zipper-list)
             (- index 1))
      (-> zipper-list
          (zip/insert-right item-id)
          (zip/up)))))

(defn add [zipper-list item-id]
  (-> zipper-list
      (zip/down)
      (zip/rightmost)
      (zip/insert-right item-id)
      (zip/up)))

(defn- find [zipper-list item-id]
  (loop [zipper-list (zip/right (zip/down zipper-list))]
    (if (= nil zipper-list)
      nil
      (if (= (zip/node zipper-list)
             item-id)
        zipper-list
        (recur (zip/next zipper-list))))))

(defn insert-after [zipper-list old-item-id new-item-id]
  (let [item-location (find zipper-list old-item-id)]
    (if (= nil item-location)
      nil
      (-> (zip/insert-right item-location new-item-id)
          (zip/up)))))

(defn remove [zipper-list item-id]
  (let [item-location (find zipper-list item-id)]
    (if (= nil item-location)
      nil
      (-> (zip/remove item-location)
          (zip/up)))))

(defn index [zipper-list item-id]
  (loop [zipper-list (zip/right (zip/down zipper-list))
         index 0]
    (if (= nil zipper-list)
      nil
      (if (= (zip/node zipper-list)
             item-id)
        index
        (recur (zip/next zipper-list)
               (+ 1 index))))))

(defn items [zipper-list]
  (-> zipper-list
      (zip/children)
      (rest)))

(defn count [zipper-list]
  (-> zipper-list
      (zip/node)
      (clojure.core/count)
      (- 1)))

(deftest item-index-grows-when-items-are-inserted-before-it
  (is (= (-> (create)
             (insert :item-1 0)
             (insert :item-2 0)
             (index :item-1))
         1)))

(deftest add-adds-items-as-last-items
  (is (= (-> (create)
             (add :item-1)
             (add :item-2)
             (add :item-3)
             (index :item-3))
         2)))

(deftest item-index-shrinks-when-items-are-removed-before-it
  (is (= (-> (create)
             (insert :item-1 0)
             (insert :item-2 1)
             (insert :item-3 2)
             (remove :item-2)
             (index :item-3))
         1)))

(deftest item-index-must-not-grow-when-items-are-inserted-after-it
  (is (=
       (-> (create)
           (insert :item-1 0)
           (insert :item-2 1)
           (index :item-1))
       0)))

(deftest count-should-return-the-number-of-items-inserted
  (is (= (-> (create)
             (insert :item-1 0)
             (insert :item-2 0)
             (count))
         2)))

(deftest inte-index-should-correspond-to-the-insertion-point
  (is (= (-> (create)
             (add :item-1)
             (add :item-2)
             (insert-after :item-1 :item-3)
             (index :item-3))
         1)))

(deftest items-should-return-the-inserted-items
  (is (= (-> (create)
             (add :item-1)
             (add :item-2)
             (items))
         [:item-1 :item-2])))
