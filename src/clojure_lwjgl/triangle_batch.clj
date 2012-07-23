(ns clojure-lwjgl.triangle-batch)

(defrecord TriangleBatch [coordinates colors])

(defn create [coordinates colors]
  (->TriangleBatch coordinates colors))

(defn concatenate [triangle-batch1 triangle-batch2]
  (->TriangleBatch (concat (:coordinates triangle-batch1)
                           (:coordinates triangle-batch2))
                   (concat (:colors triangle-batch1)
                           (:colors triangle-batch2))))

(defn number-of-triangles [triangle-batch]
  (/ (count (:coordinates triangle-batch))
     6))