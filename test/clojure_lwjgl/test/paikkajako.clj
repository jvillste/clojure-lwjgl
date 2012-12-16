(ns clojure-lwjgl.test.paikkajako
  (:require (clojure [string :as string])))

(defn name [texts]
  (let [text (first texts)]
    (subs text (- (count text)
                  120)
          (count text))))

(defn places [texts]
  (-> (second texts)
      (string/split #"(<text:list-item>)|(</text:list-item>)")))

(defn texts []
  (->> (-> (slurp "/home/jukka/Downloads/content.xml")
           (subs 14000)
           (string/split #"(<text:list )|(text-list>)")
           (rest))
       (partition 2)))

(defn names []
  (map name (texts)))


(comment
  (doseq [name (names)]
    (println name))

(println (last (names)))
  )
