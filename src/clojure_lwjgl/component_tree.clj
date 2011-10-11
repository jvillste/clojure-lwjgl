(ns clojure-lwjgl.component-tree)

(defn initialize [gui root-control]
  (assoc gui :root-control root-control))

(defn handle-input [visual-list input-state]
  (let [new-visuals (map (fn [visual]
                           (visual/handle-input visual input-state))
                         (:visuals visual-list))]

    (-> visual-list
        (update-visuals 0
                        (:visuals visual-list)
                        new-visuals)
        (assoc :visuals new-visuals))))

