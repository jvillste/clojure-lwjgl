(ns clojure-lwjgl.visual-list
  (:require (clojure-lwjgl [image-list :as image-list]
                           [visual :as visual]
                           [free-layout :as free-layout]
                           [visual :as visual])))

(defrecord VisualList [visuals image-list])

(defn- render-visual [visual-list index visual]
  (visual/render (free-layout/layout visual
                                     0
                                     0)
                 (image-list/get-graphics (:image-list visual-list)
                                          index))
  visual-list)

(defn add-visual [visual-list visual]
  (let [index (image-list/next-index (:image-list visual-list))
        visual (free-layout/layout (visual/get-visual visual) 0 0)]

    (-> visual-list
        (assoc :visuals (conj (:visuals visual-list)
                                 visual)
               :image-list (image-list/add-image (:image-list visual-list)
                                                 (:x visual)
                                                 (:y visual)
                                                 (:width visual)
                                                 (:height visual)))
        (render-visual index visual))))

(defn- visual-index [visual-list visual]
  (.indexOf (:visuals visual-list) visual))


(defn update-visual-position [visual-list index visual new-visual]
  (assoc visual-list :image-list (if (not (and (= (:x visual)
                                                        (:x new-visual))
                                                     (= (:y visual)
                                                        (:y new-visual))))
                                         (image-list/move-image (:image-list visual-list)
                                                                index
                                                                (:x new-visual)
                                                                (:y new-visual))
                                         (:image-list visual-list))))

(defn update-visual-dimensions [visual-list index visual new-visual]
  (assoc visual-list :image-list (if (not (and (= (:width visual)
                                                        (:width new-visual))
                                                     (= (:height visual)
                                                        (:height new-visual))))
                                         (image-list/resize-image (:image-list visual-list)
                                                                  index
                                                                  (:x new-visual)
                                                                  (:y new-visual))
                                         (:image-list visual-list))))

(defn update-visual-image [visual-list index visual new-visual]
  (if (not (= (dissoc visual :x :y)
              (dissoc new-visual :x :y)))
    (render-visual visual-list index new-visual)
    visual-list))

(defn update-visuals [visual-list index old-visuals new-visuals]
  (let [visual (first old-visuals)
        new-visual (first new-visuals)]

    (if (and visual new-visual)
      (-> visual-list
          (update-visual-position index visual new-visual)
          (update-visual-dimensions index visual new-visual)
          (update-visual-image index visual new-visual)
          (recur (+ index 1)
                 (rest old-visuals)
                 (rest new-visuals)))
      visual-list)))

(defn handle-input [visual-list input-state]
  (let [new-visuals (map (fn [visual]
                              (visual/handle-input visual input-state))
                            (:visuals visual-list))]

    (-> visual-list
        (update-visuals 0
                           (:visuals visual-list)
                           new-visuals)
        (assoc :visuals new-visuals))))

(defn draw [gui]
  (image-list/draw (:image-list (::visual-list gui))))


(defn initialize [gui]
  (assoc gui
    ::visual-list (VisualList. []
                                           (image-list/create))
    :drawers (conj (:drawers gui) draw)))