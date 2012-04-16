(ns clojure-lwjgl.visual-list
  (:require (clojure-lwjgl [image-list :as image-list]
                           [visual :as visual]
                           [free-layout :as free-layout]
                           [event-queue :as event-queue])))

(defrecord VisualList [visuals image-list])

(defn- render-visual [visual-list id visual]
  (update-in visual-list [:image-list] #(image-list/draw-on-image %
                                                                  id
                                                                  (fn [graphics]
                                                                    (visual/render visual
                                                                                   graphics)))))

(defn get-visual [visual-list id]
  (get-in visual-list [:visuals id]))

(defn add-visual [visual-list id visual]
  (-> visual-list
      (assoc :visuals (assoc (:visuals visual-list)
                        id
                        visual)
             :image-list (image-list/add-image (:image-list visual-list)
                                               id
                                               (:x visual)
                                               (:y visual)
                                               (:width visual)
                                               (:height visual)))
      (render-visual id visual)))


(defn update-visual-position [visual-list id visual new-visual]
  (assoc visual-list :image-list (if (not (and (= (:x visual)
                                                  (:x new-visual))
                                               (= (:y visual)
                                                  (:y new-visual))))
                                   (image-list/move-image (:image-list visual-list)
                                                          id
                                                          (:x new-visual)
                                                          (:y new-visual))
                                   (:image-list visual-list))))

(defn update-visual-dimensions [visual-list id visual new-visual]
  (assoc visual-list :image-list (if (not (and (= (:width visual)
                                                  (:width new-visual))
                                               (= (:height visual)
                                                  (:height new-visual))))
                                   (image-list/resize-image (:image-list visual-list)
                                                            id
                                                            (:x new-visual)
                                                            (:y new-visual))
                                   (:image-list visual-list))))

(defn update-visual-image [visual-list id visual new-visual]
  (if (not (= (dissoc visual :x :y)
              (dissoc new-visual :x :y)))
    (render-visual visual-list id new-visual)
    visual-list))

(defn update-visual [visual-list id new-visual]
  (let [old-visual (get-in visual-list [:visuals id])]
    (-> visual-list
        (update-visual-position id old-visual new-visual)
        (update-visual-dimensions id old-visual new-visual)
        (update-visual-image id old-visual new-visual)
        (update-in [:visuals id] new-visual))))


(defn apply-to-visual [visual-list id f]
  (update-visual visual-list id (f (get-in visual-list [:visuals id]))))

(defn draw [visual-list]
  (update-in visual-list [:image-list]  #(image-list/draw %)))

(defn create []
  (VisualList. {}
               (image-list/create)))
