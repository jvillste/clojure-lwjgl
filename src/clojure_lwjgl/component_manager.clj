(ns clojure-lwjgl.component-manager
  (:require (clojure-lwjgl [image-list :as image-list]
                           [component :as component]
                           [free-layout :as free-layout]
                           [visual :as visual])))

(defrecord ComponentManager [components image-list])

(defn create []
  (ComponentManager. []
                     (image-list/create)))

(defn- render-component [component-manager index component]
  (image-list/clear-image (:image-list component-manager)
                          index)
  (visual/render (free-layout/layout (component/get-visual component)
                                     0
                                     0)
                 (image-list/get-graphics (:image-list component-manager)
                                          index))
  component-manager)

(defn add-component [component-manager component]
  (let [index (image-list/next-index (:image-list component-manager))
        visual (free-layout/layout (component/get-visual component) 0 0)]

    (-> component-manager
        (assoc :components (conj (:components component-manager)
                                 component)
               :image-list (image-list/add-image (:image-list component-manager)
                                                 (:x visual)
                                                 (:y visual)
                                                 (:width visual)
                                                 (:height visual)))
        (render-component index component))))

(defn- component-index [component-manager component]
  (.indexOf (:components component-manager) component))


(defn update-component-position [component-manager index component new-component]
  (assoc component-manager :image-list (if (not (and (= (:x component)
                                                        (:x new-component))
                                                     (= (:y component)
                                                        (:y new-component))))
                                         (image-list/move-image (:image-list component-manager)
                                                                index
                                                                (:x new-component)
                                                                (:y new-component))
                                         (:image-list component-manager))))

(defn update-component-dimensions [component-manager index component new-component]
  (assoc component-manager :image-list (if (not (and (= (:width component)
                                                        (:width new-component))
                                                     (= (:height component)
                                                        (:height new-component))))
                                         (image-list/resize-image (:image-list component-manager)
                                                                  index
                                                                  (:width new-component)
                                                                  (:height new-component))
                                         (:image-list component-manager))))

(defn update-component-image [component-manager index component new-component]
  (if (not (= (dissoc component :x :y)
              (dissoc new-component :x :y)))
    (render-component component-manager index new-component)
    component-manager))

(defn update-components [component-manager index old-components new-components]
  (let [component (first old-components)
        new-component (first new-components)]

    (if (and component new-component)
      (-> component-manager
          (update-component-position index component new-component)
          (update-component-dimensions index component new-component)
          (update-component-image index component new-component)
          (recur (+ index 1)
                 (rest old-components)
                 (rest new-components)))
      component-manager)))

(defn handle-input [component-manager input-state]
  (let [new-components (map (fn [component]
                              (component/handle-input component input-state))
                            (:components component-manager))]

    (-> component-manager
        (update-components 0
                           (:components component-manager)
                           new-components)
        (assoc :components new-components))))

(defn draw [component-manager]
  (image-list/draw (:image-list component-manager)))