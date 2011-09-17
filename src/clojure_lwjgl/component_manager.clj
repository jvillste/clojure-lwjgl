(ns clojure-lwjgl.component-manager
  (:require (clojure-lwjgl [image-list :as image-list]
                           [component :as component]
                           [visual :as visual])
            [clojure.contrib.seq :as seq]))

(defrecord ComponentManager [components image-list])

(defn create []
  (ComponentManager. []
                     (image-list/create)))

(defn add-component [component-manager component]
  (-> component-manager
      (assoc :components
        (conj (:components component-manager)
              component))
      (assoc :image-list
        (image-list/add-image (:image-list component-manager)
                              (:x component)
                              (:y component)
                              (:width component)
                              (:height component)))))

(defn- component-index [component-manager component]
  (first (seq/positions #{component} (:components component-manager))))

(defn- render-component [component-manager component]
  (visual/render (component/get-visual component)
                 (image-list/get-graphics (:image-list component-manager)
                                          (component-index component-manager component)))
  component)

(defn remove-place [component]
  (dissoc component :x :y :width :height))

(defn update-components [component-manager new-components]
  (when (not (= (dissoc component :x :y)
                (dissoc new-component :x :y)))
    (render-component component-manager new-component))
  (when (not (and (= (:x component)
                     (:x new-component))
                  (= (:y component)
                     (:y new-component))))
    (image-list/move-image (:image-list component-manager)
                           index
                           (:x new-component)
                           (:y new-component))))

(defn handle-input [component-manager input-state]
  
  (let [new-components (map (fn [component]
                              (component/handle-input component input-state))
                            (:components component-manager))]

    (map-indexed)
    (-> component-manager
        (assoc :components new-components))))

(defn draw [component-manager]
  (image-list/draw (:image-list component-manager)))