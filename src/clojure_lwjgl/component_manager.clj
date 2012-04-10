(ns clojur-lwjgl.component-manager
  (:require (clojure-lwjgl [zipper-list :as zipper-list]
                           [component :as component])))

(defrecord ComponentManager [components component-ordering])

(defn create []
  (map->ComponentManager {:components {}
                          :component-ordering (zipper-list/create)}))

(defn add-component [component-manager id component]
  (-> component-manager
      (update-in [:components] assoc id component)
      (update-in [:component-ordering] zipper-list/add id)))