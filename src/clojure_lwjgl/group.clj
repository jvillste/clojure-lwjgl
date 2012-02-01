(ns clojure-lwjgl.group
  (:require (clojure-lwjgl [component :as component])))



(defrecord Group [components])

(defn create [& components] (Group. components))

(defn get-visuals [group]
  (map component/get-visuals (:components group)))

(extend Group
  component/Component
  {:get-visuals get-visuals})
