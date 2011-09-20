(ns clojure-lwjgl.text-field
  (:require (clojure-lwjgl [text :as text]
                           [component :as component])))

(defrecord TextField [content])

(defn create [content] (TextField. content))

(defn get-visual [text-field]
  (text/create (:content text-field)))

(defn handle-input [text-field input-state]
  (when (= (:type (:last-event input-state))
           :key-pressed)
    (assoc text-field :content
      (conj (:content text-field)
            (:key-character (:last-event input-state))))))

(extend TextField
  component/Component
  {:get-visual get-visual
   :handle-input handle-input})



