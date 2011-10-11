(ns clojure-lwjgl.text-field
  (:require (clojure-lwjgl [text :as text]
                           [component :as component]
                           [event-queue :as event-queue])))

(defrecord TextField [content])


(defn handle-button-pressed-event [text-field event]
  (if (= (:type (:last-event input-state))
         :key-pressed)
    (do (println "handling input " input-state)
        (assoc text-field :content (str ;(:content text-field)
                                        (:character (:last-event input-state)))))
    text-field))

(defn create [gui content]
  (TextField. content))

(defn get-visual [text-field]
  (text/create (:content text-field)))

(extend TextField
  component/Component
  {:get-visual get-visual
   :handle-input handle-input})



