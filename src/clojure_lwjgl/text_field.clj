(ns clojure-lwjgl.text-field
  (:require (clojure-lwjgl [text :as text]
                           [component :as component]
                           [event-queue :as event-queue])))

(defrecord TextField [content])

(defn create [gui content]
  (TextField. content))


(defn handle-key-pressed-event [text-field event]
  (assoc text-field :content (str (:content text-field)
                                  (:character event))))

(defn get-event-handlers
  {:key-pressed handle-key-pressed-event})

(defn get-visual [text-field]
  (text/create (:content text-field)))

(extend TextField
  component/Component
  {:get-visual get-visual
   :get-event-handlers get-event-handlers})



