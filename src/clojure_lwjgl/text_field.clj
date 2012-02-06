(ns clojure-lwjgl.text-field
  (:require (clojure-lwjgl [text :as text]
                           [component :as component]
                           [event-queue :as event-queue])
            (clojure-lwjgl.frp [event-stream :as event-stream]
                               [signal :as signal])))

(defrecord TextField [content-signal visual-signal])

;; (defn key-pressed-to-content [key-pressed initial-content]
;;   )

;; (defn create [content key-pressed-event-stream]
  
;;   (TextField. content))


(defn handle-key-pressed-event [text-field event]
  (assoc text-field :content (str (:content text-field)
                                  (:character event))))
(defn get-event-handlers
  {:key-pressed handle-key-pressed-event})

(defn get-visuals [text-field]
  [(text/create (:content text-field))])

(extend TextField
  component/Component
  {:get-visuals get-visuals
   :get-event-handlers get-event-handlers})



