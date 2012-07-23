(ns clojure-lwjgl.text-list
  (:require (clojure-lwjgl [image-list :as image-list]
                           [font :as font]
                           [primitive-list :as primitive-list]
                           [primitive :as primitive]))
  
  (:import [java.awt Color  RenderingHints]))

(defrecord Text [x y content font color])

(defrecord TextList [image-list])

(defn draw-text [graphics text]
  (let [[r g b a] (map float (:color text))]
    (doto graphics
      (.setColor (Color. r g b a))
      (.setFont (font/graphics-font (:font text)))
      (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_HBGR )
      (.drawString (:content text) 0 (font/height (:font text) (:content text))))))

(defn create-image-list [texts]
  (reduce (fn [image-list [index text]]
            (-> image-list
                (image-list/add-image index
                                      (:x text)
                                      (:y text)
                                      (font/width (:font text) (:content text))
                                      (font/height (:font text) (:content text)))
                (image-list/draw-on-image index
                                          (fn [graphics] (draw-text graphics text)))))
          (image-list/create)
          (map-indexed vector texts)))

(defn create [texts] (->TextList (create-image-list texts)))

(defn draw [text-list]
  (image-list/draw (:image-list text-list)))

(defn update [text-list texts]
  (image-list/delete (:image-list text-list))
  (assoc text-list :image-list (create-image-list texts)))

(defn delete [text-list]
  (image-list/delete (:image-list text-list)))

(extend Text
  primitive/Primitive
  {:list-creator (fn [primitive] create)})

(extend TextList
  primitive-list/PrimitiveList
  {:create create
   :update update
   :draw draw
   :delete delete})