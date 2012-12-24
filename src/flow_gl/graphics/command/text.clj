(ns flow-gl.graphics.command.text
  (:require (flow-gl.graphics [image-list :as image-list]
                              [font :as font]
			[command :as command]))

  (:import [java.awt Color  RenderingHints]))

(defrecord Text [x y content font color])

(defrecord TextList [texts])

(defrecord TextListRunner [image-list])

(defn draw-text [graphics text]
  (let [[r g b a] (map float (:color text))]
    (doto graphics
      (.setColor (Color. r g b a))
      (.setFont (font/graphics-font (:font text)))
      (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_HBGR )
      (.drawString (:content text) 0 (font/ascent (:font text))))))

(defn create-image-list [texts]
  (reduce (fn [image-list [index text]]
            (-> image-list
                (image-list/add-image index
                                      (:x text)
                                      (:y text)
                                      (max 1
                                           (font/width (:font text) (:content text)))
                                      (max 1
                                           (font/height (:font text))))
                (image-list/draw-on-image index
                                          (fn [graphics] (draw-text graphics text)))))
          (image-list/create)
          (map-indexed vector texts)))


(defn combine-text-lists [text-list1 text-list2]
  (->TextList (concat (:texts text-list1)
                      (:texts text-list2))))

(defn create-text-list-runner [text-list]
  (-> (:texts text-list)
      create-image-list
      ->TextListRunner))

(extend TextList
  command/Command
  {:create-runner create-text-list-runner}
  command/CombinableCommand
  {:combine combine-text-lists})

(extend TextListRunner
  command/CommandRunner
  {:delete (fn [text-list-runner] (image-list/delete (:image-list text-list-runner)))
   :run (fn [text-list-runner] (image-list/draw (:image-list text-list-runner)))})

(defn create [x y content font color] (->TextList [(->Text x y content font color)]))
