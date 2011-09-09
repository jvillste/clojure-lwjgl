(ns clojure-lwjgl.text
  (:import [org.lwjgl.opengl GL11]
           [org.lwjgl BufferUtils]
           [java.awt.image BufferedImage]
           [java.awt Color Font FontMetrics RenderingHints])
  (:require [clojure-lwjgl.component :as component]
            [clojure-lwjgl.texture :as texture]))

(def font (Font. "Arial" Font/PLAIN 12))

(defn get-font-metrics [font]
  (let [graphics (.getGraphics (BufferedImage. 1 1 BufferedImage/TYPE_INT_ARGB))]
    (.setFont graphics font)
    (.getFontMetrics graphics)))

(defn render [text-line graphics]
  (doto graphics
    (.setColor Color/BLACK)
    (.setFont font)
    (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_HBGR )
    (.drawString (:text text-line) 0 (component/get-height text-line))))

(defn get-width [text-line] (.stringWidth (get-font-metrics font) (:text text-line)))

(defn get-height [text-line] (.getHeight (get-font-metrics font)))

(defn dispose [text-line] (texture/delete (:texture text-line)))

(defrecord TextLine [text])

(extend TextLine
  component/Component
  {:render render
   :get-width get-width
   :get-height get-height
   :dispose dispose})
