(ns clojure-lwjgl.text
  (:import [org.lwjgl.opengl GL11]
           [org.lwjgl BufferUtils]
           [java.io File]
           [java.awt.image BufferedImage]
           [java.awt Color Font FontMetrics RenderingHints])
  (:require (clojure-lwjgl [visual :as visual]
                           [layoutable :as layoutable]
                           [font :as font])))


(defn width [text] (font/width (:font text)
                               (:content text)))

(defn height [text] (font/height (:font text)
                                 (:content text)))


(defn render [text graphics]
  (doto graphics
    ;;    (.setColor Color/GREEN)
    ;;    (.fillRect 0 0 (width text) (height text))
    (.setColor Color/BLACK)
    (.setFont (font/graphics-font (:font text)))
    (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_HBGR )
    (.drawString (:content text) 0 (font/ascent (:font text)))))

(defrecord Text [content font])

(defn create [content] (Text. content (font/create "LiberationSans-Regular.ttf" 11)))

(extend Text
  visual/Visual
  {:render render})

(extend Text
  layoutable/Layoutable
  {:preferred-width width
   :preferred-height height})
