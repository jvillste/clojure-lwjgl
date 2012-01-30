(ns clojure-lwjgl.text
  (:import [org.lwjgl.opengl GL11]
           [org.lwjgl BufferUtils]
           [java.awt.image BufferedImage]
           [java.awt Color Font FontMetrics RenderingHints])
  (:require (clojure-lwjgl [visual :as visual]
                           [layoutable :as layoutable])))

(def font (Font. "Arial" Font/BOLD 11))

(defn get-font-metrics [font]
  (let [graphics (.getGraphics (BufferedImage. 1 1 BufferedImage/TYPE_INT_ARGB))]
    (.setFont graphics font)
    (.getFontMetrics graphics)))

(defn get-width [text] (.stringWidth (get-font-metrics font) (:content text)))

(defn get-height [text] (.getHeight (get-font-metrics font)))

(defn render [text graphics]
  (doto graphics
    (.setColor Color/BLACK)
    (.setFont font)
    (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_HBGR )
    (.drawString (:content text) 0 (get-height text))))

(defrecord Text [content])

(defn create [content] (Text. content))

(extend Text
  visual/Visual
  {:render render})

(extend Text
  layoutable/Layoutable
  {:preferred-width get-width
   :preferred-height get-height})
