(ns clojure-lwjgl.text
  (:import [org.lwjgl.opengl GL11]
           [org.lwjgl BufferUtils]
           [java.io File]
           [java.awt.image BufferedImage]
           [java.awt Color Font FontMetrics RenderingHints])
  (:require (clojure-lwjgl [visual :as visual]
                           [layoutable :as layoutable])))

(def font (-> (Font/createFont Font/TRUETYPE_FONT (File. "Vera.ttf"))
              (.deriveFont (float 50))))

(defn get-font-metrics [font]
  (let [graphics (.getGraphics (BufferedImage. 1 1 BufferedImage/TYPE_INT_ARGB))]
    (.setFont graphics font)
    (.getFontMetrics graphics)))

(defn width [text] (.stringWidth (get-font-metrics font) (:content text)))

(defn height [text] (+ (.getMaxAscent (get-font-metrics font))
                   ;;    (.getLeading (get-font-metrics font))
                       (.getMaxDescent (get-font-metrics font))))

(defn ascent [text] (.getMaxAscent (get-font-metrics font)))


(defn render [text graphics]
  (doto graphics
;;    (.setColor Color/GREEN)
;;    (.fillRect 0 0 (width text) (height text))
    (.setColor Color/BLACK)
    (.setFont font)
    (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_HBGR )
    (.drawString (:content text) 0 (ascent text))))

(defrecord Text [content])

(defn create [content] (Text. content))

(extend Text
  visual/Visual
  {:render render})

(extend Text
  layoutable/Layoutable
  {:preferred-width width
   :preferred-height height})
