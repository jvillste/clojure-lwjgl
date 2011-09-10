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

(defn render [text graphics]
  (doto graphics
    (.setColor Color/BLACK)
    (.setFont font)
    (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_HBGR )
     (.drawString (:content text) 0 (component/get-height text))
    )
  )

(defn get-width [text] (.stringWidth (get-font-metrics font) (:content text)))

(defn get-height [text] (.getHeight (get-font-metrics font)))

(defn dispose [text] (texture/delete (:texture text)))

(defrecord Text [content])

(defn create [content] (Text. content))

(extend Text
  component/Component
  {:render render
   :get-width get-width
   :get-height get-height
   :dispose dispose})
