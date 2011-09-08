(ns clojure-lwjgl.text
  (:import [org.lwjgl.opengl GL11]
           [org.lwjgl BufferUtils]
           [java.awt.image BufferedImage]
           [java.awt Color Font FontMetrics RenderingHints])
  (:use clojure-lwjgl.component
        clojure-lwjgl.texture))

(def font (Font. "Arial" Font/PLAIN 12))
  
(defn get-font-metrics [font]
  (let [graphics (.getGraphics (BufferedImage. 1 1 BufferedImage/TYPE_INT_ARGB))]
    (.setFont graphics font)
    (.getFontMetrics graphics)))

(defrecord TextLine [text]
  Component

  (render [text-line graphics]
    (doto graphics
      (.setColor Color/BLACK)
      (.setFont font)
      (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_HBGR )
      (.drawString (:text text-line) 0 (get-height text-line))))

  (get-width [text-line] (.stringWidth (get-font-metrics font) (:text text-line)))
  
  (get-height [text-line] (.getHeight (get-font-metrics font)))
  
  (dispose [text-line] (delete-texture (:texture-id text-line))))
