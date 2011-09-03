(ns clojure-lwjgl.text
  (:import [org.lwjgl.opengl GL11]
           [org.lwjgl BufferUtils]
           [java.awt Color Font FontMetrics RenderingHints]
           [java.awt.image BufferedImage Raster DataBuffer ComponentColorModel]
           [java.awt.color ColorSpace]
           [java.util Hashtable]
           [java.nio IntBuffer FloatBuffer ByteBuffer ByteOrder])
  (:use clojure-lwjgl.component clojure-lwjgl.texture))

(defrecord Text [text texture-id width height]
  Component

  (clojure-lwjgl.core/render [text]
    (draw-texture (:texture-id text)
                  (:width text)
                  (:height text)))

  (clojure-lwjgl.core/dispose
    [text] (delete-texture (:texture-id text))))

(defn get-font-metrics [font]
  (let [graphics (.getGraphics (BufferedImage. 1 1 BufferedImage/TYPE_INT_ARGB))]
    (.setFont graphics font)
    (.getFontMetrics graphics)))

(defn create-text [text]
  (let [texture-id (create-texture)
        font (Font. "Arial" Font/PLAIN 12)
        font-metrics (get-font-metrics font)
        text-width (.stringWidth font-metrics text)
        text-height (.getHeight font-metrics)
        texture-width (texture-dimension text-width)
        texture-height (texture-dimension text-height)
        raster (Raster/createInterleavedRaster DataBuffer/TYPE_BYTE texture-width texture-height 4 nil)
        component-color-model (ComponentColorModel. (ColorSpace/getInstance ColorSpace/CS_sRGB)
                                                    (int-array [8 8 8 8])
                                                    true
                                                    false
                                                    ComponentColorModel/TRANSLUCENT
                                                    DataBuffer/TYPE_BYTE)
        buffered-image (BufferedImage. component-color-model
                                       raster
                                       false
                                       (Hashtable.))
        graphics (.getGraphics buffered-image)]
    (doto graphics
;      (.setColor (Color. (float 1.0)
;                         (float 1.0)
;                         (float 1.0)
;                         (float 1.0)))
;      (.fillRect 0 0 texture-width texture-height)
      (.setColor Color/BLACK)
      (.setFont font)
                                        ;      (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
      (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_HBGR )

      (.drawString "Tämä on testitekstiä" 0 text-height))
    (let [bytes (-> buffered-image (.getRaster) (.getDataBuffer) (.getData))
          byte-buffer (ByteBuffer/allocateDirect (alength bytes))]
      (.order byte-buffer (ByteOrder/nativeOrder))
      (.put byte-buffer bytes 0 (alength bytes))
      (.flip byte-buffer)
      (load-texture texture-id byte-buffer texture-width texture-height))

    (Text. text texture-id texture-width texture-height)))
