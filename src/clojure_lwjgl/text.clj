(ns clojure-lwjgl.text
  (:import [org.lwjgl.opengl GL11]
           [org.lwjgl BufferUtils]
           [java.awt Color Font FontMetrics RenderingHints]
           [java.awt.image BufferedImage Raster DataBuffer ComponentColorModel]
           [java.awt.color ColorSpace]
           [java.util Hashtable]
           [java.nio IntBuffer FloatBuffer ByteBuffer ByteOrder])
  (:require clojure-lwjgl.core))

(defrecord Text [text texture-id width height]
  clojure-lwjgl.core/Component
  (render [text])
  (dispose [text]))


(def *texture-ids* (atom {}))

(defn get-texture [key] (key @*texture-ids*))
(defn set-texture [key id]
  (swap! *texture-ids* #(assoc % key id)))
(defn remove-texture [key]
  (swap! *texture-ids* #(dissoc % key)))

(defn create-texture [key]
  (let [id (GL11/glGenTextures)]
    (set-texture key id)
    (println (str "generated texture " key " " (get-texture key)))
    id))



(defn delete-texture [key]
  (GL11/glDeleteTextures (get-texture key))
  (remove-texture key))

(defn load-texture [texture-id byte-buffer width height]
  (GL11/glBindTexture GL11/GL_TEXTURE_2D texture-id)
  (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGBA width height 0 GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE byte-buffer)
  ;;  (GL11/glTexParameterf GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_S GL11/GL_CLAMP)
  ;;  (GL11/glTexParameterf GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_T GL11/GL_CLAMP)
  (GL11/glTexParameterf GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER GL11/GL_LINEAR)
  (GL11/glTexParameterf GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER GL11/GL_LINEAR)
  ;;  (GL11/glTexEnvi GL11/GL_TEXTURE_ENV GL11/GL_TEXTURE_ENV_MODE GL11/GL_DECAL)
  )

(defn get-font-metrics [font]
  (let [graphics (.getGraphics (BufferedImage. 1 1 BufferedImage/TYPE_INT_ARGB))]
    (.setFont graphics font)
    (.getFontMetrics graphics)))

(defn texture-dimension [value] 128)

(defn create-text-texture [key text]
  (let [texture-id (create-texture key)
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
      (.setColor (Color. (float 1.0)
                         (float 1.0)
                         (float 1.0)
                         (float 1.0)))
      (.fillRect 0 0 texture-width texture-height)
      (.setColor Color/BLACK)
      (.setFont font)
                                        ;      (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
      (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_HBGR )

      (.drawString "Tämä on testitekstiä" 0 text-height)
      )
    (let [bytes (-> buffered-image (.getRaster) (.getDataBuffer) (.getData))
          byte-buffer (ByteBuffer/allocateDirect (alength bytes))]
      (.order byte-buffer (ByteOrder/nativeOrder))
      (.put byte-buffer bytes 0 (alength bytes))
      (.flip byte-buffer)
      (load-texture texture-id byte-buffer texture-width texture-height))))

(defn draw-texture [key width height]
  (GL11/glEnable GL11/GL_TEXTURE_2D)
  (GL11/glBindTexture GL11/GL_TEXTURE_2D (get-texture key))
  (GL11/glBegin GL11/GL_QUADS)
  (GL11/glTexCoord2f (float 0) (float 1))
  (GL11/glVertex3f (float 0) (float 0) (float 0))

  (GL11/glTexCoord2f (float 0) (float 0))
  (GL11/glVertex3f (float 0) (float height) (float 0))

  (GL11/glTexCoord2f (float 1) (float 0))
  (GL11/glVertex3f (float width) (float height) (float 0))

  (GL11/glTexCoord2f (float 1) (float 1))
  (GL11/glVertex3f (float width) (float 0) (float 0))
  (GL11/glEnd))



