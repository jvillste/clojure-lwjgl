(ns clojure-lwjgl.text
  (:import [org.lwjgl.opengl  GL11]
           [org.lwjgl BufferUtils]
           [java.awt Color Font FontMetrics]
           [java.awt.image BufferedImage]
           [java.nio IntBuffer FloatBuffer ByteBuffer ByteOrder]))

(def *texture-ids* (atom {}))

(defn get-texture [key] (key @*texture-ids*))
(defn set-texture [key id]
  (swap! *texture-ids* #(assoc % key id)))

(defn create-texture [key]
  (let [id (GL11/glGenTextures)]
    (set-texture key id)
    (println (str "generated texture " key " " (get-texture key)))
    id))

(defn load-texture [texture-id byte-buffer width height]
  (GL11/glBindTexture GL11/GL_TEXTURE_2D texture-id)
  (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGB width height 0 GL11/GL_RGB GL11/GL_UNSIGNED_BYTE byte-buffer)
  (GL11/glTexParameterf GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_S GL11/GL_CLAMP)
  (GL11/glTexParameterf GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_T GL11/GL_CLAMP)
  (GL11/glTexParameterf GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER GL11/GL_NEAREST)
  (GL11/glTexParameterf GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER GL11/GL_NEAREST)
  (GL11/glTexEnvi GL11/GL_TEXTURE_ENV GL11/GL_TEXTURE_ENV_MODE GL11/GL_DECAL))

(defn get-font-metrics [font]
  ;; TYPE_3BYTE_BGR TYPE_INT_ARGB
  (let [graphics (.getGraphics (BufferedImage. 1 1 BufferedImage/TYPE_3BYTE_BGR))]
    (.setFont graphics font)
    (.getFontMetrics graphics)))

(defn create-text-texture [key text]
  (let [texture-id (create-texture key)
        font (Font. "Dialog" Font/BOLD 12)
        font-metrics (get-font-metrics font)
        width (.stringWidth font-metrics text)
        height (.getHeight font-metrics)
        buffered-image (BufferedImage. width height BufferedImage/TYPE_3BYTE_BGR)
        graphics (.getGraphics buffered-image)]
    (println (str "width " width " " height))
    (doto graphics
      (.setColor Color/BLACK)
      (.fillRect 0 0 width height) 
      (.setColor Color/BLUE)
      (.setFont font)
      (.drawString "Foo" 0 0))
    (let [byte-buffer (ByteBuffer/allocateDirect (* width height 3))
          bytes (-> buffered-image (.getRaster) (.getDataBuffer) (.getData))]
      (.order byte-buffer (ByteOrder/nativeOrder))
      (println (str "length " (alength bytes)))
      (.put byte-buffer bytes 0 (alength bytes))
      (.flip byte-buffer)
      (load-texture texture-id byte-buffer width height))))

(defn draw-texture [key]
  (GL11/glBindTexture GL11/GL_TEXTURE_2D (get-texture key))
  (GL11/glBegin GL11/GL_QUADS)
  (GL11/glTexCoord2f 0 0)
  (GL11/glVertex3f -1 -1 1)
  (GL11/glTexCoord2f 0 1)
  (GL11/glVertex3f 1 -1 1)
  (GL11/glTexCoord2f 1 1)
  (GL11/glVertex3f 1 1 1)
  (GL11/glTexCoord2f 1 0)
  (GL11/glVertex3f -1 1 1)
  (GL11/glEnd))



