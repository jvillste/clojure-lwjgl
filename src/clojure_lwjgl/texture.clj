(ns clojure-lwjgl.texture
  (:import [org.lwjgl.opengl GL11]
           [java.awt.image BufferedImage Raster DataBuffer ComponentColorModel]
           [java.awt.color ColorSpace]
           [java.util Hashtable]
           [java.nio IntBuffer FloatBuffer ByteBuffer ByteOrder]))

(defrecord Texture [id width height buffered-image byte-buffer])

(defn delete-texture [texture-id] (GL11/glDeleteTextures texture-id))
(defn bind-texture [texture-id]  (GL11/glBindTexture GL11/GL_TEXTURE_2D texture-id))

(defn load-texture [texture-id byte-buffer width height]
  (GL11/glBindTexture GL11/GL_TEXTURE_2D texture-id)
  (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGBA width height 0 GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE byte-buffer)
  ;;  (GL11/glTexParameterf GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_S GL11/GL_CLAMP)
  ;;  (GL11/glTexParameterf GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_T GL11/GL_CLAMP)
  (GL11/glTexParameterf GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER GL11/GL_LINEAR)
  (GL11/glTexParameterf GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER GL11/GL_LINEAR)
  ;;  (GL11/glTexEnvi GL11/GL_TEXTURE_ENV GL11/GL_TEXTURE_ENV_MODE GL11/GL_DECAL)
  )

(defn draw-texture [texture-id width height]

  (bind-texture texture-id)
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

(defn create-buffered-image [width height]
  (let [raster (Raster/createInterleavedRaster DataBuffer/TYPE_BYTE
                                               width
                                               height
                                               4
                                               nil)
        component-color-model (ComponentColorModel. (ColorSpace/getInstance ColorSpace/CS_sRGB)
                                                    (int-array [8 8 8 8])
                                                    true
                                                    false
                                                    ComponentColorModel/TRANSLUCENT
                                                    DataBuffer/TYPE_BYTE)]
    (BufferedImage. component-color-model
                    raster
                    false
                    (Hashtable.))))

(defn create-byte-buffer-for-image [buffered-image]
  (let [bytes (-> buffered-image (.getRaster) (.getDataBuffer) (.getData))
        byte-buffer (ByteBuffer/allocateDirect (alength bytes))]
    (.order byte-buffer (ByteOrder/nativeOrder))
    (.put byte-buffer bytes 0 (alength bytes))
    (.flip byte-buffer)))

(defn texture-dimension [value] value)

(defn create-texture
  [min-width min-height]
  (let [width (texture-dimension min-width)
        height (texture-dimension min-height)
        id (GL11/glGenTextures)
        buffered-image (create-buffered-image width height)
        byte-buffer (create-byte-buffer buffered-image)]
    (Texture. id width height buffered-image byte-buffer))

  []
  (create-texture 128 128))


