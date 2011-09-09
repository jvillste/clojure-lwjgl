(ns clojure-lwjgl.buffered-image
  (:import [org.lwjgl.opengl GL11]
           [java.awt.image BufferedImage Raster DataBuffer ComponentColorModel]
           [java.awt.color ColorSpace]
           [java.util Hashtable]
           [java.nio IntBuffer FloatBuffer ByteBuffer ByteOrder]))

(defn create-byte-buffer [buffered-image]
  (let [bytes (-> buffered-image (.getRaster) (.getDataBuffer) (.getData))
        byte-buffer (ByteBuffer/allocateDirect (alength bytes))]
    (.order byte-buffer (ByteOrder/nativeOrder))
    (.put byte-buffer bytes 0 (alength bytes))
    (.flip byte-buffer)))

(defn create [width height]
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

(defn create-child [parent x y width height])