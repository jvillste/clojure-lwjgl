(ns flow-gl.opengl.texture
  (:refer-clojure :exclude (load))
  (:import [org.lwjgl.opengl GL11 ARBMultitexture])
  (:require [flow-gl.opengl.buffered-image :as buffered-image]))

(defn- texture-dimension [value] value)
(defn- create-gl-texture [] (GL11/glGenTextures))

(defrecord Texture [id width height buffered-image])

(defn delete [texture] (GL11/glDeleteTextures (:id texture)))

(defn bind [texture]
  (GL11/glBindTexture GL11/GL_TEXTURE_2D (:id texture)))

(defn load [texture]
  (bind texture)
  (GL11/glTexImage2D GL11/GL_TEXTURE_2D
                     0
                     GL11/GL_RGBA
                     (:width texture)
                     (:height texture)
                     0
                     GL11/GL_RGBA
                     GL11/GL_UNSIGNED_BYTE
                     (buffered-image/create-byte-buffer (:buffered-image texture)))

  (GL11/glTexParameterf GL11/GL_TEXTURE_2D
                        GL11/GL_TEXTURE_MAG_FILTER
                        GL11/GL_NEAREST)

  (GL11/glTexParameterf GL11/GL_TEXTURE_2D
                        GL11/GL_TEXTURE_MIN_FILTER
                        GL11/GL_LINEAR)
  texture)

(defn draw [texture]
  (let [width (:width texture)
        height (:height texture)]
    (bind texture)

    (GL11/glBegin GL11/GL_QUADS)
    (GL11/glTexCoord2f (float 0) (float 1))
    (GL11/glVertex3f (float 0) (float 0) (float 0))

    (GL11/glTexCoord2f (float 0) (float 0))
    (GL11/glVertex3f (float 0) (float height) (float 0))

    (GL11/glTexCoord2f (float 1) (float 0))
    (GL11/glVertex3f (float width) (float height) (float 0))

    (GL11/glTexCoord2f (float 1) (float 1))
    (GL11/glVertex3f (float width) (float 0) (float 0))
    (GL11/glEnd)))


(defn get-graphics [texture]
  (buffered-image/get-graphics (:buffered-image texture)))

(defn texture-x-to-texel-x [texture texture-x]
  (* texture-x
     (:width texture)))

(defn texture-y-to-texel-y [texture texture-y]
  (* texture-y
     (:height texture)))

(defn create-for-buffered-image [buffered-image]
  (Texture. (create-gl-texture)
            (.getWidth buffered-image)
            (.getHeight buffered-image)
            buffered-image))

(defn create
  ([minimum-width minimum-height]
     (create-for-buffered-image (buffered-image/create (texture-dimension minimum-width)
                                                       (texture-dimension minimum-height))))

  ([]
     (create 128 128)))


(defn create-child [texture x y width height]
  (create-for-buffered-image (buffered-image/create-child (:buffered-image texture) x y width height)))

(defn number-of-texture-units []
  (GL11/glGetInteger ARBMultitexture/GL_MAX_TEXTURE_UNITS_ARB))

(defn maximum-texture-size []
  (GL11/glGetInteger GL11/GL_MAX_TEXTURE_SIZE))