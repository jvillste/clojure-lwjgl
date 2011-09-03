(ns clojure-lwjgl.texture
    (:import [org.lwjgl.opengl GL11]))

(defn create-texture [] (GL11/glGenTextures))
(defn delete-texture [texture-id] (GL11/glDeleteTextures texture-id))

(defn texture-dimension [value] 128)

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
  (GL11/glEnable GL11/GL_TEXTURE_2D)
  (GL11/glBindTexture GL11/GL_TEXTURE_2D texture-id)
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
