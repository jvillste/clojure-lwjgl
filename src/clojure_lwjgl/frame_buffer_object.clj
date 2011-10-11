(ns clojure-lwjgl.frame-buffer-object 
  (:import [org.lwjgl.opengl EXTFramebufferObject]
           [org.lwjgl.opengl GL11]))

(defn create []
  (EXTFramebufferObject/glGenFramebuffersEXT))

(defn bind-buffer [id]
  (EXTFramebufferObject/glBindFramebufferEXT EXTFramebufferObject/GL_FRAMEBUFFER_EXT id))

(defn delete [id]
  (EXTFramebufferObject/glDeleteFrameBuffersEXT id))

(defn bind-texture [texture-id]
  (EXTFramebufferObject/glFramebufferTexture2DEXT EXTFramebufferObject/GL_FRAMEBUFFER_EXT
                                                  EXTFramebufferObject/GL_COLOR_ATTACHMENT0_EXT
                                                  GL11/GL_TEXTURE_2D
                                                  texture-id
                                                  0))