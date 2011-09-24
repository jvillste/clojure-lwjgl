(ns clojure-lwjgl.gui)

(defn create [window]
  (GL11/glClearColor 1 1 1 0)
  (GL11/glEnable GL11/GL_BLEND)
  (GL11/glEnable GL11/GL_TEXTURE_2D)
  (GL11/glColorMask true, true, true, true)
  (GL11/glBlendFunc GL11/GL_SRC_ALPHA GL11/GL_ONE_MINUS_SRC_ALPHA)

  (assoc gui :component-manager (-> (component-manager/create)
                                    (component-manager/add-component (text-field/create "Foobar")))))