(ns clojure-lwjgl.window
  (:import [org.lwjgl.opengl GLContext PixelFormat Display DisplayMode GL11 GL12  GL30 ARBVertexBufferObject]
           [org.lwjgl.input Mouse]
           [org.lwjgl BufferUtils]
           [java.awt Frame Canvas]
           [java.awt.event WindowAdapter ComponentAdapter]
           [java.nio IntBuffer FloatBuffer])
  (:require [clojure-lwjgl.input :as input]))

(def width (atom 300))
(def height (atom 300))
(def resize-requested (atom false))
(def closeRequested (atom false))

(defn resize []
  (when resize-requested
    (GL11/glViewport 0 0 @width @height)
    (GL11/glMatrixMode GL11/GL_PROJECTION)
    (GL11/glLoadIdentity)
    (GL11/glOrtho 0, @width, 0, @height, -1, 1)
    (GL11/glMatrixMode GL11/GL_MODELVIEW)
    (reset! resize-requested false)))

(defn render [renderer]

  (resize)

  (try
    (@renderer)
    (catch Exception e
      (println e)
      (.printStackTrace e)))

  (Display/update)

  (Display/sync 1))

(defn open [renderer initializer input-listener]
  (let [canvas (Canvas.)
        frame (new Frame)
        input (input/create input-listener)]

    (.addComponentListener canvas
                           (proxy [ComponentAdapter] []
                             (componentResized [e]
                               (println "Resizing")
                               (reset! width (-> canvas .getSize .getWidth))
                               (reset! height (-> canvas .getSize .getHeight)))))


    (.addMouseListener canvas (input/create-mouse-input-handler input))
    (.addMouseMotionListener canvas (input/create-mouse-input-handler input))
    (.addKeyListener canvas (input/create-keyboard-input-handler input))

    (doto frame
      (.add canvas)
      (.addWindowListener
       (proxy [WindowAdapter] []
         (windowClosing [e]
           (println "Frame closed")
           (reset! closeRequested true))))
      (.setSize 400 400)
      .show)

    (.requestFocusInWindow canvas)
    
    (Display/setParent canvas)

    (Display/create)

    (GL11/glClearColor 1 1 1 0)
    (GL11/glEnable GL11/GL_BLEND)
    (GL11/glEnable GL11/GL_TEXTURE_2D)
    (GL11/glColorMask true, true, true, true)
    (GL11/glBlendFunc GL11/GL_SRC_ALPHA GL11/GL_ONE_MINUS_SRC_ALPHA)

    (try
      (initializer)
      (catch Exception e
        (println e)))

    (while (not @closeRequested)
      (render renderer))

    (println "Destroying window")
    (Display/destroy)
    (.dispose frame)
    (reset! closeRequested false)))








