(ns flow-gl.opengl.window
  (:import [org.lwjgl.opengl GLContext PixelFormat Display DisplayMode GL11 GL12  GL30 ARBVertexBufferObject]
           [org.lwjgl.input Mouse]
           [org.lwjgl BufferUtils]
           [java.awt Frame Canvas]
           [java.awt.event WindowAdapter ComponentAdapter]
           [java.nio IntBuffer FloatBuffer]))

(defrecord Window [frame
                   close-requested
                   resize-requested
                   width
                   height])

(defn resize [window]
  (let [{:keys [width height]} window]
      (GL11/glViewport 0 0 width height)
      (GL11/glMatrixMode GL11/GL_PROJECTION)
      (GL11/glLoadIdentity)
      (GL11/glOrtho 0, width, 0, height, -1, 1)

      (GL11/glMatrixMode GL11/GL_MODELVIEW)
      (GL11/glLoadIdentity)
      (GL11/glScalef 1 -1 1)
      (GL11/glTranslatef 0 (- height) 0)
      (assoc window :resize-requested false)))

(defn update [window framerate]
  (let [window (if (:resize-requested window)
                 (resize window)
                 window)]
    (Display/update)
    (Display/sync framerate)
    window))

(defn initialize-gl []
  (GL11/glClearColor 0 0 0 0)
  (GL11/glEnable GL11/GL_BLEND)
  (GL11/glEnable GL11/GL_TEXTURE_2D)
  (GL11/glColorMask true, true, true, true)
  (GL11/glBlendFunc GL11/GL_SRC_ALPHA GL11/GL_ONE_MINUS_SRC_ALPHA))

(defn create [initial-width initial-height]
  (let [canvas (Canvas.)
        window-atom  (atom (map->Window {:frame (new Frame)
                                         :close-requested false
                                         :resize-requested true
                                         :width 0
                                         :height 0}))]

    (.addComponentListener canvas
                           (proxy [ComponentAdapter] []
                             (componentResized [e]
                               (swap! window-atom assoc
                                      :width (-> canvas .getSize .getWidth)
                                      :height (-> canvas .getSize .getHeight)
                                      :resize-requested true))))

    (doto (:frame @window-atom)
      (.add canvas)
      (.addWindowListener
       (proxy [WindowAdapter] []
         (windowClosing [e]
           (println "Frame closed")
           (swap! window-atom assoc
                  :close-requested true))))
      (.setSize initial-width initial-height)
      .show)

    (.requestFocus canvas)

    (Display/setParent canvas)
    (Display/create)
    (initialize-gl)

    (swap! window-atom update 10)
    window-atom))

(defn request-close [window]
  (assoc window :close-requested true))

(defn close [window]
  (println "Destroying window")
  (Display/destroy)
  (.dispose (:frame window))
  (assoc window :close-requested false))