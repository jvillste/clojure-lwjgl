(ns clojure-lwjgl.core
  (:import [org.lwjgl.opengl GLContext PixelFormat Display DisplayMode GL11 GL12  GL30 ARBVertexBufferObject]
           [org.lwjgl.input Mouse]
           [org.lwjgl BufferUtils]
           [java.awt Frame Canvas]
           [java.awt.event WindowAdapter ComponentAdapter]
           [java.nio IntBuffer FloatBuffer])
  (:use clojure-lwjgl.text)
  (:use clojure-lwjgl.component)
  (:use clojure-lwjgl.buffer))

(defn draw-quads [vertex-buffer-id texture-coordinate-buffer-id index-buffer-id index-count]
  
  (GL11/glEnableClientState GL11/GL_TEXTURE_COORD_ARRAY)
  (bind-buffer texture-coordinate-buffer-id)
  (GL11/glTexCoordPointer 2 GL11/GL_FLOAT 0 (long 0))

  (GL11/glEnableClientState GL11/GL_VERTEX_ARRAY)
  (bind-buffer vertex-buffer-key)
  (GL11/glVertexPointer 3 GL11/GL_FLOAT 0 (long 0))

  (bind-element-buffer index-buffer-id)
  (GL12/glDrawArrays GL11/GL_QUADS 0 index-count)

(defn draw-triangles [color-buffer-key vertex-buffer-key index-buffer-key triangle-count]

  (GL11/glEnableClientState GL11/GL_VERTEX_ARRAY)
  (bind-buffer vertex-buffer-key)
  (GL11/glVertexPointer 3 GL11/GL_FLOAT 0 (long 0))

  (GL11/glEnableClientState GL11/GL_COLOR_ARRAY)
  (bind-buffer color-buffer-key)
  (GL11/glColorPointer 4 GL11/GL_FLOAT 0 (long 0))

  (bind-element-buffer index-buffer-key)
  (GL12/glDrawRangeElements GL11/GL_TRIANGLES 0 (- (* 3 4 triangle-count) 1) (* 3 triangle-count) GL11/GL_UNSIGNED_INT 0))

(def width (atom 300))
(def height (atom 300))
(def resize-requested (atom false))

(defn resize []
  (when resize-requested
    (GL11/glViewport 0 0 @width @height)
    (GL11/glMatrixMode GL11/GL_PROJECTION)
    (GL11/glLoadIdentity)
    (GL11/glOrtho 0, @width, 0, @height, -1, 1)
    (GL11/glMatrixMode GL11/GL_MODELVIEW)
    (reset! resize-requested false)))

(defn render-all []
  (resize)

  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)

  (GL11/glLoadIdentity)

  (let [text (create-text "Foo")]
    (render text)
    (dispose text))

  (GL11/glTranslatef 100 100 0)
  (GL11/glScalef 40 40 1)

  (dotimes [n 20]
    (GL11/glRotatef (* (/ (System/nanoTime) 1000000000) (float 14)) 0 0 1)
    (GL11/glTranslatef (* n (float 0.24)) 0 0)
    (GL11/glRotatef (* (/ (System/nanoTime) 1000000000) (float 2)) 0 0 1)

    (draw-triangles :color-buffer :vertex-buffer :index-buffer 1)))

(def closeRequested (atom false))

(def canvas (doto (Canvas.)
              (.addComponentListener
               (proxy [ComponentAdapter] []
                 (componentResized [e]
                   (println "Resizing")
                   (reset! width (-> canvas .getSize .getWidth))
                   (reset! height (-> canvas .getSize .getHeight)))))
              ))

(def frame (doto (new Frame)
             (.add canvas)
             (.addWindowListener
              (proxy [WindowAdapter] []
                (windowClosing [e]
                  (println "Frame closed")
                  (reset! closeRequested true))))
             (.setSize 400 400)
             .show))
(Display/setParent canvas)

(Display/create)

(println "create color buffer")
(create-buffer :color-buffer)
(load-buffer :color-buffer (create-float-buffer [1 0 0 1
                                                 1 1 0 1
                                                 1 0 0 1]))

(println "create vertex buffer")
(create-buffer :vertex-buffer)
(load-buffer :vertex-buffer (create-float-buffer [0 1 0
                                                  0 0 0
                                                  1 0 0]))

(println "create index buffer")
(create-buffer :index-buffer)
(load-element-buffer :index-buffer (create-int-buffer [0 1 2]))

(GL11/glClearColor 1 1 1 0)
(GL11/glEnable GL11/GL_BLEND)
(GL11/glEnable GL11/GL_TEXTURE_2d)
(GL11/glColorMask true, true, true, true)
(GL11/glBlendFunc GL11/GL_SRC_ALPHA GL11/GL_ONE_MINUS_SRC_ALPHA)

;;(try  (catch Exception e (println e)))

(defn do-loop []
  (try (render-all)
       (Display/update)

       (Display/sync 30)

       (catch Exception e (println e))))

(while (not @closeRequested)
  (do-loop))

(println "Destroying")
(Display/destroy)
(.dispose frame)
