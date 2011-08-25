(ns clojure-lwjgl.core
  (:import [org.lwjgl.opengl GLContext PixelFormat Display DisplayMode GL11 GL12  GL30 ARBVertexBufferObject]
           [org.lwjgl.input Mouse]
           [org.lwjgl BufferUtils]
           [java.awt Frame Canvas]
           [java.awt.event WindowAdapter ComponentAdapter]
           [java.nio IntBuffer FloatBuffer]))

(def *buffer-ids* (atom {}))

(defn get-buffer [key] (key @*buffer-ids*))
(defn set-buffer [key id]
  (swap! *buffer-ids* #(assoc % key id)))

(defn create-buffer [key]
  (let [id (ARBVertexBufferObject/glGenBuffersARB)]
    (set-buffer key id)
    (println (str "generated buffer " key " " (get-buffer key)))
    id))

(defn bind-buffer [key]
  ;;  (println (str "Binding buffer " key " " (get-buffer key)))
  (ARBVertexBufferObject/glBindBufferARB ARBVertexBufferObject/GL_ARRAY_BUFFER_ARB
                                         (get-buffer key)))

(defn bind-element-buffer [key]
  ;;  (println (str "Binding element buffer " key " " (get-buffer key)))
  (ARBVertexBufferObject/glBindBufferARB ARBVertexBufferObject/GL_ELEMENT_ARRAY_BUFFER_ARB
                                         (get-buffer key)))


(defn load-buffer [key buffer]
  (bind-buffer key)
  (ARBVertexBufferObject/glBufferDataARB ARBVertexBufferObject/GL_ARRAY_BUFFER_ARB
                                         buffer
                                         ARBVertexBufferObject/GL_STATIC_DRAW_ARB))

(defn load-element-buffer [key buffer]
  (bind-element-buffer key)
  (ARBVertexBufferObject/glBufferDataARB ARBVertexBufferObject/GL_ELEMENT_ARRAY_BUFFER_ARB
                                         buffer
                                         ARBVertexBufferObject/GL_STATIC_DRAW_ARB))

(defn create-float-buffer [values]
  (let [float-buffer (BufferUtils/createFloatBuffer (count values))]
    (.put float-buffer (float-array values))
    (.rewind float-buffer)
    float-buffer))

(defn create-int-buffer [values]
  (let [int-buffer (BufferUtils/createIntBuffer (count values))]
    (.put int-buffer (int-array values))
    (.rewind int-buffer)
    int-buffer))

(defn draw-triangles [color-buffer-key vertex-buffer-key index-buffer-key triangle-count]

  (GL11/glEnableClientState GL11/GL_VERTEX_ARRAY)
  (bind-buffer vertex-buffer-key)
  (GL11/glVertexPointer 3 GL11/GL_FLOAT 0 (long 0))

  (GL11/glEnableClientState GL11/GL_COLOR_ARRAY)
  (bind-buffer color-buffer-key)
  (GL11/glColorPointer 4 GL11/GL_FLOAT 0 (long 0))

  (bind-element-buffer index-buffer-key)
  (GL12/glDrawRangeElements GL11/GL_TRIANGLES 0 (- (* 3 4 triangle-count) 1) (* 3 triangle-count) GL11/GL_UNSIGNED_INT 0))


(def width (atom 0))
(def height (atom 0))


(defn render []
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)

  (GL11/glMatrixMode GL11/GL_PROJECTION)
  (GL11/glLoadIdentity)
  (GL11/glOrtho -1, 2, -1, 2, -1, 1)
  (GL11/glMatrixMode GL11/GL_MODELVIEW)


  (GL11/glLoadIdentity)
  (GL11/glTranslatef (float 0.5) 0.5 0)
  (GL11/glScalef 0.2 0.2 1)


  (dotimes [n 35]
    (GL11/glRotatef (* (/ (System/nanoTime) 1000000000) (float 10)) 0 0 1)
    (GL11/glTranslatef (* n (float 0.04)) 0 0)

    (draw-triangles :color-buffer :vertex-buffer :index-buffer 1))

  )

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
             (.setSize 800 800)
             .show))
(Display/setParent canvas)


(Display/create)



(try (println "create color buffer")
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

     (catch Exception e (println e)))

(defn do-loop []
  (try (render)
       (Display/update)
       (Display/sync 40)
       (catch Exception e (println e))))

(while (not @closeRequested)
  (do-loop))

(println "Destroying")
(Display/destroy)
(.dispose frame)