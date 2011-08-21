(ns clojure-lwjgl.core
  (:import [org.lwjgl.opengl Display DisplayMode GL11 GL12  ARBVertexBufferObject]
           [org.lwjgl.input Mouse]
           [org.lwjgl BufferUtils]
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
  (println (str "Binding buffer " key " " (get-buffer key)))
  (ARBVertexBufferObject/glBindBufferARB ARBVertexBufferObject/GL_ARRAY_BUFFER_ARB
                                         (get-buffer key)))

(defn bind-element-buffer [key]
  (println (str "Binding element buffer " key " " (get-buffer key)))
  (ARBVertexBufferObject/glBindBufferARB ARBVertexBufferObject/GL_ELEMENT_ARRAY_BUFFER_ARB
                                         (get-buffer key)))


(defn load-buffer [key buffer]
  (bind-buffer key)
  (ARBVertexBufferObject/glBufferDataARB ARBVertexBufferObject/GL_ARRAY_BUFFER_ARB
                                         buffer
                                         ARBVertexBufferObject/GL_STATIC_DRAW_ARB))

(defn load-float-buffer [key buffer]
  (let [float-buffer (BufferUtils/createFloatBuffer (count buffer))]
    (.put float-buffer (float-array buffer))
    (println (str "loaded " buffer))
    (load-buffer key float-buffer)))

(defn load-int-buffer [key buffer]
  (let [int-buffer (BufferUtils/createIntBuffer (count buffer))]
    (.put int-buffer (int-array buffer))
    (load-buffer key int-buffer)))

(defn draw-triangles [color-buffer-key vertex-buffer-key index-buffer-key count]
  
  (GL11/glEnableClientState GL11/GL_VERTEX_ARRAY)
  (bind-buffer vertex-buffer-key)
  (GL11/glVertexPointer 3 GL11/GL_FLOAT 0 (long 0))

  (GL11/glEnableClientState GL11/GL_COLOR_ARRAY)
  (bind-buffer color-buffer-key)
  (GL11/glColorPointer 4 GL11/GL_FLOAT 0 (long 0))

  (bind-element-buffer index-buffer-key)
  (GL12/glDrawRangeElements GL11/GL_TRIANGLES 0 (* 9 count) count GL11/GL_UNSIGNED_INT (long 0)))


(def width 500)
(def height 500)


(defn render []
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)

  (GL11/glColor3f (float 1) (float 0) (float 0))
  (GL11/glBegin GL11/GL_TRIANGLES)
  (GL11/glVertex3f 0 0 0)
  (GL11/glVertex3f 10 0 0)
  (GL11/glVertex3f 5 10 0)
  (GL11/glEnd)

  (GL11/glColor3f (float 0) (float 1) (float 0))
  (draw-triangles :color-buffer :vertex-buffer :index-buffer 3))


(Display/setDisplayMode (DisplayMode. 500 500))
(Display/create)

(GL11/glMatrixMode GL11/GL_PROJECTION)
(GL11/glLoadIdentity)
(GL11/glOrtho 0 width height 0 1 -1)
(GL11/glMatrixMode GL11/GL_MODELVIEW)



(try (println "create color buffer")
     (create-buffer :color-buffer)
     (load-float-buffer :color-buffer [1 0 0 1
                                        1 0 0 1
                                        1 0 0 1])

     (println "create vertex buffer")
     (create-buffer :vertex-buffer)
     (load-float-buffer :vertex-buffer [0 0 0
                                        100 0 0
                                        50 100 0])

     (println "create index buffer")
     (create-buffer :index-buffer)
     (load-int-buffer :index-buffer [0 1 2])

     (catch Exception e (println (str e (.getStackTrace e)))))


(while (not (Display/isCloseRequested))
  (try (render)
       (catch Exception e (println (str e (.getStackTrace e)))))
  (Display/update)
  (Display/sync 1))

(println "Destroying")
(Display/destroy)