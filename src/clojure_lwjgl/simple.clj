(ns clojure-lwjgl.simple
  (:import [org.lwjgl.opengl Display DisplayMode GL11 GL12  ARBVertexBufferObject]
           [org.lwjgl.input Mouse]
           [org.lwjgl BufferUtils]
           [java.nio IntBuffer FloatBuffer]))


(Display/setDisplayMode (DisplayMode. 500 500))
(Display/create)

(GL11/glMatrixMode GL11/GL_PROJECTION)
(GL11/glLoadIdentity)
(GL11/glOrtho -1, 2, -1, 2, -1, 1)

(def *vertex-buffer* (BufferUtils/createFloatBuffer (* 4 3)))
(.put *vertex-buffer* (float-array [
                                    0.0, 0.0, 0.0,
                                    0.0, 1.0, 0.0,
                                    1.0, 1.0, 0.0,
                                    1.0, 0.0, 0.0,
                                    ]))
(.rewind *vertex-buffer*)

(def *vertex-buffer-id* (ARBVertexBufferObject/glGenBuffersARB))

(ARBVertexBufferObject/glBindBufferARB ARBVertexBufferObject/GL_ARRAY_BUFFER_ARB *vertex-buffer-id*)
(ARBVertexBufferObject/glBufferDataARB ARBVertexBufferObject/GL_ARRAY_BUFFER_ARB *vertex-buffer* ARBVertexBufferObject/GL_STATIC_DRAW_ARB)

(while (not (Display/isCloseRequested))
  (Display/update)
  

  (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))

  (GL11/glColor3f (float 1) (float 0) (float 0))

  (GL11/glEnableClientState GL11/GL_VERTEX_ARRAY)
  (ARBVertexBufferObject/glBindBufferARB ARBVertexBufferObject/GL_ARRAY_BUFFER_ARB *vertex-buffer-id*)
  (GL11/glVertexPointer 3 GL11/GL_FLOAT 0 (long 0))
  (GL11/glDrawArrays GL11/GL_QUADS 0 (* 4 3))
  (GL11/glDisableClientState GL11/GL_VERTEX_ARRAY)

  (Display/sync 10))

(println "Destroying")
(Display/destroy)