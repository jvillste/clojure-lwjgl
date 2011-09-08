(ns clojure-lwjgl.buffer
  (:import [org.lwjgl.opengl GL11 GL12 ARBVertexBufferObject]
           [org.lwjgl BufferUtils]
           [java.nio FloatBuffer]))

(defrecord Buffer [id data])

(defn create-buffer [])

(defn create-gl-buffer [] (ARBVertexBufferObject/glGenBuffersARB))

(defn bind-buffer [id]
  ;;  (println (str "Binding buffer " key " " (get-buffer key)))
  (ARBVertexBufferObject/glBindBufferARB ARBVertexBufferObject/GL_ARRAY_BUFFER_ARB id))

(defn bind-element-buffer [id]
  ;;  (println (str "Binding element buffer " key " " (get-buffer key)))
  (ARBVertexBufferObject/glBindBufferARB ARBVertexBufferObject/GL_ELEMENT_ARRAY_BUFFER_ARB id))


(defn load-buffer [id buffer]
  (bind-buffer id)
  (ARBVertexBufferObject/glBufferDataARB ARBVertexBufferObject/GL_ARRAY_BUFFER_ARB
                                         buffer
                                         ARBVertexBufferObject/GL_STATIC_DRAW_ARB))

(defn load-element-buffer [id buffer]
  (bind-element-buffer id)
  (ARBVertexBufferObject/glBufferDataARB ARBVertexBufferObject/GL_ELEMENT_ARRAY_BUFFER_ARB
                                         buffer
                                         ARBVertexBufferObject/GL_STATIC_DRAW_ARB))

(defn create-float-buffer [size] (BufferUtils/createFloatBuffer size))

(defn update-buffer [buffer start-index values]
  (.position buffer start-index)
  (doseq [value values]
    (.put buffer value))
  buffer)

(defn float-buffer-to-array [^FloatBuffer float-buffer]
  (let [result (float-array (.limit float-buffer))]
    (.rewind float-buffer)
    (.get float-buffer result)
    result))

(defn create-float-buffer-from-values [values]
  (let [float-buffer (BufferUtils/createFloatBuffer (count values))]
    (.put float-buffer (float-array values))
    (.rewind float-buffer)
    float-buffer))

(defn create-int-buffer [size] (BufferUtils/createIntBuffer size))


(defn draw-quads [vertex-buffer-id
                  texture-coordinate-buffer-id
                  index-buffer-id
                  quad-count]

  (GL11/glEnableClientState GL11/GL_TEXTURE_COORD_ARRAY)
  (bind-buffer texture-coordinate-buffer-id)
  (GL11/glTexCoordPointer 2 GL11/GL_FLOAT 0 (long 0))

  (GL11/glEnableClientState GL11/GL_VERTEX_ARRAY)
  (bind-buffer vertex-buffer-id)
  (GL11/glVertexPointer 3 GL11/GL_FLOAT 0 (long 0))

  (bind-element-buffer index-buffer-id)
  (GL12/glDrawRangeElements GL11/GL_QUADS 0 (- (* 4 3 quad-count) 1) (* 4 quad-count) GL11/GL_UNSIGNED_INT 0))

(defn draw-triangles [color-buffer-key
                      vertex-buffer-key
                      index-buffer-key
                      triangle-count]

  (GL11/glEnableClientState GL11/GL_VERTEX_ARRAY)
  (bind-buffer vertex-buffer-key)
  (GL11/glVertexPointer 3 GL11/GL_FLOAT 0 (long 0))

  (GL11/glEnableClientState GL11/GL_COLOR_ARRAY)
  (bind-buffer color-buffer-key)
  (GL11/glColorPointer 4 GL11/GL_FLOAT 0 (long 0))

  (bind-element-buffer index-buffer-key)
  (GL12/glDrawRangeElements GL11/GL_TRIANGLES 0 (- (* 3 4 triangle-count) 1) (* 3 triangle-count) GL11/GL_UNSIGNED_INT 0))
