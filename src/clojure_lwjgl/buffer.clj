(ns clojure-lwjgl.buffer
  (:import [org.lwjgl.opengl ARBVertexBufferObject]
           [org.lwjgl BufferUtils]
           [java.nio FloatBuffer IntBuffer]))

(defn create-gl-buffer [] (ARBVertexBufferObject/glGenBuffersARB))


(defn bind-buffer [id]
  (ARBVertexBufferObject/glBindBufferARB ARBVertexBufferObject/GL_ARRAY_BUFFER_ARB id))

(defn delete [id]
  (ARBVertexBufferObject/glDeleteBuffersARB id))

(defn bind-element-buffer [id]
  (ARBVertexBufferObject/glBindBufferARB ARBVertexBufferObject/GL_ELEMENT_ARRAY_BUFFER_ARB id))

(defn float-buffer-to-array [^FloatBuffer float-buffer]
  (let [result (float-array (.limit float-buffer))]
    (.rewind float-buffer)
    (.get float-buffer result)
    result))

(defn int-buffer-to-array [^IntBuffer int-buffer]
  (let [result (int-array (.limit int-buffer))]
    (.rewind int-buffer)
    (.get int-buffer result)
    result))

(defn load-buffer [id buffer]
  (.rewind buffer)
  (bind-buffer id)
  (ARBVertexBufferObject/glBufferDataARB ARBVertexBufferObject/GL_ARRAY_BUFFER_ARB
                                         buffer
                                         ARBVertexBufferObject/GL_STATIC_DRAW_ARB))

(defn load-element-buffer [id buffer]
  (.rewind buffer)

  (bind-element-buffer id)
  (ARBVertexBufferObject/glBufferDataARB ARBVertexBufferObject/GL_ELEMENT_ARRAY_BUFFER_ARB
                                         buffer
                                         ARBVertexBufferObject/GL_STATIC_DRAW_ARB))

(defn create-float-buffer [size] (BufferUtils/createFloatBuffer size))

(defn update-buffer
  ([buffer start-index values coersion]
     (.position buffer start-index)
     (doseq [value values]
       (.put buffer (coersion value)))
     buffer)

  ([buffer start-index values]
      (update-buffer buffer start-index values identity)))


(defn create-float-buffer-from-values [values]
  (let [float-buffer (BufferUtils/createFloatBuffer (count values))]
    (.put float-buffer (float-array values))
    (.rewind float-buffer)
    float-buffer))

(defn create-int-buffer [size] (BufferUtils/createIntBuffer size))
