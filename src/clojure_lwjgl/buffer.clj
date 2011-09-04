(ns clojure-lwjgl.buffer
  (:import [org.lwjgl.opengl GL11 ARBVertexBufferObject]
           [org.lwjgl BufferUtils]))

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
