(ns flow-gl.opengl.buffer
  (:import [org.lwjgl.opengl ARBVertexBufferObject]
           [org.lwjgl BufferUtils]
           [java.nio FloatBuffer IntBuffer]))

(def native-buffers (atom {}))

(defn create-native-buffer [type capacity]
  (case type
    :int (BufferUtils/createIntBuffer capacity)
    :float (BufferUtils/createFloatBuffer capacity)))

(defn buffer-capacity [minimum-capacity]
  (loop [capacity 256]
    (if (>= capacity minimum-capacity)
      capacity
      (recur (* 2 capacity)))))

(defn add-native-buffer [type minimum-capacity]
  (let [native-buffer (create-native-buffer type
                                            (buffer-capacity minimum-capacity))]
    (swap! native-buffers assoc
           type
           native-buffer)
    native-buffer))

(defn native-buffer [type minimum-capacity]
  (let [native-buffer (get @native-buffers type)]
    (if native-buffer
      (if (>= (.capacity native-buffer)
              minimum-capacity)
        native-buffer
        (add-native-buffer type minimum-capacity))
      (add-native-buffer type minimum-capacity))))

(defn coercion [type]
  (case type
    :int int
    :float float))

(defn native-buffer-with-values [type values]
  (let [native-buffer (native-buffer type (count values))
        coerce (coercion type)]
    (.rewind native-buffer)
    (doseq [value values]
      (.put native-buffer (coerce value)))
    (.rewind native-buffer)
    native-buffer))


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


(defn load-buffer [id type values]
  (let [native-buffer (native-buffer-with-values type values)]
    (bind-buffer id)
    (ARBVertexBufferObject/glBufferDataARB ARBVertexBufferObject/GL_ARRAY_BUFFER_ARB
                                           native-buffer
                                           ARBVertexBufferObject/GL_STATIC_DRAW_ARB)))

(defn load-element-buffer [id values]
  (let [native-buffer (native-buffer-with-values :int values)]
    (bind-element-buffer id)
    (ARBVertexBufferObject/glBufferDataARB ARBVertexBufferObject/GL_ELEMENT_ARRAY_BUFFER_ARB
                                           native-buffer
                                           ARBVertexBufferObject/GL_STATIC_DRAW_ARB)))
