(ns flow-gl.opengl.shader
  (:import [org.lwjgl.opengl GL11 ARBShaderObjects ARBVertexShader ARBFragmentShader]
           [org.lwjgl BufferUtils]))

(defn compile-errors [shader-id]
  (ARBShaderObjects/glGetInfoLogARB shader-id 1000))

(defn compile-shader [shader-id source]
  (ARBShaderObjects/glShaderSourceARB shader-id source)
  (ARBShaderObjects/glCompileShaderARB shader-id)
  (when (> (count (compile-errors shader-id))
           0)
    (throw (Exception. (compile-errors shader-id)))))

(defn create-vertex-shader []
  (ARBShaderObjects/glCreateShaderObjectARB ARBVertexShader/GL_VERTEX_SHADER_ARB))

(defn create-fragment-shader []
  (ARBShaderObjects/glCreateShaderObjectARB ARBFragmentShader/GL_FRAGMENT_SHADER_ARB))

(defn create-program [vertex-shader-id fragment-shader-id]
  (let [program-id (ARBShaderObjects/glCreateProgramObjectARB)]
    (ARBShaderObjects/glAttachObjectARB program-id vertex-shader-id)
    (ARBShaderObjects/glAttachObjectARB program-id fragment-shader-id)
    (ARBShaderObjects/glLinkProgramARB program-id)
    (ARBShaderObjects/glValidateProgramARB program-id)
    (when (> (count (compile-errors program-id))
             0)
      (throw (Exception. (str "Error when creating shader program: " (compile-errors program-id)))))
    program-id))

(defn get-uniform-location [program name]
  (ARBShaderObjects/glGetUniformLocationARB program name))

(defn set-float-uniform [program name value]
  (ARBShaderObjects/glUniform1fARB (get-uniform-location program
                                                         name)
                                   value))

(defn set-float3-uniform [program name value1 value2 value3]
  (ARBShaderObjects/glUniform3fARB (get-uniform-location program
                                                         name)
                                   value1
                                   value2
                                   value3))

(defn compile-program [vertex-shader-source fragment-shader-source]
  (let [vertex-shader-id (create-vertex-shader)
        fragment-shader-id (create-fragment-shader)]
    
    (try (compile-shader vertex-shader-id vertex-shader-source)
         (catch Exception exception
           (throw (Exception. (str "Error when compiling vertex shader: " exception)))))

    (try (compile-shader fragment-shader-id fragment-shader-source)
         (catch Exception exception
           (throw (Exception. (str "Error when compiling fragment shader: " exception)))))
    
    (create-program vertex-shader-id
                    fragment-shader-id)))

(defn enable-program [program-id]
  (ARBShaderObjects/glUseProgramObjectARB program-id))

(defn disable-program []
  (enable-program 0))

(defn delete-program [program-id]
  (ARBShaderObjects/glDeleteObjectARB program-id))