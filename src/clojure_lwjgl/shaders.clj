(ns clojure-lwjgl.shaders)


(def default-vertex-shader-source "
void main() {
  gl_TexCoord[0] = gl_MultiTexCoord0;
  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
}
")

(def default-fragment-shader-source "
uniform sampler2D texture;

void main() {
        float x = gl_TexCoord[0].s;
        float y = gl_TexCoord[0].t;
        gl_FragColor = texture2D(texture,vec2(x,y))
}
")
