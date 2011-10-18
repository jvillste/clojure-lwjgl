(ns clojure-lwjgl.paint.core
  (:refer-clojure :exclude (load))
  (:require (clojure-lwjgl [window :as window]
                           [quad-buffer :as quad-buffer]
                           [quad-list :as quad-list]
                           [draw :as draw]
                           [texture :as texture]
                           [texture-coordinate-buffer :as texture-coordinate-buffer]
                           [frame-buffer-object :as frame-buffer-object]
                           [shader :as shader]))
  (:import [java.awt Color Font  RenderingHints]
           [org.lwjgl.opengl GL11]))

(def width 400)
(def height 400)

(defn add-quad [paint]
  (assoc paint
    :quad-buffer (quad-buffer/add-quad (:quad-buffer paint) 0 0 width height)
    :quad-list (quad-list/add-quad (:quad-list paint))
    :texture-coordinate-buffer (texture-coordinate-buffer/update (:texture-coordinate-buffer paint)
                                                                 0
                                                                 0.0
                                                                 0.0
                                                                 1.0
                                                                 1.0)))
(defn load [paint]
  (assoc paint
    :quad-buffer (quad-buffer/load (:quad-buffer paint))
    :quad-list (quad-list/load (:quad-list paint))
    :texture-coordinate-buffer (texture-coordinate-buffer/load (:texture-coordinate-buffer paint))
    :texture-1 (texture/load (:texture-1 paint))
    :texture-2 (texture/load (:texture-2 paint))))

(def vertex-shader-source "
void main(){
    gl_Position = gl_ModelViewProjectionMatrix*gl_Vertex;
}
")

(def vertex-shader-2-source "
void main() {
  gl_TexCoord[0] = gl_MultiTexCoord0;
  gl_Position = ftransform();
}
")




(def fragment-shader-source "
void main(){
    gl_FragColor = vec4(0.0, 0.0, 1.0, 1.0);
}
")

(def fragment-shader-2-source "
uniform sampler2D tex;

void main() {
	vec4 color = texture2D(tex,gl_TexCoord[0].st);
	gl_FragColor = color * vec4(0.5, 0.5, 0.5, 1.0);;
}
")

(defn create-paint []
  {:window (window/create)
   :quad-buffer (quad-buffer/create)
   :quad-list (quad-list/create)
   :texture-coordinate-buffer (texture-coordinate-buffer/create)
   :texture-1 (texture/create width height)
   :texture-2 (texture/create width height)
   :frame-buffer-object (frame-buffer-object/create)
   :shader-program (shader/compile-program vertex-shader-2-source
                                           fragment-shader-2-source)})

(defn draw-texture [paint]
  (let [graphics (texture/get-graphics (:texture-1 paint))]
    (doto graphics
      (.setColor Color/GREEN)
      (.fillRect 0 0 200 100))
    paint))

(defn update-window [paint]
  (assoc paint :window (window/update (:window paint))))


(defn render-texture [paint texture]
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
  (GL11/glMatrixMode GL11/GL_MODELVIEW)
  (GL11/glLoadIdentity)

  (shader/enable-program (:shader-program paint))
  
  ;;  (GL11/glDisable GL11/GL_LIGHTING)
  ;;  (GL11/glDisable GL11/GL_BLEND)
  ;;  (GL11/glTexEnvi GL11/GL_TEXTURE_ENV GL11/GL_TEXTURE_ENV_MODE GL11/GL_MODULATE)
  ;;  (GL11/glTexEnvi GL11/GL_TEXTURE_ENV GL11/GL_TEXTURE_ENV_MODE GL11/GL_TEXTURE)

  (texture/bind texture)
  (draw/draw-quads (:vertex-buffer-id (:quad-buffer paint))
                   (:buffer-id (:texture-coordinate-buffer paint))
                   (:index-buffer-id (:quad-list paint))
                   1)

  (shader/disable-program))

(defn render [paint]
  (render-texture paint
                  (:texture-1 paint))
  paint)

(defn render-to-texture [paint]
  (frame-buffer-object/bind (:frame-buffer-object paint))
  (frame-buffer-object/bind-texture (:id (:texture-2 paint)))

  (GL11/glPushAttrib GL11/GL_VIEWPORT_BIT)
  (GL11/glViewport 0 0 width height)
  (GL11/glMatrixMode GL11/GL_PROJECTION)
  (GL11/glPushMatrix)
  (GL11/glLoadIdentity)
  (GL11/glOrtho 0 width 0 height -1 1)

  (render-texture paint
                  (:texture-1 paint))

  (GL11/glMatrixMode GL11/GL_PROJECTION)
  (GL11/glPopMatrix)
  (GL11/glPopAttrib)

  (frame-buffer-object/bind 0)
  paint)

(defn update [paint]
  (-> paint
      (render)
      (update-window)))

(let [initial-paint (-> (create-paint)
                        (add-quad)
                        (draw-texture)
                        (load)
                        (render-to-texture))]
  (try
    (loop [paint initial-paint]
      (if (not @(:close-requested (:window paint)))
        (recur (update paint))
        (window/close (:window paint))))
    (catch Exception e
      (println e)
      (.printStackTrace e)
      (window/close (:window initial-paint)))))


