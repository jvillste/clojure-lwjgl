(ns clojure-lwjgl.paint.core
  (:refer-clojure :exclude (load))
  (:require (clojure-lwjgl [window :as window]
                           [quad-buffer :as quad-buffer]
                           [quad-list :as quad-list]
                           [draw :as draw]
                           [texture :as texture]
                           [texture-coordinate-buffer :as texture-coordinate-buffer]
                           [frame-buffer-object :as frame-buffer-object]
                           [shader :as shader]
                           [buffered-image :as buffered-image]
                           [input :as input]))
  (:import [java.awt Color Font  RenderingHints]
           [org.lwjgl.opengl GL11]))

(defn width [paint]
  (:width (:texture-1 paint)))

(defn height [paint]
  (:height (:texture-1 paint)))

(defn add-quad [paint]
  (assoc paint
    :quad-buffer (quad-buffer/add-quad (:quad-buffer paint) 0 0 (width paint) (height paint))
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
  gl_Position = gl_ModelViewProjectionMatrix*gl_Vertex;

}
")

(def fragment-shader-source "
void main(){
    gl_FragColor = vec4(0.0, 0.0, 1.0, 1.0);
}
")

(def fragment-shader-2-source "
uniform sampler2D tex;
uniform float mouseX;

void main() {
        vec4 color = texture2D(tex,gl_TexCoord[0].st);
        gl_FragColor = color * vec4(1.0, 1.0, 1.0, mouseX);
}
")

(defn create-paint [window]
  {:window window
   :quad-buffer (quad-buffer/create)
   :quad-list (quad-list/create)
   :texture-coordinate-buffer (texture-coordinate-buffer/create)
   :visible-texture :texture-1
   :frame-buffer-object (frame-buffer-object/create)
   :shader-program (shader/compile-program vertex-shader-2-source
                                           fragment-shader-2-source)})

(defn non-visible-texture [paint]
  (if (= (:visible-texture paint)
         :texture-1)
    :texture-2
    :texture-1))

(defn swap-visible-texture [paint]
  (assoc paint
    :visible-texture (non-visible-texture paint)))

(defn load-image [paint]
  (let [image (buffered-image/create-from-file "mood_study_by_exphrasis-d4cnrgu.jpg")]
    (assoc paint
      :texture-1 (texture/create (.getWidth image) (.getHeight image))
      :texture-2 (texture/create-for-buffered-image image))))

(defn draw-initial-texture [paint]
  (let [graphics (texture/get-graphics ((non-visible-texture paint) paint))]
    (doto graphics
      (.setColor Color/GREEN)
      (.fillRect 0 0 200 100)
      (.setColor Color/BLACK)
      (.setFont (Font. "Arial" Font/BOLD 20))
      (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_HBGR )
      (.drawString "Foo" 20 100))
    paint))

(defn update-window [paint]
  (assoc paint :window (window/update (:window paint)
                                      40)))

(defn render-texture [paint texture]
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
  (GL11/glMatrixMode GL11/GL_MODELVIEW)
  (GL11/glLoadIdentity)

  (shader/enable-program (:shader-program paint))

  (shader/set-float-uniform (:shader-program paint)
                            "mouseX"
                            (float (/ (input/mouse-x)
                                      @(:width (:window paint)))))

  (texture/bind texture)
  (draw/draw-quads (:vertex-buffer-id (:quad-buffer paint))
                   (:buffer-id (:texture-coordinate-buffer paint))
                   (:index-buffer-id (:quad-list paint))
                   1)

  (shader/disable-program))

(defn render [paint]
  (render-texture paint
                  ((:visible-texture paint) paint))
  paint)

(defn render-to-texture [paint]
  (frame-buffer-object/bind (:frame-buffer-object paint))
  (frame-buffer-object/bind-texture (:id ((:visible-texture paint) paint)))

  (GL11/glPushAttrib GL11/GL_VIEWPORT_BIT)
  (GL11/glViewport 0 0 (width paint) (height paint))
  (GL11/glMatrixMode GL11/GL_PROJECTION)
  (GL11/glPushMatrix)
  (GL11/glLoadIdentity)
  (GL11/glOrtho 0 (width paint) 0 (height paint) -1 1)



  (render-texture paint
                  ((non-visible-texture paint) paint))

  (GL11/glMatrixMode GL11/GL_PROJECTION)
  (GL11/glPopMatrix)
  (GL11/glPopAttrib)

  (frame-buffer-object/bind 0)
  paint)

(defn update [paint]
  (println "mousex " )
  (-> paint
      (render)
      ;;      (swap-visible-texture)
      (render-to-texture)
      (update-window)))

(let [window (window/create 800 500)]
  (try
    (let [initial-paint (-> (create-paint window)
                            (load-image)
                            (add-quad)
                            (load)
                            (render-to-texture))]
      (loop [paint initial-paint]
        (if (not @(:close-requested (:window paint)))
          (recur (update paint))
          (window/close window))))

    (catch Exception e
      (println e)
      (.printStackTrace e)
      (window/close window))))



