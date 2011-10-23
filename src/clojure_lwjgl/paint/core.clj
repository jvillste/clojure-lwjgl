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

(def vertex-shader-2-source "
void main() {
  gl_TexCoord[0] = gl_MultiTexCoord0;
  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
}
")

(def fragment-shader-2-source "
uniform sampler2D texture;
uniform float mouseX;
uniform float mouseY;

void main() {
        float x = gl_TexCoord[0].s;
        float y = gl_TexCoord[0].t;

        float dx = x - mouseX;
        float dy = y - mouseY;
        float alpha = sqrt(dx*dx + dy*dy) + 0.9;
        gl_FragColor = texture2D(texture,vec2(x,y)) *  vec4(1.0, 1.0, 1.0, alpha);
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
                                           fragment-shader-2-source)
   :mouse-state (input/create-initial-mouse-state)})

(defn non-visible-texture [paint]
  (if (= (:visible-texture paint)
         :texture-1)
    :texture-2
    :texture-1))

(defn swap-visible-texture [paint]
  (assoc paint
    :visible-texture (non-visible-texture paint)))

(defn load-image [paint]
  (let [image (buffered-image/create-from-file "grass.jpg")]
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
                                      30)))

(defn render-texture [paint texture]
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
  (GL11/glMatrixMode GL11/GL_MODELVIEW)
  (GL11/glLoadIdentity)

  (texture/bind texture)
  (draw/draw-quads (:vertex-buffer-id (:quad-buffer paint))
                   (:buffer-id (:texture-coordinate-buffer paint))
                   (:index-buffer-id (:quad-list paint))
                   1)

  )

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

(defn enable-brush-program [paint x y]

  (shader/enable-program (:shader-program paint))

  (shader/set-float-uniform (:shader-program paint)
                            "mouseX"
                            (float (/ x
                                      @(:width (:window paint)))))

  (shader/set-float-uniform (:shader-program paint)
                            "mouseY"
                            (float (/ y
                                      @(:height (:window paint)))))
  paint)


(defn disable-brush-program [paint]
  (shader/disable-program)
  paint)

(defn blit-brush [paint x y]
  (-> paint
      (swap-visible-texture)
      (enable-brush-program x y)
      (render-to-texture)
      (disable-brush-program)))

(defn mouse-event-to-coordinates [mouse-event]
  {:x (:mouse-x mouse-event)
   :y (:mouse-y mouse-event)})


(defn interpolate [maximum-delta previous-coordinate next-coordinate]
  (let [dx (- (:x next-coordinate)
              (:x previous-coordinate))
        dy (- (:y next-coordinate)
              (:y previous-coordinate))

        total-delta (Math/sqrt (+ (* dx dx)
                                  (* dy dy)))
        number-of-moves (/ total-delta
                           maximum-delta)]
    (if (> dx
           dy)
      (let [k (/ dx
                 dy)
            dx-per-move (/ dx
                           number-of-moves)]
        (map (fn [x] {:x x
                      :y (* k x)})
             (range (:x previous-coordinate)
                    (:x next-coordinate)
                    dx-per-move)))
      (let [k (/ dy
                 dx)
            dy-per-move (/ dy
                           number-of-moves)]
        (map (fn [y] {:x (* k y)
                      :y y})
             (range (:y previous-coordinate)
                    (:y next-coordinate)
                    dy-per-move))))))

(defn stroke-coordinates []
  (concat interpolate)
  (map mouse-event-to-coordinates
       (filter (fn [mouse-event] (= (:type mouse-event)
                                    :mouse-moved))
               (input/unread-mouse-events))))

(defn draw-stroke [paint]
  (reduce (fn [paint mouse-event]
            (blit-brush paint
                        (:x mouse-event)
                        (:y mouse-event)))
          paint
          (stroke-coordinates)))

(defn update [paint]
  (-> paint
      (render)
      (draw-stroke)
      (update-window)))

(let [window (window/create 1580 695)]
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



