(ns clojure-lwjgl.window
  (:import [org.lwjgl.opengl GLContext PixelFormat Display DisplayMode GL11 GL12  GL30 ARBVertexBufferObject]
           [org.lwjgl.input Mouse]
           [org.lwjgl BufferUtils]
           [java.awt Frame Canvas]
           [java.awt.event WindowAdapter ComponentAdapter]
           [java.nio IntBuffer FloatBuffer])
  (:require [clojure-lwjgl.event-queue :as event-queue]))

(defrecord Window [frame
                   close-requested
                   resize-requested
                   width
                   height])

(defn resize [window]
  (if @(:resize-requested window)
    (do
      (GL11/glViewport 0 0 @(:width window)  @(:height window))
      (GL11/glMatrixMode GL11/GL_PROJECTION)
      (GL11/glLoadIdentity)
      (GL11/glOrtho 0, @(:width window), 0, @(:height window), -1, 1)

      (GL11/glMatrixMode GL11/GL_MODELVIEW)
      (GL11/glLoadIdentity)
      (GL11/glScalef 1 -1 1)
      (GL11/glTranslatef 0 (- @(:height window)) 0)
      (reset! (:resize-requested window) false)
      window)
    window))

(defn update [window framerate]
  (let [new-window (resize window)]
    (Display/update)
    (Display/sync framerate)
    new-window))

(defn handle-update-event [gui event]
  (assoc gui
    ::window (update (::window gui))))

(defn initialize-gl []
  (GL11/glClearColor 0 0 0 0)
  (GL11/glEnable GL11/GL_BLEND)
  (GL11/glEnable GL11/GL_TEXTURE_2D)
  (GL11/glColorMask true, true, true, true)
  (GL11/glBlendFunc GL11/GL_SRC_ALPHA GL11/GL_ONE_MINUS_SRC_ALPHA))

(defn create [initial-width initial-height]
  (let [canvas (Canvas.)
        frame (new Frame)
        resize-requested (atom true)
        close-requested (atom false)
        width (atom 0)
        height (atom 0)]

    (.addComponentListener canvas
                           (proxy [ComponentAdapter] []
                             (componentResized [e]
                               (println "Resizing")
                               (reset! width (-> canvas .getSize .getWidth))
                               (reset! height (-> canvas .getSize .getHeight))
                               (reset! resize-requested true))))

    (doto frame
      (.add canvas)
      (.addWindowListener
       (proxy [WindowAdapter] []
         (windowClosing [e]
           (println "Frame closed")
           (reset! close-requested true))))
      (.setSize initial-width initial-height)
      .show)

    (.requestFocus canvas)

    (Display/setParent canvas)
    (Display/create)
    (initialize-gl)
    (map->Window {:frame frame
                  :close-requested close-requested
                  :resize-requested resize-requested
                  :width width
                  :height height})))

(defn request-close [window]
  (reset! (:close-requested window) true))

(defn close [window]
  (println "Destroying window")
  (Display/destroy)
  (.dispose (:frame window))
  (reset! (:close-requested window) false))

(defn start [width height framerate initialize update-state handle-close handle-resize]
  (let [window (-> (create width height)
                   (update framerate))]
    (try
      (loop [state (initialize window)]
        (if (not @(:close-requested window))
          (do (let [resize-requested @(:resize-requested window)]
                (update window framerate)
                (if resize-requested
                  (recur (-> state
                             (handle-resize @(:width window) @(:height window))
                             (update-state)))
                  (recur (update-state state)))))
          (do (handle-close state)
              (close window))))
      (catch Exception e
        (println "Exception in window loop: " e)
        (.printStackTrace e)
        (close window)))))
