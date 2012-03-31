(ns clojure-lwjgl.window
  (:import [org.lwjgl.opengl GLContext PixelFormat Display DisplayMode GL11 GL12  GL30 ARBVertexBufferObject]
           [org.lwjgl.input Mouse]
           [org.lwjgl BufferUtils]
           [java.awt Frame Canvas]
           [java.awt.event WindowAdapter ComponentAdapter]
           [java.nio IntBuffer FloatBuffer]
           [jpen.owner.multiAwt AwtPenToolkit]
           [jpen.event PenAdapter])
  (:require [clojure-lwjgl.event-queue :as event-queue]))

(defrecord Window [frame
                   close-requested
                   resize-requested
                   width
                   height])

(defn resize [window]
  (if @(:resize-requested window)
    (do
      (println "resize")
      (GL11/glViewport 0 0 @(:width window)  @(:height window))
      (GL11/glMatrixMode GL11/GL_PROJECTION)
      (GL11/glLoadIdentity)
      (GL11/glOrtho 0, @(:width window), 0, @(:height window), -1, 1)
      (GL11/glMatrixMode GL11/GL_MODELVIEW)
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

    (AwtPenToolkit/addPenListener frame
                                  (proxy [PenAdapter] []
                                    (penLevelEvent [e]
                                      (println "pen level event " e))))

    (Display/setParent canvas)
    (Display/create)
    (initialize-gl)
    (map->Window {:frame frame
                  :close-requested close-requested
                  :resize-requested resize-requested
                  :width width
                  :height height})))

(defn close [window]
  (println "Destroying window")
  (Display/destroy)
  (.dispose (:frame window))
  (reset! (:close-requested window) false))

(defn initialize [gui]
  (-> gui
      (assoc ::window (create))
      (event-queue/add-event-handler :update update)))

(defn start [width height framerate initialize update-state]
  (let [window (create width height)]
    (try
      (loop [state (initialize)]
        (if (not @(:close-requested window))
          (do (update window framerate)
              (recur (update-state state)))
          (close window)))
      (catch Exception e
        (println e)
        (.printStackTrace e)
        (close window)))))
