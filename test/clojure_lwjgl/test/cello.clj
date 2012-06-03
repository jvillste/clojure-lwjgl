(ns clojure-lwjgl.test.cello.clj
  (:require (clojure-lwjgl [triangle-list :as triangle-list]
                           [window :as window])
            [clojure-cello.pitch-detector :as pitch-detector])
  (:import [org.lwjgl.opengl GL11 GL20 ARBVertexBufferObject ARBVertexProgram ARBVertexShader]))



(defn start-pitch-detector [pitch-atom]
  (println "starting")
  (pitch-detector/create (fn [pitch probability time-stamp progress]
                           (reset! pitch-atom pitch))))

(defn stop-pitch-detector [application]
  (println "stopping")
  (pitch-detector/stop (application :pitch-detector))
  application)

(defn render [application]
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
  (GL11/glMatrixMode GL11/GL_MODELVIEW)
  (GL11/glLoadIdentity)

  (triangle-list/render (application :triangle-list))
  application)



(defn update-pitch-indicator [application]
  (if (> @(:pitch-atom application)
         0)
    (let [y (* (/ @(:pitch-atom application)
                  600)
               @(:height (:window application)))]
      (println "Y: " y)
      (assoc application
        :triangle-list (triangle-list/update (application :triangle-list)
                                             0
                                             {:coordinates (map float [0.0 (+ y 50.0)
                                                                       100.0 y
                                                                       0.0 (- y 50.0)])
                                              :colors (map float [0.0 0.0 1.0 1.0
                                                                  0.0 0.0 1.0 1.0
                                                                  0.0 0.0 1.0 1.0])})))
    application))

(defn update [application]
  (-> application
      (update-pitch-indicator)
      (render)))

(defn create-application [window]
  (let [pitch-atom (atom -1)]
    {:window window
     :pitch-atom pitch-atom
     :pitch-detector (start-pitch-detector pitch-atom)
     :triangle-list (-> (triangle-list/create)
                        (triangle-list/update 0 {:coordinates (map float [0.0 0.0
                                                                          100.0 0.0
                                                                          100.0 100.0])
                                                 :colors (map float [0.0 0.0 1.0 1.0
                                                                     0.0 0.0 1.0 1.0
                                                                     0.0 0.0 1.0 1.0])}))}))

(comment
(window/start 700 500
                30
                create-application
                update
                stop-pitch-detector))