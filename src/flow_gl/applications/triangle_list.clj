(ns flow-gl.applications.triangle-list
  (:require (flow-gl.gui [application :as application])
            (flow-gl.opengl [triangle-list :as triangle-list]))
  (:import [org.lwjgl.opengl GL11]))

(defn update [state-atom]
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
  (GL11/glMatrixMode GL11/GL_MODELVIEW)
  (GL11/glPushMatrix)
  (let [interval (* 1e9 4)
        x (-> (System/nanoTime)
              (mod interval)
              (/ interval)
              (* 2 Math/PI)
              (Math/sin)
              (* 50)
              (+ 150))]
    (GL11/glTranslatef x 0 0))

  (triangle-list/render (:triangle-list @state-atom))
  (GL11/glPopMatrix))

(defn initialize [state state-atom]

  (assoc state
    :triangle-list (triangle-list/create-for-coordinates :triangles
                                                         [10 10
                                                          200 200
                                                          10 200]
                                                         [1 0 0 1
                                                          0 1 0 1
                                                          0 0 1 1])))

(defn close [state-atom]
  (triangle-list/delete (:triangle-list @state-atom)))

(defn run []
  (application/start-viewless #(update %)
                              :close close
                              :initialize initialize
                              :framerate 200))

(comment
  (.start (Thread. run))
  )