(ns flow-gl.applications.triangle-list
  (:require (flow-gl.gui [application :as application])
            (flow-gl.opengl [triangle-list :as triangle-list])))

(defn update [state-atom]
  (triangle-list/render (:triangle-list @state-atom)))

(defn initialize [state state-atom]

  (assoc state
    :triangle-list (triangle-list/create-for-coordinates :triangles
                                            (map float [10 10
                                                        200 200
                                                        10 200])
                                            (map float [1 0 0 1
                                                        0 1 0 1
                                                        0 0 1 1]))))

(defn close [state-atom]
  (triangle-list/delete (:triangle-list @state-atom)))

(defn run []
  (application/start-viewless update
                              :close close
                              :initialize initialize
                              :framerate 200))

(comment
  (.start (Thread. run))
  )