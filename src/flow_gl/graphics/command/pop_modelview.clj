(ns flow-gl.graphics.command.pop-modelview
  (:require [flow-gl.graphics.command :as command])
  (:import [org.lwjgl.opengl GL11]))

(defrecord PopModelview [])

(extend PopModelview
  command/Command
  {:create-runner identity}
  command/CommandRunner
  {:delete identity
   :run (fn [_]
          (GL11/glMatrixMode GL11/GL_MODELVIEW)
          (GL11/glPopMatrix))})
