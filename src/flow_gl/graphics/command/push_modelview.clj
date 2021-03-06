(ns flow-gl.graphics.command.push-modelview
  (:require [flow-gl.graphics.command :as command])
  (:import [org.lwjgl.opengl GL11]))

(defrecord PushModelview [])

(extend PushModelview
  command/Command
  {:create-runner identity}
  command/CommandRunner
  {:delete identity
   :run (fn [_]
          (GL11/glMatrixMode GL11/GL_MODELVIEW)
          (GL11/glPushMatrix))})
