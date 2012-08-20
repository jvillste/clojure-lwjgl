(ns clojure-lwjgl.command.pop-modelview
  (:require [clojure-lwjgl.command.command :as command])
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
