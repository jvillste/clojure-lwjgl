(ns flow-gl.graphics.command.scale
  (:require [flow-gl.graphics.command :as command])
  (:import [org.lwjgl.opengl GL11]))

(defrecord Scale [ratio])

(defn combine [scale1 scale2]
  (->Scale (* (:ratio scale1)
              (:ratio scale2))))

(extend Scale
  command/Command
  {:create-runner identity}
  command/CombinableCommand
  {:combine combine}
  command/CommandRunner
  {:delete identity
   :run (fn [{:keys [ratio]}]
          (GL11/glMatrixMode GL11/GL_MODELVIEW)
          (GL11/glScalef ratio ratio 1))})
