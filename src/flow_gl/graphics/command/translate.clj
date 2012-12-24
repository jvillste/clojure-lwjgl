(ns flow-gl.graphics.command.translate
  (:require [flow-gl.graphics.command :as command]
            (flow-gl.graphics.command 
                                   [push-modelview :as push-modelview]
                                   [pop-modelview :as pop-modelview]))
  (:import [org.lwjgl.opengl GL11]))

(defrecord Translate [x y])

(defn combine [translate1 translate2]
  (->Translate (+ (:x translate1)
                  (:x translate2))
               (+ (:y translate1)
                  (:y translate2))))

(extend Translate
  command/Command
  {:create-runner identity}
  command/CombinableCommand
  {:combine combine}
  command/CommandRunner
  {:delete identity
   :run (fn [{:keys [x y]}]
          (GL11/glMatrixMode GL11/GL_MODELVIEW)
          (GL11/glTranslatef x y 0))})

(defn translate [x y & commands]
  (concat [(push-modelview/->PushModelview)
           (->Translate x y)]
          commands
          [(pop-modelview/->PopModelview)]))
