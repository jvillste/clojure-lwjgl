(ns clojure-lwjgl.test.graphics
  (:require (clojure-lwjgl [image :as image]
                           [image-list :as image-list]
                           [texture :as texture]
                           [buffered-image :as buffered-image]
                           [window :as window]
                           [font :as font]
                           [text-list :as text-list]
                           [triangle-batch :as triangle-batch]
                           [vector-rectangle :as vector-rectangle]
                           [primitive-list :as primitive-list]
                           [triangle-list :as triangle-list]
                           [primitive :as primitive]))
  (:import [org.lwjgl.opengl GL11 GL20 ARBVertexBufferObject ARBVertexProgram ARBVertexShader]
           [java.awt Color Font FontMetrics RenderingHints]
           [clojure_lwjgl.triangle_batch TriangleBatch]
           [clojure_lwjgl.triangle_list TriangleList]
           [clojure_lwjgl.text_list Text TextList]
           [clojure_lwjgl.translate Translate]))

(defprotocol Command
  (create-runner [command]))

(defprotocol CombinableCommand
  (combine [command other-command]))

(defprotocol CommandRunner
  (delete [command-runner])
  (run [command-runner]))

(defrecord CommandRunnerBatch [command-runners]
  CommandRunner
  (delete [command-runner-batch] (update-in command-runner-batch [:command-runners] #(doall (map delete %))))
  (run [command-runner-batch] (update-in command-runner-batch [:command-runners] #(doall (map run %)))))

(defn draw-view-part [application view-part]
  (doseq [command-runner (get-in application [:view-part-command-runners view-part])]
    (println "running " (type command-runner))
    (run command-runner)))

(defn render [application]
  (println "render")
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)

  (doseq [view-part (:view application)]
    (draw-view-part application view-part))

  application)

(extend Translate
  Command
  {:create-runner identity}
  CommandRunner
  {:delete identity
   :run (fn [{:keys [x y]}]
          (GL11/glMatrixMode GL11/GL_MODELVIEW)
          (GL11/glTranslatef x y 0))})

(extend TriangleBatch
  Command
  {:create-runner triangle-list/create-from-batch}
  CombinableCommand
  {:combine triangle-batch/concatenate})

(extend TriangleList
  CommandRunner
  {:delete triangle-list/delete
   :run triangle-list/render})

(extend Text
  Command
  {:create-runner (fn [text] (text-list/create [text]))})

(extend TextList
  CommandRunner
  {:delete text-list/delete
   :run text-list/draw})

(defn command-runners-for-commands [commands]
  (flatten (map (fn [command-group]
                  (if (satisfies? CombinableCommand (first command-group))
                    (-> (reduce combine command-group)
                        (create-runner))
                    (map create-runner command-group)))
                (partition-by type commands))))

(defn update-view-part [application view-part]
  (doseq [command-runner (get-in application [:view-part-command-runners view-part])]
    (delete command-runner))

  (assoc-in application [:view-part-command-runners view-part] (command-runners-for-commands ((get-in application [:view-parts view-part]) application))))

(defn update-view [application]
  (reduce update-view-part application (:view application)))

(defn background [{:keys [width height]}]
  [(vector-rectangle/rectangle 20 20
                               (- width 40)
                               (- height 40)
                               [0.5 0.5 0.5 1])
   (text-list/->Text (/ width 2)
                     (- (/ height 2)
                        100)
                     "JEES"
                     (font/create "LiberationSans-Regular.ttf" 40)
                     [0.0 0.0 0.0 1.0])
   (vector-rectangle/rectangle 30 30
                               (- width 60)
                               (- height 60)
                               [1 1 1 0.5])])

(defn foreground [{:keys [count width height]}]
  [(text-list/->Text (/ width 2)
                     (/ height 2)
                     (str count)
                     (font/create "LiberationSans-Regular.ttf" 40)
                     [0.0 0.0 0.0 1.0])])

(defn update [application]
  (-> application
      (update-in [:count] inc)
      (update-view-part :foreground)
      (render)))

(defn create-application [window]
  (-> {:view-parts {:background background
                    :foreground foreground}
       :view [:background
              :foreground]
       :view-part-command-runners {}
       :width @(:width window)
       :height @(:height window)
       :count 0}
      (update-view)))

(comment
(window/start 700 500
                5
                create-application
                update
                identity
                (fn [state width height]
                  (-> state
                      (assoc
                          :width width
                          :height height)
                      (update-view)))))